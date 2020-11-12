
#####################################################################################################
################### MLVAR: MODEL ESTIMATION AND VISUALIZATION
#####################################################################################################

### Load the packages
# Basic
library(ggplot2)
library(ggrepel)
library(ggfortify)
library(reshape2)
library(stringr)
library(rio)
library(gridExtra)
library(tidyr)
library(dplyr)
library(usmap)
# Extra
library(covidcast)
library(mlVAR)
library(graph4lg)
library(igraph)
library(scales)

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))


### -------------------------- PREPROCESSING

# Load the data
df_state <- readRDS("df_state.rds")

# Create variable that indicates shortage of test
df_state <- df_state %>% mutate(pct_testing_shortage=pct_could_not_get_tested)

# Date
df_state$date <- as.Date(df_state$date)
df_state <- df_state %>% filter(gender=="overall")
colnames(df_state) <- str_replace_all(colnames(df_state), " ", "_")

# Create dataset with state code and name
df_state_names <- data.frame(state_code=unique(df_state$state_code))
df_state_names$state_code_up <- toupper(df_state_names$state_code)
df_state_names <- df_state_names %>%
  left_join(data.frame(state_code_up=state.abb, state_name=state.name))
# Create dataset with aggregated time-invariant statistics by state
df_state_agg <- df_state %>%
  filter(gender=="overall", age_bucket=="overall") %>% 
  group_by(state_code) %>%
  summarise(total_n=sum(summed_n),
            mean_n=mean(summed_n))
df_state_agg$state_code_up <- toupper(df_state_agg$state_code)
df_state_agg <- df_state_agg %>%
  left_join(df_state_names %>% dplyr::select(state_code_up, state_name)) %>%
  dplyr::select(-state_code_up)
# Add name to District Columbia
df_state_agg[is.na(df_state_agg$state_name),]$state_name <- "District of Columbia"

# Add state names to the df_state
df_state <- df_state %>% left_join(df_state_agg %>% dplyr::select(state_code, state_name))

# Add additional time-invariant aggregates for each state
df_state_agg <- df_state_agg %>% left_join(df_state %>%
                                             filter(gender=="overall", age_bucket=="overall") %>%
                                             group_by(state_code) %>%
                                             summarize(mean_pct_cmnty_cli=mean(pct_cmnty_cli),
                                                       mean_pct_avoid_contact=mean(pct_avoid_contact),
                                                       mean_stringency_index=mean(StringencyIndex),
                                                       cluster=max(cluster)))

# Transform to wide format
selected_vars <- c("pct_cmnty_cli", "pct_worked_outside_home", "pct_avoid_contact", "pct_cli_anosmia_ageusia",
                   "pct_self_fever", "pct_self_cough", "pct_self_shortness_of_breath",
                   "pct_self_difficulty_breathing", "pct_self_anosmia_ageusia",
                   "pct_self_sore_throat", "pct_self_persistent_pain_pressure_in_chest",
                   "pct_self_nausea_vomiting", "pct_self_diarrhea",
                   "pct_testing_shortage", "StringencyIndex")
df_state <- df_state %>% dplyr::select(c(c("state_code", "date", "cluster", "cases_prop", "deaths_prop",
                                           "age_bucket"), selected_vars))
# Long to wide format
df_state_wide <- pivot_wider(df_state,
                             id_cols = c("date", "state_code", "cluster", "cases_prop",
                                         "deaths_prop", "StringencyIndex"),
                             names_from = age_bucket,
                             values_from = colnames(df_state)[startsWith(colnames(df_state), "pct_")])
colnames(df_state_wide) <- str_replace(colnames(df_state_wide), "-", "_")
colnames(df_state_wide) <- str_remove(colnames(df_state_wide), "\\+")
selected_vars <- colnames(df_state_wide)[4:ncol(df_state_wide)]

# Detrending
list_states <- list()
i <- 1
for (st in unique(df_state_wide$state_code)){
  
  df_sub <- df_state_wide %>% filter(state_code==st)
  
  for (var_name in selected_vars){
    
    df_sub[var_name] <- residuals(lm(as.formula(paste(var_name, " ~ date")), data = df_sub, na.action = na.exclude))
    
  }
  list_states[[i]] <- df_sub
  i <- i + 1
  
}
df_state_wide <- do.call(rbind, list_states)

# Arrange for modelling
df_state_wide <- df_state_wide %>% arrange(state_code, date)

### -------------------------- MODELLING
### --- MODEL 0. Symptoms + Number of cases
variables.0 <- c("cases_prop",
                 "pct_self_fever_overall",
                 "pct_self_cough_overall",
                 "pct_self_shortness_of_breath_overall",
                 "pct_self_difficulty_breathing_overall",
                 "pct_self_anosmia_ageusia_overall")
mlvar.0 <- mlVAR(df_state_wide,
                   vars = variables.0,
                   idvar = "state_code", lags = 14,
                   temporal = "correlated", nCores = 12, scale=T)

### --- MODEL 1. OVERALL
variables.1 <- c("pct_cmnty_cli_overall", 
                 "pct_avoid_contact_overall",
                 "pct_worked_outside_home_overall",
                 "pct_testing_shortage_overall",
                 "StringencyIndex")
mlvar.1 <- mlVAR(df_state_wide,
                  vars = variables.1,
                  idvar = "state_code", lags = 14,
                  temporal = "correlated", nCores = 12, scale=T)

### --- MODEL 1.C OVERALL, cases
variables.1.c <- c("cases_prop", 
                   "pct_avoid_contact_overall",
                   "pct_worked_outside_home_overall",
                   "pct_testing_shortage_overall",
                   "StringencyIndex")
mlvar.1.c <- mlVAR(df_state_wide,
                   vars = variables.1.c,
                   idvar = "state_code", lags = 14,
                   temporal = "correlated", nCores = 12, scale=T)

### --- MODEL 1.1 18-34 +
variables.1.1 <- c("pct_cmnty_cli_18_34",
                   "pct_avoid_contact_18_34",
                   "pct_worked_outside_home_18_34",
                   "pct_testing_shortage_18_34",
                   "StringencyIndex")
mlvar.1.1 <- mlVAR(df_state_wide,
                   vars = variables.1.1,
                   idvar = "state_code", lags = 14,
                   temporal = "correlated", nCores = 12, scale=T)

### --- MODEL 1.2 55+
variables.1.2 <- c("pct_cmnty_cli_55",
                   "pct_avoid_contact_55",
                   "pct_worked_outside_home_55",
                   "pct_testing_shortage_55",
                   "StringencyIndex")
mlvar.1.2 <- mlVAR(df_state_wide,
                   vars = variables.1.2,
                   idvar = "state_code", lags = 14,
                   temporal = "correlated", nCores = 12, scale=T)


# --- TESTING: Compute confidence intervals for random effects >>>>>>>>>>>>>>>>>>>>>>>
library(lme4)

lmer_model <- mlvar.1$output$temporal$pct_cmnty_cli_overall

cc <- coef(lmer_model)$state_code
cc <- cc[1:6]

## variances of fixed effects
fixed.vars <- diag(vcov(lmer_model))
fixed.vars <- fixed.vars[1:6]

## extract variances of conditional modes
r1 <- ranef(lmer_model, condVar=TRUE)

cmode.vars <- t(apply(cv <- attr(r1[[1]],"postVar"),3,diag))
seVals <- sqrt(sweep(cmode.vars,2,fixed.vars,"+"))
res <- cbind(cc,seVals)
# Drop intercept and SE for intercept
res <- res %>% dplyr::select(-c("(Intercept)", "1"))
# Calculate significance
res_mean <- res[1:(ncol(res)/2)]
colnames(res_mean) <- paste0("V", as.character(c(1:ncol(res_mean))), "_mean")
res_se <- res[((ncol(res)/2)+1):ncol(res)]
colnames(res_se) <- paste0("V", as.character(c(1:ncol(res_se))), "_se")
res_low <- res_mean - (1.96*res_se)
res_high <- res_mean + (1.96*res_se)
res_signif <- as.data.frame(sign(res_low) == sign(res_high))
colnames(res_signif) <- paste0("V", as.character(c(1:ncol(res_signif))), "_signif")
colSums(res_signif)
res_full <- cbind(res_mean, res_se, res_signif)

# --- TESTING: Compute confidence intervals for random effects >>>>>>>>>>>>>>>>>>>>>>>


### -------------------------- PLOTTING


### --- PLOT 0 SYMPTOMS
mlvar.0$input$vars <- c("New\nCases", "Fever", "Cough", "Shortness\nof Breath",
                        "Difficulty\nBreathing", "Anosmia\nAgeusia")
plot(mlvar.0,
     vsize=10,
     esize=5,
     label.prop=0.22,
     label.cex=6,
     usePCH=F,
     node.resolution=300,
     label.scale.equal=T,
     layoutScale=c(0.7,0.7),
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gray95","white", "white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle", "circle"),
     asize=5,  edge.labels=T, edge.label.cex=0.8)


### --- PLOT 1. OVERALL NETWORK
mlvar.1$input$vars <- c("CLI in\nCommunity", "Avoiding\nContact", "Worked\nOutside", "Testing\nShortage", "Stringency\nIndex")
plot(mlvar.1,
     vsize=10,
     esize=5,
     label.prop=0.22,
     label.cex=6,
     usePCH=F,
     node.resolution=300,
     label.scale.equal=T,
     layoutScale=c(0.7,0.7),
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gainsboro","white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle"),
     asize=5,  edge.labels=T, edge.label.cex=0.8)


### --- PLOT 1.C OVERALL NETWORK, Cases
mlvar.1.c$input$vars <- c("New\nCases", "Avoiding\nContact", "Worked\nOutside", "Testing\nShortage", "Stringency\nIndex")
plot(mlvar.1.c,
     vsize=10,
     esize=5,
     label.prop=0.22,
     label.cex=6,
     usePCH=F,
     node.resolution=300,
     label.scale.equal=T,
     layoutScale=c(0.7,0.7),
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gray95","white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle"),
     asize=5,  edge.labels=T, edge.label.cex=0.8)

### --- PLOT 2. 18-34 NETWORK
mlvar.1.1$input$vars <- c("CLI in\nCommunity", "Avoiding\nContact", "Worked\nOutside", "Testing\nShortage", "Stringency\nIndex")
plot(mlvar.1.1,
     vsize=10,
     esize=5,
     usePCH=F,
     node.resolution=300,
     label.prop=0.22,
     label.cex=6,
     label.scale.equal=T,
     layoutScale=c(0.7,0.7),
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gainsboro","white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle"),
     asize=5,  edge.labels=T, edge.label.cex=0.8)

### --- PLOT 3. 55+ NETWORK
mlvar.1.2$input$vars <- c("CLI in\nCommunity", "Avoiding\nContact", "Worked\nOutside", "Testing\nShortage", "Stringency\nIndex")
plot(mlvar.1.2,
     vsize=10,
     esize=5,
     usePCH=F,
     node.resolution=300,
     label.prop=0.22,
     label.cex=6,
     label.scale.equal=T,
     layoutScale=c(0.7,0.7),
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gainsboro","white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle"),
     asize=5,  edge.labels=T, edge.label.cex=0.8)

### --- PLOT 5. The Effects of the Stringency Index and Avoiding Contact on CLI & Time-Clusters

# Obtain coeficents for each state
coef_by_states <- list()
for (i in 1:length(mlvar.1$IDs)){
  temp <- melt(getNet(mlvar.1, subject=i))
  temp$state <- mlvar.1$IDs[i]
  coef_by_states[[i]] <- temp
}
coef_by_states <- do.call(rbind, coef_by_states)

# Preprocessing
df_plot <- data.frame(
  StringencyIndex.impact = (coef_by_states %>% filter(Var1=="Stringency\nIndex",
                                                      Var2=="CLI in\nCommunity"))$value,
  pct_avoid_contact_overall.impact=(coef_by_states %>% filter(Var1=="Avoiding\nContact",
                                                              Var2=="CLI in\nCommunity"))$value,
  state_code=unique(coef_by_states$state)
)
df_plot <- df_plot %>%
  left_join(df_state_agg %>% dplyr::select(cluster, state_code)) %>%
  mutate(cluster=as.character(cluster))

# Plot
ggplot(df_plot, aes(x = StringencyIndex.impact, y = pct_avoid_contact_overall.impact, color=cluster)) +
  geom_point(size=5, alpha=0.75) +
  geom_text_repel(aes(label = toupper(state_code)), size = 5) + 
  labs(x = "Temporal Effect of Stringency Index on CLI",
       y = "Temporal Effect of Avoiding Contact on CLI") + 
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title=element_text(size=22,face="bold"))

# ADD number of cases
# Load the total number of cases per population
df_cases <- covidcast_signal("jhu-csse", "confirmed_cumulative_prop",
                             max(df_state$date),
                             max(df_state$date),
                             geo_type = "state")
df_cases <- df_cases %>%
  data.frame() %>%
  rename("total_cases_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  dplyr::select("state_code", "total_cases_prop")

### --- PLOT 6. The Effects of the Stringency Index and Avoiding Contact on CLI & Politics
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))
df_governors <- read.csv("us-governors.csv")
df_governors <- df_governors %>%
  dplyr::select(state_code_slug, party) %>%
  rename(state_code=state_code_slug)
df_plot <- df_plot %>% left_join(df_governors)
df_plot$party[df_plot$state_code == "dc"] <- "democrat"
df_plot <- df_plot %>%
  rename(Governor=party, Cluster=cluster) %>%
  mutate(Governor=recode(Governor, republican="Republican", democrat="Democrat"))

# Add total cases
df_plot <- df_plot %>% left_join(df_cases)

# Plot CLUSTER 1
df_plot.1 <- df_plot %>% filter(Cluster==1)
ggplot(df_plot.1,
       aes(x = StringencyIndex.impact, y = pct_avoid_contact_overall.impact,
           color=Governor)) + scale_color_manual(values=c("#0015BC", "#E9141D")) +
  geom_point(size=7, alpha=0.75) + 
  geom_text_repel(aes(label = toupper(state_code)), size = 5, show.legend = FALSE) + 
  labs(x = "Stringency Index  →  CLI in Community",
       y = "Avoiding Contact  →  CLI in Community") + 
  xlim(-1, 1) +
  ylim(-2.5, 2.5) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title=element_text(size=24),
        legend.text=element_text(size=16),
        legend.position = c(0.9, 0.1),
        legend.title=element_text(size=16),
        text = element_text(size=20))

# Plot CLUSTER 2
df_plot.2 <- df_plot %>% filter(Cluster==2)
ggplot(df_plot.2,
       aes(x = StringencyIndex.impact, y = pct_avoid_contact_overall.impact,
           color=Governor)) + scale_color_manual(values=c("#0015BC", "#E9141D")) +
  geom_point(size=7, alpha=0.75) + 
  geom_text_repel(aes(label = toupper(state_code)), size = 5, show.legend = FALSE) + 
  labs(x = "Stringency Index  →  CLI in Community",
       y = "Avoiding Contact  →  CLI in Community") + 
  xlim(-1, 1) +
  ylim(-2.5, 2.5) +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title=element_text(size=24),
        legend.text=element_text(size=16),
        legend.position = c(0.9, 0.1),
        legend.title=element_text(size=16),
        text = element_text(size=20))
# Means
means_by_groups <- df_plot %>% group_by(Governor, Cluster) %>%
  summarise(mean_StringencyIndex=mean(StringencyIndex.impact),
            mean_pct_avoid_contact=mean(pct_avoid_contact_overall.impact))
means_by_groups

# t-test for the diffence in means between democrats and republicans from the cluster №1
dem_c1 <- df_plot %>%
  filter(party=="democrat", cluster=="1") %>%
         dplyr::select(pct_avoid_contact_overall.impact)
rep_c1 <- df_plot %>%
  filter(party=="republican", cluster=="1") %>%
  dplyr::select(pct_avoid_contact_overall.impact)
t.test(dem_c1, rep_c1)

