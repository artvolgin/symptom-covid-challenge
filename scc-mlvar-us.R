
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
library(lme4)
library(usmap)
# Extra
library(covidcast)
library(mlVAR)
library(graph4lg)
library(igraph)
library(scales)
library(pheatmap)
library(RColorBrewer)


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
                                           "hospital_admissions_prop", "doctor_visits_prop",
                                           "age_bucket"), selected_vars))
# Long to wide format
df_state_wide <- pivot_wider(df_state,
                             id_cols = c("date", "state_code", "cluster", "cases_prop",
                                         "deaths_prop", "doctor_visits_prop",
                                         "hospital_admissions_prop", "StringencyIndex"),
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

### --- MODEL 2. CLI by age groups, Deaths and Hospital admissions
variables.2 <- c("pct_cmnty_cli_18_34", "pct_cmnty_cli_55",
                 "deaths_prop", "hospital_admissions_prop",
                 "StringencyIndex")
mlvar.2 <- mlVAR(df_state_wide,
                 vars = variables.2,
                 idvar = "state_code", lags = 14,
                 temporal = "correlated", nCores = 12, scale=T,
                 scaleWithin=T)

### --- Extract coefficents from models with different lags 
lags_coef <- data.frame()
for (i in 1:21){
  
  mlvar.temp <- mlVAR(df_state_wide,
                      vars = variables.1,
                      idvar = "state_code", lags = i,
                      temporal = "correlated", nCores = 12, scale=T)
  
  temp_coef <- data.frame(mlvar.temp$results$Beta$mean)
  colnames(temp_coef)<- substr(colnames(temp_coef), 1, regexpr("\\.", colnames(temp_coef))-1)
  lags_coef <- rbind(lags_coef, temp_coef[1,])

  print(i)
  
}

lags_coef <- lags_coef %>% t()
colnames(lags_coef) <- as.character(1:21)
rownames(lags_coef) <- c("CLI in Community", "Avoiding Contact", "Worked Outside",
                         "Testing Shortage", "Stringency Index")
saveRDS(lags_coef, "lags_coef")
lags_coef <- readRDS("lags_coef")


# --- Compute confidence intervals for random effects
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
     border.color="gray45",
     color=c("gainsboro","white", "white", "white", "white", "white"),
     shape="circle",
     asize=5,  edge.labels=T, edge.label.cex=0.8)


### --- PLOT 1. OVERALL NETWORK
mlvar.1$input$vars <- c("CLI in\nCommunity", "Avoiding\nContact", "Worked\nOutside",
                       "Testing\nShortage", "Stringency\nIndex")
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
     border.color="gray45",
     color=c("white", "white", "white", "white", "gainsboro"),
     shape="circle",
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
     border.color="gray45",
     color=c("gainsboro","white", "white", "white", "gainsboro"),
     shape="circle",
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
     border.color="gray45",
     color=c("white","white", "white", "white", "gainsboro"),
     shape="circle",
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
     border.color="gray45",
     color=c("white","white", "white", "white", "gainsboro"),
     shape="circle",
     asize=5,  edge.labels=T, edge.label.cex=0.8)

### --- PLOT 4. CLI by age groups, Deaths and Hospitalizations
mlvar.2$input$vars <- c("CLI in\nCommunity\n18-34", "CLI in\nCommunity\n55+",
                        "New\nDeaths", "Hospital\nAdmissions", "Stringency\nIndex")
plot(mlvar.2,
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
     border.color="gray45",
     color=c("white","white", "gainsboro", "gainsboro", "gainsboro"),
     shape="circle",
     asize=5,  edge.labels=T, edge.label.cex=0.8)


### --- PLOT 5. The Effects of Avoiding Contact on CLI by States

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
  left_join(df_state_agg %>% dplyr::select(cluster, state_code, state_name)) %>%
  mutate(cluster=as.character(cluster))

# Add SE of coefficents to the dataset
res_full$state_code <- rownames(res_full)
df_plot <- df_plot %>% left_join(res_full %>%
                        dplyr::select(state_code, V2_se, V5_se) %>%
                        rename(pct_avoid_contact_overall.impact.se=V5_se, StringencyIndex.impact.se=V2_se))

df_plot <- df_plot %>% arrange(pct_avoid_contact_overall.impact)
ggplot(df_plot, aes(x = reorder(state_name, -pct_avoid_contact_overall.impact),
                    y = pct_avoid_contact_overall.impact,
                    color=cluster)) + 
  geom_errorbar(aes(ymin = pct_avoid_contact_overall.impact-(pct_avoid_contact_overall.impact.se*1.96),
                    ymax = pct_avoid_contact_overall.impact+(pct_avoid_contact_overall.impact.se*1.96)),
                width=0.5) + 
  scale_color_manual(labels = c("Late Peak", "Early Peak"), values=c("cornflowerblue", "brown2")) +
  geom_point(data=df_plot, mapping=aes(x=state_name, y=pct_avoid_contact_overall.impact,
                                       color=cluster), size=2) +
  labs(x = "",
       y = "Avoiding Contact  →  CLI in Community") + 
  geom_hline(aes(yintercept=0), 
             linetype="dashed", color = "gray40") +
  coord_flip() + theme_minimal() +
  theme(axis.title=element_text(size=24),
        legend.text=element_text(size=16),
        # legend.position = c(0.9, 0.1),
        legend.title=element_text(size=16),
        text = element_text(size=20),
        axis.text.y = element_text(face = ifelse(df_plot$state_name %in% c("Texas", "Minnesota"),
                                                 "bold", "plain"),
                                   size =ifelse(df_plot$state_name %in% c("Texas", "Minnesota"),
                                                16, 12)))

### --- PLOT 6. Heatmap with different lags
pheatmap(lags_coef, cluster_rows=F, cluster_cols=F, angle_col = "0",
         color = colorRampPalette(c("brown3", "white", "forestgreen"))(100),
         border_color="white",
         display_numbers=T, legend=F, fontsize = 14,
         cellwidth=35, cellheight=50)

