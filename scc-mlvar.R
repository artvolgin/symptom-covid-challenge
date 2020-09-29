
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
                                                       mean_stringency_index=mean(StringencyIndex)))

# Transform to wide format
selected_vars <- c("pct_cmnty_cli", "pct_worked_outside_home", "pct_avoid_contact",
                   "pct_testing_shortage", "StringencyIndex")

df_state <- df_state %>% dplyr::select(c(c("state_code", "date", "age_bucket"), selected_vars))
  
# Long to wide format
df_state_wide <- pivot_wider(df_state,
                             id_cols = c("date", "state_code", "StringencyIndex"),
                             names_from = age_bucket,
                             values_from = colnames(df_state)[startsWith(colnames(df_state), "pct_")])
colnames(df_state_wide) <- str_replace(colnames(df_state_wide), "-", "_")
colnames(df_state_wide) <- str_remove(colnames(df_state_wide), "\\+")
selected_vars <- colnames(df_state_wide)[3:ncol(df_state_wide)]

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

### --- MODEL 1. OVERALL
variables.1 <- c("pct_cmnty_cli_overall",
                 "pct_avoid_contact_overall",
                 "pct_worked_outside_home_overall",
                 "pct_testing_shortage_overall",
                 "StringencyIndex")
mlvar.1 <- mlVAR(df_state_wide,
                vars = variables.1,
                idvar = "state_code", lags = 10,
                temporal = "correlated", nCores = 12, scale=T)

### --- MODEL 2. 18-34
variables.2 <- c(
  "pct_cmnty_cli_18_34",
  "pct_avoid_contact_18_34",
  "pct_worked_outside_home_18_34",
  "pct_testing_shortage_18_34",
  "StringencyIndex")
mlvar.2 <- mlVAR(df_state_wide,
                 vars = variables.2,
                 idvar = "state_code", lags = 10,
                 temporal = "correlated", nCores = 12, scale=T)

### --- MODEL 3. 55+
variables.3 <- c(
  "pct_cmnty_cli_55",
  "pct_avoid_contact_55",
  "pct_worked_outside_home_55",
  "pct_testing_shortage_55",
  "StringencyIndex")
mlvar.3 <- mlVAR(df_state_wide,
                 vars = variables.3,
                 idvar = "state_code", lags = 10,
                 temporal = "correlated", nCores = 12, scale=T)

# Obtain coeficents for each state
coef_by_states <- list()
for (i in 1:length(mlvar.1$IDs)){
  temp <- melt(getNet(mlvar.1, subject=i))
  temp$state <- mlvar.1$IDs[i]
  coef_by_states[[i]] <- temp
}
coef_by_states <- do.call(rbind, coef_by_states)


### -------------------------- PLOTTING

### --- PLOT 1.1 OVERALL NETWORK - TEMPORAL
mlvar.1$input$vars <- c("CLI", "Avoiding\nContact", "Worked\nOutside", "Testing\nShortage", "Stringency\nIndex")
plot(mlvar.1, vsize=7, esize=7, label.cex=c(2.7,3,3,3.3,3.5), label.scale.equal=F,
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gainsboro","white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle"),
     asize=5,  edge.labels=T, edge.label.cex=1.5)

### --- PLOT 1.2 OVERALL NETWORK - CONTEMPORANEOUS
plot(mlvar.1, vsize=7, esize=7, label.cex=c(2.7,3,3,3.3,3.5), label.scale.equal=F,
     type="contemporaneous", layout="circle", labels=T,
     border.width=3,
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gainsboro","white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle"),
     asize=5,  edge.labels=T, edge.label.cex=1.5)

### --- PLOT 2. 18-34 NETWORK
mlvar.2$input$vars <- c("CLI", "Avoiding\nContact", "Worked\nOutside", "Testing\nShortage", "Stringency\nIndex")
plot(mlvar.2, vsize=7, esize=7, label.cex=c(2.7,3,3,3.3,3.5), label.scale.equal=F,
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gainsboro","white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle"),
     asize=5,  edge.labels=T, edge.label.cex=1.5)

### --- PLOT 3. 55+ NETWORK
mlvar.3$input$vars <- c("CLI", "Avoiding\nContact", "Worked\nOutside", "Testing\nShortage", "Stringency\nIndex")
plot(mlvar.3, vsize=7, esize=7, label.cex=c(2.7,3,3,3.3,3.5), label.scale.equal=F,
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gainsboro","white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle"),
     asize=5,  edge.labels=T, edge.label.cex=1.5)

### --- PLOT 4. MAP OF EFFECTS: StringencyIndex ---> pct_cmnty_cli_overall

# Function for plotting coefficents on the map
plot.coef.map <- function(from_var, to_var, coef_by_states=coef_by_states){
  
  selected_coefs <- coef_by_states %>%
    filter(Var1==from_var, Var2==to_var)
  plot_usmap(data = selected_coefs, values = "value",
             color = "white", labels = T, label_color = "black") + 
    # scale_fill_continuous(type = "viridis", name = "coef", label = scales::comma) + 
    scale_fill_gradientn(colours=c("brown2","white","forestgreen"),
                         values=rescale(c(min(selected_coefs$value), 0, max(selected_coefs$value))),
                         name="Coefficient") + 
    theme(legend.position = "right",
          plot.title = element_text(hjust=0.5, size=18),
          plot.subtitle = element_text(hjust=0.5, size=14),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14)) + 
    labs(title = paste0(from_var, "  --->  ", to_var), subtitle = "")
  
}

plot.coef.map("StringencyIndex", "pct_cmnty_cli_overall", coef_by_states)

### --- PLOT 5. MAP OF EFFECTS: pct_avoid_contact_overall ---> pct_cmnty_cli_overall
plot.coef.map("pct_avoid_contact_overall", "pct_cmnty_cli_overall", coef_by_states)

### --- PLOT 6. MAP Mean Avoiding Contact
df_state_agg$state <- df_state_agg$state_code

plot_usmap(data = df_state_agg, values = "mean_pct_avoid_contact",
           color = "white", labels = T, label_color = "black") + 
  # scale_fill_continuous(type = "viridis", name = "coef", label = scales::comma) + 
  scale_fill_gradientn(colours=c("white","steelblue2"),
                       # values=rescale(c(min(selected_coefs$value), 0, max(selected_coefs$value))),
                       name="Pct") + 
  theme(legend.position = "right",
        plot.title = element_text(hjust=0.5, size=18),
        plot.subtitle = element_text(hjust=0.5, size=14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) + 
  labs(title = "mean_pct_avoid_contact", subtitle = "")

### --- PLOT 7. MAP Mean Stringency Index
plot_usmap(data = df_state_agg, values = "mean_stringency_index",
           color = "white", labels = T, label_color = "black") + 
  # scale_fill_continuous(type = "viridis", name = "coef", label = scales::comma) + 
  scale_fill_gradientn(colours=c("white","steelblue2"),
                       # values=rescale(c(min(selected_coefs$value), 0, max(selected_coefs$value))),
                       name="Index") + 
  theme(legend.position = "right",
        plot.title = element_text(hjust=0.5, size=18),
        plot.subtitle = element_text(hjust=0.5, size=14),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) + 
  labs(title = "mean_stringency_index", subtitle = "")


### --- PLOT 8 SCATTER PLOT: The Effects of the Stringency Index and Avoiding Contact on CLI

# Preprocessing
df_plot <- data.frame(
  StringencyIndex.impact = (coef_by_states %>% filter(Var1=="StringencyIndex",
                                                      Var2=="pct_cmnty_cli_overall"))$value,
  pct_avoid_contact_overall.impact=(coef_by_states %>% filter(Var1=="pct_avoid_contact_overall",
                                                              Var2=="pct_cmnty_cli_overall"))$value,
  state_code=unique(coef_by_states$state)
)
df_plot <- df_plot %>% left_join(df_state_agg %>% dplyr::select(total_cases_prop, state_code))
string_index <- df_plot$StringencyIndex.impact
avoid_contact <- df_plot$pct_avoid_contact_overall.impact
point_colors <- ifelse(string_index > 0 & avoid_contact > 0, "brown2",
                     ifelse(string_index < 0 & avoid_contact < 0, "forestgreen", "steelblue1"))
# Plot
ggplot(df_plot, aes(x = StringencyIndex.impact, y = pct_avoid_contact_overall.impact)) +
  geom_point(size=5, color=point_colors, alpha=0.75) +
  geom_text_repel(aes(label = toupper(state_code)), size = 5) + 
  geom_hline(yintercept = 0, color="grey", size=1, linetype="dashed") + 
  geom_vline(xintercept = 0, color="darkgrey", size=1, linetype="dashed") + 
  labs(x = "Temporal Effect of Stringency Index on CLI",
       y = "Temporal Effect of Avoiding Contact on CLI") + 
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title=element_text(size=22,face="bold"))

