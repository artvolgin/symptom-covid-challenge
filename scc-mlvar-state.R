
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
# Time-series
library(dtwclust)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
# Extra
library(covidcast)
library(mlVAR)
library(graph4lg)
library(igraph)


# Function for plotting coefficents on the map
plot.coef.map <- function(from_var, to_var, coef_by_states=coef_by_states){
  
  selected_coefs <- coef_by_states %>%
    filter(Var1==from_var, Var2==to_var)
  plot_usmap(data = selected_coefs, values = "value",
             color = "white", labels = T, label_color = "black") + 
    scale_fill_continuous(type = "viridis", name = "coef", label = scales::comma) + 
    theme(legend.position = "right",
          plot.title = element_text(hjust=0.5, size=18),
          plot.subtitle = element_text(hjust=0.5, size=14)) + 
    labs(title = paste0(from_var, "  --->  ", to_var), subtitle = "")
  
}


setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))

### -------------------------- PREPROCESSING

# Load the data
df_state <- readRDS("df_state.rds")

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

# Add state names to the the df_state
df_state <- df_state %>% left_join(df_state_agg %>% dplyr::select(state_code, state_name))

# Add additional time-invariant aggregates for each state
df_state_agg <- df_state_agg %>% left_join(df_state %>%
                                             filter(gender=="overall", age_bucket=="overall") %>%
                                             group_by(state_code) %>%
                                             summarize(mean_pct_cmnty_cli=mean(pct_cmnty_cli),
                                                       mean_pct_avoid_contact=mean(pct_avoid_contact),
                                                       total_cases=max(cases_cum_prop),
                                                       max_stringency_index=max(StringencyIndex)))
# ! Select only states with more than a 10,000 respondents each day
# df_state_agg <- df_state_agg %>% filter(mean_n > 5000)
# df_state <- df_state %>% filter(state_code %in% df_state_agg$state_code)

### Transform to wide format

selected_vars <- c("pct_cmnty_cli", "pct_worked_outside_home", "pct_avoid_contact", "StringencyIndex",
                   "C2_Workplace_closing",
                   "C3_Cancel_public_events",
                   "C4_Restrictions_on_gatherings",
                   "C5_Close_public_transport",
                   "C6_Stay_at_home_requirements")

df_state <- df_state %>% dplyr::select(c(c("state_code", "date", "age_bucket"), selected_vars))

# Long to wide format
df_state_wide <- pivot_wider(df_state,
                             id_cols = c("date", "state_code", "StringencyIndex", "C2_Workplace_closing",
                                         "C3_Cancel_public_events", "C4_Restrictions_on_gatherings",
                                         "C5_Close_public_transport", "C6_Stay_at_home_requirements"),
                             names_from = age_bucket,
                             values_from = c(pct_cmnty_cli, pct_worked_outside_home, pct_avoid_contact))
colnames(df_state_wide) <- str_replace(colnames(df_state_wide), "-", "_")
colnames(df_state_wide) <- str_remove(colnames(df_state_wide), "\\+")
selected_vars <- colnames(df_state_wide)[3:ncol(df_state_wide)]

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

# scaleWithin = TRUE or FALSE

### --- MODEL 1. 18-34 and StringencyIndex
variables.1 <- c("pct_cmnty_cli_18_34",
                 "pct_worked_outside_home_18_34",
                 "pct_avoid_contact_18_34",
                 "StringencyIndex")
mlvar.1 <- mlVAR(df_state_wide,
                vars = variables.1,
                idvar = "state_code", lags = 14,
                temporal = "correlated", nCores = 12)
plot(mlvar.1, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

for (i in 1:length(mlvar.1$IDs)){
  plot(mlvar.1, vsize=10, label.cex=3, label.scale.equal=T, type="temporal", layout="circle",
       subject=i, title=mlvar.1$IDs[i])
}

# Obtain the coeficents by states
coef_by_states <- list()
for (i in 1:length(mlvar.1$IDs)){
  temp <- melt(getNet(mlvar.1, subject=i))
  temp$state <- mlvar.1$IDs[i]
  coef_by_states[[i]] <- temp
}
coef_by_states <- do.call(rbind, coef_by_states)

# Plot 1. pct_cmnty_cli_18_34 ---> pct_avoid_contact_18_34
plot.coef.map("pct_cmnty_cli_18_34", "pct_avoid_contact_18_34", coef_by_states)

# Plot 2. StringencyIndex ---> pct_cmnty_cli_18_34
plot.coef.map("StringencyIndex", "pct_cmnty_cli_18_34", coef_by_states)


### --- MODEL 2. 55+ and StringencyIndex
variables.2 <- c("pct_cmnty_cli_55",
                 "pct_worked_outside_home_55",
                 "pct_avoid_contact_55",
                 "StringencyIndex")
mlvar.2 <- mlVAR(df_state_wide,
                 vars = variables.2,
                 idvar = "state_code", lags = 14,
                 temporal = "correlated", nCores = 12)
plot(mlvar.2, vsize=10, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

for (i in 1:length(mlvar.2$IDs)){
  
  plot(mlvar.2, vsize=10, label.cex=3, label.scale.equal=T, type="temporal", layout="circle",
       subject=i, title=mlvar.2$IDs[i])
  
}


### --- MODEL 3. 18-34 and seperate restrictions
variables.3 <- c("pct_cmnty_cli_18_34",
                 "pct_worked_outside_home_18_34",
                 "pct_avoid_contact_18_34",
                 "C2_Workplace_closing",
                 "C3_Cancel_public_events",
                 "C4_Restrictions_on_gatherings",
                 "C5_Close_public_transport")
mlvar.3 <- mlVAR(df_state_wide,
                 vars = variables.3,
                 idvar = "state_code", lags = 14,
                 temporal = "correlated", nCores = 12)

# Plot the overall US network
plot(mlvar.3, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

# Plot the networks by states
for (i in 1:length(mlvar.3$IDs)){
  plot(mlvar.3, vsize=10, label.cex=3, label.scale.equal=T, type="temporal", layout="circle",
       subject=i, title=mlvar.3$IDs[i])
}

# Obtain the coeficents by states
coef_by_states <- list()
for (i in 1:length(mlvar.3$IDs)){
  temp <- melt(getNet(mlvar.3, subject=i))
  temp$state <- mlvar.3$IDs[i]
  coef_by_states[[i]] <- temp
}
coef_by_states <- do.call(rbind, coef_by_states)


# Plot 1. pct_cmnty_cli_18_34 ---> pct_avoid_contact_18_34
plot.coef.map("pct_cmnty_cli_18_34", "pct_avoid_contact_18_34", coef_by_states)

# Plot 2. pct_avoid_contact_18_34 ---> pct_cmnty_cli_18_34
plot.coef.map("pct_avoid_contact_18_34", "pct_cmnty_cli_18_34", coef_by_states)

# Plot 3. C4_Restrictions_on_gatherings ---> pct_cmnty_cli_18_34
plot.coef.map("C4_Restrictions_on_gatherings", "pct_cmnty_cli_18_34", coef_by_states)


### --- MODEL 4. 55+ and seperate restrictions
variables.4 <- c("pct_cmnty_cli_55",
                 "pct_worked_outside_home_55",
                 "pct_avoid_contact_55",
                 "C2_Workplace_closing",
                 "C3_Cancel_public_events",
                 "C4_Restrictions_on_gatherings",
                 "C5_Close_public_transport")
mlvar.4 <- mlVAR(df_state_wide,
                 vars = variables.4,
                 idvar = "state_code", lags = 14,
                 temporal = "correlated", nCores = 12)
plot(mlvar.4, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

for (i in 1:length(mlvar.3$IDs)){
  plot(mlvar.4, vsize=10, label.cex=3, label.scale.equal=T, type="temporal", layout="circle",
       subject=i, title=mlvar.4$IDs[i])
}



