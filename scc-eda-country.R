
#####################################################################################################
################### EDA - COUNTRIES
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
# Time-series
library(dtwclust)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
# Extra
library(covidcast)
library(dplyr)

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),"/YandexDisk/covid_facebook/data"))

# Load the data
df_country <- readRDS("df_country.rds")


# ------------ 1. Plot time-series graphs for each country


plot.country.ts <- function(country_name, agg_by_age=TRUE, variables_to_plot, ncol_plot, df_full){
  
  df_temp <- df_full[df_full$country_agg==country_name,]
  
  if (agg_by_age) {
    
    df_temp <- df_temp %>% filter(gender=="overall") %>% 
      dplyr::select(c(c("date", "age_bucket"),  variables_to_plot))
    
    df_temp <- melt(df_temp, id.vars = c('date', "age_bucket"))
    print(
      ggplot(
        df_temp,
        aes(x = date, y = value, color=age_bucket)) + 
        geom_line() + 
        facet_wrap(~ variable, scales = 'free_y', ncol = ncol_plot) + 
        ggtitle(country_name) + 
        theme(plot.title = element_text(hjust=0.5, size=24)) + 
        theme_bw()
    )
    
  } else {
    
    df_temp <- df_temp %>% filter(gender=="overall", age_bucket=="overall") %>% 
      dplyr::select(c(c("date"),  variables_to_plot))
    
    df_temp <- melt(df_temp, id.vars = c('date'))
    print(
      ggplot(
        df_temp,
        aes(x = date, y = value)) + 
        geom_line() + 
        facet_wrap(~ variable, scales = 'free_y', ncol = ncol_plot) + 
        ggtitle(country_name) + 
        theme(plot.title = element_text(hjust=0.5, size=24)) + 
        theme_bw()
    )
    
  }
}

variables_to_plot <- c( "new_confirmed", "pct_cmnty_sick",
                        "pct_wear_mask_most_time", "pct_wear_mask_none_time",
                        "pct_worried_ill_covid19_somewhat", "pct_attended_public_event")
for(cntry_name in unique(df_country$country_agg)){
  plot.country.ts(country_name = cntry_name, variables_to_plot, agg_by_age=F, 1, df_country)
}


df_country %>% select(pct_wear_mask_all_time, pct_wear_mask_most_time, pct_wear_mask_half_time,
                      pct_wear_mask_some_time, pct_wear_mask_none_time) %>% summary()

df_country %>% select(pct_worried_ill_covid19_very, pct_worried_ill_covid19_somewhat,
                      pct_worried_ill_covid19_notTooWorried, pct_worried_ill_covid19_notWorried) %>% summary()


# ------------ 2. Scatter plots

# Initiate a ggplot
b <- ggplot(df_state_agg, aes(x = mean_avoid_contact, y = cor_pct_cmnty_cli_pct_avoid_contact))
# Add text to the plot
b + geom_point() +
  geom_text_repel(aes(label = state_name), size = 4) + 
  theme_bw()


#####################################################################################################
################### TIME-SERIES CLUSTERING
#####################################################################################################

# Cluster time-series based on number of cases 

### Naive try

# Prepare the data for the dtwclust
tslist_state <- df_state %>%
  filter(gender=="overall", age_bucket=="overall") %>%
  select(date, state_code, cases_prop) %>%
  pivot_wider(., names_from=state_code, values_from=cases_prop) %>%
  arrange(date) %>%
  select(-date) %>%
  as.list()

# Obtain the solution
hc_ward <- tsclust(
  tslist_state,
  k = 3,                 # Number of clusters
  type = "hierarchical", # Type of the clustering
  distance = "dtw",      # Distance measure
  seed = 42,
  control = hierarchical_control(method = "ward.D2"), # Method for merging
  args = tsclust_args(dist = list(window.size = 7),), # Window size
  trace = T
)

# Plot the dendrogram
par(mar = c(0, 4, 2, 2))
plot(hc_ward, xlab = "", sub = "", main = "")

# Plot series splited by clusters
plot(hc_ward, type = "sc")


### Find the best clustering algorithm with grid-search type of approach

# Different parameters to try
k <- 3
method <- c("ward.D2", "average", "single", 
            "complete", "median", "mcquitty")

# Create clustering solution with all select parameters
hc_par <- tsclust(
  tslist_state,
  k = k,
  type = "hierarchical",
  distance = "dtw",
  seed = 42,
  control = hierarchical_control(method = method),
  args = tsclust_args(dist = list(window.size = 7)),
  trace = TRUE)

# Find the best one in terms of Silhouette, Dunn, Calinski–Harabasz
lapply(hc_par, cvi, type = c("Sil", "D", "CH")) %>% 
  do.call(rbind, .) %>%
  apply(., MARGIN = 2, FUN = which.max)

# Look at the select solutions
plot(hc_par[[1]], type = "series")
plot(hc_par[[3]], type = "series")
plot(hc_par[[5]], type = "series")

# First one seems more appropriate to use
plot(hc_par[[1]], type = "sc")


