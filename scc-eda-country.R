
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


setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))

### -------------------------- PREPROCESSING

# Load the data
df_country <- readRDS("df_country.rds")

# Date
df_country$date <- as.Date(df_country$date)
df_country <- df_country %>% filter(date < "2020-09-01")

df_country <- df_country %>% filter(gender=="overall")
colnames(df_country) <- str_replace_all(colnames(df_country), " ", "_")

# Select only high sampled countries
df_country_agg <- df_country %>%
  group_by(country_agg) %>%
  summarize(mean_total_responses=mean(total_responses)) %>%
  filter(mean_total_responses>500)
df_country <- df_country %>% filter(country_agg %in% df_country_agg$country_agg)

### Transform to wide format
selected_vars <- c("pct_cmnty_sick", "pct_ever_tested", "pct_worked_outside_home", "pct_grocery_outside_home",
                   "pct_ate_outside_home", "pct_attended_public_event", "pct_used_public_transit",
                   "pct_wear_mask_most_time", "pct_wear_mask_half_time", "pct_feel_nervous_all_time",
                   "pct_feel_nervous_most_time", "pct_feel_nervous_some_time", "pct_feel_depressed_all_time",
                   "pct_feel_depressed_most_time", "pct_feel_depressed_some_time", "pct_worried_ill_covid19_very",
                   "pct_worried_ill_covid19_somewhat", "stringency_index", "school_closing", "workplace_closing",
                   "cancel_public_events", "restrictions_on_gatherings", "public_transport_closing",
                   "stay_at_home_requirements")
df_country <- df_country %>% dplyr::select(c(c("country_agg", "date", "age_bucket", "gender"), selected_vars))
df_country <- df_country %>%
  mutate(pct_wear_mask=pct_wear_mask_half_time+pct_wear_mask_most_time,
         pct_feel_nervous=pct_feel_nervous_all_time+pct_feel_nervous_most_time+pct_feel_nervous_some_time,
         pct_feel_depressed=pct_feel_depressed_all_time+pct_feel_depressed_most_time+pct_feel_depressed_some_time,
         pct_feel_worried_ill_covid19=pct_worried_ill_covid19_very+pct_worried_ill_covid19_somewhat) %>%
  dplyr::select(-c(pct_wear_mask_half_time, pct_wear_mask_most_time,
                   pct_feel_nervous_all_time, pct_feel_nervous_most_time, pct_feel_nervous_some_time,
                   pct_feel_depressed_all_time, pct_feel_depressed_most_time, pct_feel_depressed_some_time,
                   pct_worried_ill_covid19_very, pct_worried_ill_covid19_somewhat))


df_country <- df_country[!(duplicated(df_country[, c("date", "country_agg", 'age_bucket')])),]

# Long to wide format
df_country_wide <- pivot_wider(df_country,
                               id_cols = c("date", "gender", "country_agg", "stringency_index", "school_closing",
                                           "workplace_closing", "cancel_public_events", "restrictions_on_gatherings",
                                           "public_transport_closing", "stay_at_home_requirements"),
                               names_from = age_bucket,
                               # values_from = pct_wear_mask)
                               values_from = colnames(df_country)[startsWith(colnames(df_country), "pct_")])
colnames(df_country_wide) <- str_replace(colnames(df_country_wide), "-", "_")
colnames(df_country_wide) <- str_remove(colnames(df_country_wide), "\\+")
selected_vars <- colnames(df_country_wide)[4:ncol(df_country_wide)]


list_countries <- list()
i <- 1
for (cntry in unique(df_country_wide$country_agg)){
  
  df_sub <- df_country_wide %>% filter(country_agg==cntry)
  
  for (var_name in selected_vars){
    
    df_sub[var_name] <- residuals(lm(as.formula(paste(var_name, " ~ date")), data = df_sub, na.action=na.exclude))
    
  }
  list_countries[[i]] <- df_sub
  i <- i + 1
  
}
df_country_wide <- do.call(rbind, list_countries)

# Arrange for modelling
df_country_wide <- df_country_wide %>% arrange(country_agg, date)


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



variables_to_plot <- c( "pct_cmnty_sick",
                        "pct_ever_tested", "pct_wear_mask",
                        "pct_feel_worried_ill_covid19", "pct_attended_public_event")
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


