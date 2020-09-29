
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
  filter(mean_total_responses>1000)
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
df_country <- df_country %>% dplyr::select(c(c("country_agg", "date", "age_bucket"), selected_vars))
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
                             id_cols = c("date", "country_agg", "stringency_index", "school_closing",
                                         "workplace_closing", "cancel_public_events", "restrictions_on_gatherings",
                                         "public_transport_closing", "stay_at_home_requirements"),
                             names_from = age_bucket,
                             # values_from = pct_wear_mask)
                             values_from = colnames(df_country)[startsWith(colnames(df_country), "pct_")])
colnames(df_country_wide) <- str_replace(colnames(df_country_wide), "-", "_")
colnames(df_country_wide) <- str_remove(colnames(df_country_wide), "\\+")
selected_vars <- colnames(df_country_wide)[3:ncol(df_country_wide)]


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


### -------------------------- MODELLING

df_uk <- df_country_wide %>% filter(country_agg=="France")

# VAR
Model1 <- VAR(df_uk[, c('pct_wear_mask_18_34', 'pct_cmnty_sick_18_34',
                     'pct_feel_worried_ill_covid19_18_34', 'pct_ever_tested_18_34',
                     'pct_feel_depressed_18_34')],
              p = 7) 
summary(Model1)
df_uk$

# Diagnostics
Serial1 <- serial.test(Model1, lags.pt = 7, type = "PT.asymptotic")
Serial1
Arch1 <- arch.test(Model1, lags.multi = 15, multivariate.only = TRUE)
Arch1
Norm1 <- normality.test(Model1, multivariate.only = TRUE)
Norm1
Stability1 <- stability(Model1, type = "OLS-CUSUM")
plot(Stability1)

# Granger causality
GrangerRRP<- causality(Model1, cause = "detrend_cases_prop")
GrangerRRP
GrangerM1 <- causality(Model1, cause = "detrend_pct_avoid_contact")
GrangerM1

# Impulse Response

irf1 <- vars::irf(Model1,
            impulse = "pct_ever_tested_18_34",
            response = "pct_wear_mask_18_34", n.ahead = 20, boot = TRUE)
plot(irf1, ylab = "pct_wear_mask_18_34")

irf2 <- vars::irf(Model1, impulse = "pct_ever_tested_18_34",
            response = "pct_feel_worried_ill_covid19_18_34", n.ahead = 20, boot = TRUE)
plot(irf2, ylab = "pct_feel_worried_ill_covid19_18_34")

irf3 <- vars::irf(Model1, impulse = "pct_cmnty_sick_18_34",
                  response = "pct_feel_depressed_18_34", n.ahead = 20, boot = TRUE)
plot(irf3, ylab = "pct_feel_depressed_18_34")



### --- MODEL 1. 
variables.1 <- c("pct_cmnty_sick_18_34", "pct_wear_mask_18_34",
                 "pct_feel_worried_ill_covid19_18_34", "pct_ever_tested_18_34",
                 "pct_attended_public_event_18_34")
mlvar.1 <- mlVAR(df_country_wide,
                vars = variables.1,
                idvar = "country_agg", lags = 14,
                temporal = "correlated", nCores = 12)
plot(mlvar.1, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")



