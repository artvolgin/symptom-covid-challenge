
#####################################################################################################
################### DATA LOADING and PREPROCESSING - US STATES
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
# Time-series
library(dtwclust)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
# Extra
library(covidcast)

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data/CMU_survey_data"))

# ------------ Main data

# Load the smoothed data by states
df_state <- import("overall-state-smoothed.csv")
df_state$date <- as.Date(df_state$date)
df_state <- df_state %>% filter(date < "2020-08-29")

# Select only observations that represent "overall" category and remove puerto rico
df_state <- df_state %>%
  # filter(gender=="overall", age_bucket=="overall", state_code!="pr")
  filter(state_code!="pr")
df_state$date <- as.Date(df_state$date)

# Drop unweighted columns and rename others
keep_columns <- c("date", "state_code", "gender", "age_bucket", "summed_n")
df_state <- df_state[,(colnames(df_state) %in% keep_columns) | grepl("weighted", colnames(df_state))]
colnames(df_state) <- str_remove(colnames(df_state), "smoothed_")
colnames(df_state) <- str_remove(colnames(df_state), "_weighted")


# Remove outliers from the mean values
# TODO

# ------------ Additional data

### 1. Load the number of cases per population
# 1.1 Daily number of cases per population
df_cases.1 <- covidcast_signal("jhu-csse", "confirmed_7dav_incidence_prop",
                       min(df_state$date),
                       max(df_state$date),
                       geo_type = "state")
df_cases.1 <- df_cases.1 %>%
  data.frame() %>%
  rename("cases_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  select("state_code", "date", "cases_prop")
df_cases.1$date <- as.Date(df_cases.1$date)

# 1.2 Cumulative number of cases per population
df_cases.2 <- covidcast_signal("jhu-csse", "confirmed_7dav_cumulative_prop",
                               min(df_state$date),
                               max(df_state$date),
                               geo_type = "state")
df_cases.2 <- df_cases.2 %>%
  data.frame() %>%
  rename("cases_cum_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  select("state_code", "date", "cases_cum_prop")
df_cases.2$date <- as.Date(df_cases.2$date)

### 2. Load the number of deaths per population
df_deaths <- covidcast_signal("jhu-csse", "deaths_7dav_incidence_prop",
                             min(df_state$date),
                             max(df_state$date),
                             geo_type = "state")
df_deaths <- df_deaths %>% 
  data.frame() %>%
  rename("deaths_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  select("state_code", "date", "deaths_prop")
df_deaths$date <- as.Date(df_deaths$date)

### 3. Load the percentage of COVID-related doctor’s visits
df_doctors <- covidcast_signal("doctor-visits", "smoothed_adj_cli",
                              min(df_state$date),
                              max(df_state$date),
                              geo_type = "state")
df_doctors <- df_doctors %>% 
  data.frame() %>%
  rename("doctor_visits_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  select("state_code", "date", "doctor_visits_prop")
df_doctors$date <- as.Date(df_doctors$date)

### 4. Load the volume of COVID-related searches in Google
# TODO: What to do with missing data for 4 days?
df_google <- covidcast_signal("ght", "smoothed_search",
                               min(df_state$date),
                               max(df_state$date),
                               geo_type = "state")
df_google <- df_google %>% 
  data.frame() %>%
  rename("google_trend"="value", "state_code"="geo_value", "date"="time_value") %>%
  select("state_code", "date", "google_trend")
df_google$date <- as.Date(df_google$date)

### 5. Load the percentage of new hospital admissions with a COVID-associated diagnosis
df_hospital_admissions <- covidcast_signal("hospital-admissions", "smoothed_adj_covid19",
                                           min(df_state$date),
                                           max(df_state$date),
                                           geo_type = "state")
df_hospital_admissions <- df_hospital_admissions %>% 
  data.frame() %>%
  rename("hospital_admissions_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  select("state_code", "date", "hospital_admissions_prop")
df_hospital_admissions$date <- as.Date(df_hospital_admissions$date)

### 6. Load SafeGraph data
# 6.1 completely_home_prop
df_safegraph.1 <- covidcast_signal("safegraph", "completely_home_prop",
                                   min(df_state$date),
                                   max(df_state$date),
                                   geo_type = "state")
df_safegraph.1 <- df_safegraph.1 %>% 
  data.frame() %>%
  rename("completely_home_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  select("state_code", "date", "completely_home_prop")
df_safegraph.1$date <- as.Date(df_safegraph.1$date)

# 6.2 full_time_work_prop
df_safegraph.2 <- covidcast_signal("safegraph", "full_time_work_prop",
                                   min(df_state$date),
                                   max(df_state$date),
                                   geo_type = "state")
df_safegraph.2 <- df_safegraph.2 %>% 
  data.frame() %>%
  rename("full_time_work_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  select("state_code", "date", "full_time_work_prop")
df_safegraph.2$date <- as.Date(df_safegraph.2$date)

# 6.3 part_time_work_prop
df_safegraph.3 <- covidcast_signal("safegraph", "part_time_work_prop",
                                   min(df_state$date),
                                   max(df_state$date),
                                   geo_type = "state")
df_safegraph.3 <- df_safegraph.3 %>% 
  data.frame() %>%
  rename("part_time_work_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  select("state_code", "date", "part_time_work_prop")
df_safegraph.3$date <- as.Date(df_safegraph.3$date)

# 6.4 median_home_dwell_time
df_safegraph.4 <- covidcast_signal("safegraph", "median_home_dwell_time",
                                   min(df_state$date),
                                   max(df_state$date),
                                   geo_type = "state")
df_safegraph.4 <- df_safegraph.4 %>% 
  data.frame() %>%
  rename("median_home_dwell_time"="value", "state_code"="geo_value", "date"="time_value") %>%
  select("state_code", "date", "median_home_dwell_time")
df_safegraph.4$date <- as.Date(df_safegraph.4$date)

### Append additional data to the main database
df_state <- df_state %>%
  left_join(df_cases.1, by=c("state_code"="state_code", "date"="date"))
df_state <- df_state %>%
  left_join(df_cases.2, by=c("state_code"="state_code", "date"="date"))
df_state <- df_state %>%
  left_join(df_deaths, by=c("state_code"="state_code", "date"="date"))
df_state <- df_state %>%
  left_join(df_doctors, by=c("state_code"="state_code", "date"="date"))
df_state <- df_state %>%
  left_join(df_hospital_admissions, by=c("state_code"="state_code", "date"="date"))
df_state <- df_state %>%
  left_join(df_google, by=c("state_code"="state_code", "date"="date"))
df_state <- df_state %>%
  left_join(df_safegraph.1, by=c("state_code"="state_code", "date"="date"))
df_state <- df_state %>%
  left_join(df_safegraph.2, by=c("state_code"="state_code", "date"="date"))
df_state <- df_state %>%
  left_join(df_safegraph.3, by=c("state_code"="state_code", "date"="date"))
df_state <- df_state %>%
  left_join(df_safegraph.4, by=c("state_code"="state_code", "date"="date"))

# Change the column name
df_state <- df_state %>% rename(pct_avoid_contact=pct_avoid_contact_all_or_most_time)

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))

# Save the data
saveRDS(df_state, "df_state.rds")

# Load the data
df_state <- readRDS("df_state.rds")

