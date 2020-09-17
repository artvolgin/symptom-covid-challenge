
#####################################################################################################
################### DATA LOADING and PREPROCESSING
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

# ------------ UMD data
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data/UMD_survey_data"))

# Load smoothed data by countries
umd_files <- list.files()
umd_files_country_smoothed <- umd_files[grepl("country", umd_files) & grepl("smoothed", umd_files)]

df_country <- bind_rows(import(umd_files_country_smoothed[1]),
                        import(umd_files_country_smoothed[2]),
                        import(umd_files_country_smoothed[3]),
                        import(umd_files_country_smoothed[4]),
                        import(umd_files_country_smoothed[5]),
                        import(umd_files_country_smoothed[6]))

# Drop unweighted columns and rename others
keep_columns <- c("country_agg", "GID_0", "gender", "age_bucket",
                  "date", "rolling_total_responses", "weight_sums")
df_country <- df_country[,(colnames(df_country) %in% keep_columns) |
                           grepl("weighted", colnames(df_country))]
colnames(df_country) <- str_remove(colnames(df_country), "smoothed_")
colnames(df_country) <- str_remove(colnames(df_country), "_weighted")

# Drop countries with some no more than 50 dates missing, based on the age groups buckets
country_date <- df_country %>%
  filter(gender=="overall") %>%
  group_by(country_agg) %>%
  summarize(date_n=n()) %>%
  ungroup() %>%
  filter(date_n >= max(date_n)-50)
df_country <- df_country %>% filter(country_agg %in% country_date$country_agg)


# ------------ Google open data: Inforamtion about countries and COVID related statistics
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data/Google_open_data"))

# Load the data
df_google <- import("main.csv")
# Select only state-level data
df_google <- df_google %>% filter(subregion1_code=="", key!="UA_KBP")

# Merge datasets
df_country <- df_country %>% left_join(df_google, by=c("GID_0"="3166-1-alpha-3", "date"="date"))

# Remove Armenia due to the lack of information about the government response
df_country <- df_country %>% filter(country_agg != "Armenia")

# Remove redundant columns
df_country <- df_country %>% dplyr::select(-c(wikidata, datacommons, subregion1_code, subregion1_name, subregion2_code,
                                subregion2_name, locality_code, locality_name, "3166-1-alpha-2", aggregation_level,
                                related_locality, noaa_station, noaa_distance))

# ------------ Save the final dataset
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))
saveRDS(df_country, "df_country.rds")


