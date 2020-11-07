
#####################################################################################################
################### DATA LOADING and PREPROCESSING - WORLD
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
             "/YandexDisk/covid_facebook/data/UMD_survey_data/country"))

# Load data by countries
umd_files <- list.files()
# umd_files_country <- umd_files[grepl("country", umd_files) & !grepl("smooth", umd_files)]
umd_files_country <- umd_files[grepl("country", umd_files) & grepl("smooth", umd_files)]
df_country <- bind_rows(import(umd_files_country[1]),
                        import(umd_files_country[2]),
                        import(umd_files_country[3]),
                        import(umd_files_country[4]),
                        import(umd_files_country[5]),
                        import(umd_files_country[6]),
                        import(umd_files_country[7]),
                        import(umd_files_country[8]))
# Filter by date
df_country <- df_country %>% filter(date < "2020-11-01")

# Drop unweighted columns and rename others
keep_columns <- c("country_agg", "GID_0", "gender", "age_bucket",
                  "date", "total_responses", "weight_sums")
df_country <- df_country[,(colnames(df_country) %in% keep_columns) |
                           grepl("weighted", colnames(df_country))]
df_country <- df_country[,(colnames(df_country) %in% keep_columns) |
                           !grepl("weighted_sums", colnames(df_country))]
colnames(df_country) <- str_remove(colnames(df_country), "_weighted")

# Drop intersected groups between gender and age (for ex. 18-34 males). Samples are too small.
df_country <- df_country %>% filter(age_bucket=="overall" | gender=="overall")

# Drop countries with some no more than 50 dates missing, based on the age groups buckets
country_date <- df_country %>%
  filter(gender=="overall") %>%
  group_by(country_agg) %>%
  summarize(date_n=n()) %>%
  ungroup() %>%
  filter(date_n >= max(date_n)-50)

df_country <- df_country %>% filter(country_agg %in% country_date$country_agg)

# Smooth the data
smoothSurveyData <- function(country_name, df_country){
  
  list_result <- list()
  # Select the country of interest
  df_country_selected <- df_country %>% filter(country_agg==country_name)
  
  # Process the smoothing for each respective subgroup
  i <- 1
  
  for(gender_i in c("female", "male")){
    # Select specific gender and age group
    df_country_selected_sub <- df_country_selected %>% filter(gender==gender_i)
    # Smoothing
    df_country_selected_sub <- df_country_selected_sub[order(df_country_selected_sub$date),]
    df_country_selected.7 <- df_country_selected_sub[7:nrow(df_country_selected_sub),]
    survey_columns <- colnames(df_country)[grepl("pct", colnames(df_country))]
    for (colname in survey_columns){
      df_country_selected.7[colname] <- as.numeric(rollmean(df_country_selected_sub[colname], 7))
    }
    # Save smoothed subdataset to list
    list_result[[i]] <- df_country_selected.7
    i <- i + 1
  }
  
  for(age_bucket_i in c("18-34","35-54","55+")){
    # Select specific gender and age group
    df_country_selected_sub <- df_country_selected %>% filter(age_bucket==age_bucket_i)
    # Smoothing
    df_country_selected_sub <- df_country_selected_sub[order(df_country_selected_sub$date),]
    df_country_selected.7 <- df_country_selected_sub[7:nrow(df_country_selected_sub),]
    survey_columns <- colnames(df_country)[grepl("pct", colnames(df_country))]
    for (colname in survey_columns){
      df_country_selected.7[colname] <- as.numeric(rollmean(df_country_selected_sub[colname], 7))
    }
    # Save smoothed subdataset to list
    list_result[[i]] <- df_country_selected.7
    i <- i + 1
  }
  
  # Select specific gender and age group
  df_country_selected_sub <- df_country_selected %>% filter(age_bucket=="overall", gender=="overall")
  # Smoothing
  df_country_selected_sub <- df_country_selected_sub[order(df_country_selected_sub$date),]
  df_country_selected.7 <- df_country_selected_sub[7:nrow(df_country_selected_sub),]
  survey_columns <- colnames(df_country)[grepl("pct", colnames(df_country))]
  for (colname in survey_columns){
    df_country_selected.7[colname] <- as.numeric(rollmean(df_country_selected_sub[colname], 7))
  }
  # Save smoothed subdataset to list
  list_result[[i]] <- df_country_selected.7
  i <- i + 1
  
  df_result <- do.call(rbind, list_result)
  return(df_result)
  
}

# Obtain smoothed data for each country respectively 
list_countries <- list()
country_names <- unique(df_country$country_agg)
for(j in 1:length(country_names)){
  list_countries[[j]] <- smoothSurveyData(country_names[j], df_country)
  print(j)
}
df_country <- do.call(rbind, list_countries)

# Remove observations from April
df_country <- df_country %>% filter(date >= "2020-05-01")

# Remove 'smoothed' from column names
colnames(df_country) <- str_remove(colnames(df_country), "smoothed_")


# ------------ Google open data: Inforamtion about countries and COVID related statistics
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data/Google_open_data"))

# Load the data
df_google <- import("main.csv")
# Select only state-level data
df_google <- df_google %>% filter(subregion1_code=="", key!="UA_KBP")

# Smooth the COVID statitics
list_google <- list()
i <- 1
for (country_key in unique(df_google$key)){
  
  # Select the country of interest
  df_google_selected <- df_google %>% filter(key==country_key)
  # Smoothing
  df_google_selected <- df_google_selected[order(df_google_selected$date),]
  # df_google_selected.7 <- df_google_selected[7:nrow(df_google_selected),]
  # covid_columns <- c("new_confirmed", "new_deceased", "new_recovered", "new_tested",
  #                    "total_confirmed", "total_deceased", "total_recovered", "total_tested")
  
  covid_columns <- c("new_confirmed", "new_deceased", "new_recovered", "new_tested")
  
  # for (colname in covid_columns){
  #   df_google_selected.7[colname] <- as.numeric(rollmean(df_google_selected[colname], 7))
  # }
  
  for (colname in covid_columns){
        
       # First smoothing
       temp_column <- rollapply(df_google_selected[colname],
                                width=7, FUN=function(x) mean(x, na.rm=TRUE),
                                by=1, by.column=F, partial=F, fill=NA, align="right")
       
       # # Second smoothing
       temp_column <- rollapply(temp_column,
                                width=7, FUN=function(x) mean(x, na.rm=TRUE),
                                by=1, by.column=F, partial=F, fill=NA, align="right")
       
      
       df_google_selected[colname] <- temp_column
  }
  
  # Save smoothed subdataset to list
  list_google[[i]] <- df_google_selected
  i <- i + 1
  print(i)
  
}
df_google <- do.call(rbind, list_google)

# Merge datasets
df_country <- df_country %>% left_join(df_google, by=c("GID_0"="3166-1-alpha-3", "date"="date"))

# Remove Armenia due to the lack of information about the government response
df_country <- df_country %>% filter(country_agg != "Armenia")

# Remove redundant columns
df_country <- df_country %>% dplyr::select(-c(wikidata, datacommons, subregion1_code, subregion1_name, subregion2_code,
                                              subregion2_name, locality_code, locality_name, "3166-1-alpha-2", aggregation_level,
                                              noaa_station, noaa_distance))

# ! Select countries with the more than 10,000 total number of COVID-19 cases
keep_countries <- (df_country %>%
  group_by(country_agg) %>%
  summarise(total_cases=max(total_confirmed, na.rm=T)) %>%
  filter(total_cases>=10000) %>%
  dplyr::select(country_agg))$country_agg
df_country <- df_country %>% filter(country_agg %in% keep_countries)

# FINAL NUMBER OF COUNTRIES
length(unique(df_country$country_agg))

# Calculate COVID-19 related statistics proportional to population
df_country <- df_country %>%
  mutate(new_confirmed_prop=new_confirmed/population,
         new_deceased_prop=new_deceased/population,
         new_recovered_prop=new_recovered/population,
         new_tested_prop=new_tested/population)

# ------------ Finding the peaks <<< IN PROGRESS

library(pracma)

# Print number of cases for each country

for (name in unique(df_country$country_agg)){
  df_sample <- df_country %>% filter(gender=="overall", age_bucket=="overall", country_agg==name)
  plot(df_sample$new_confirmed_prop, type="l", main=name)
  print(name)
  print(min(df_sample$new_confirmed_prop))
}

# Venezuela
df_venezuela <- df_country %>% filter(gender=="overall", age_bucket=="overall", country_agg=="Venezuela")
plot(df_venezuela$new_confirmed_prop, type="l", main="Venezuela")
findpeaks(df_venezuela$new_confirmed_prop, nups=7, ndowns=0, threshold=0.00001)

# UK
df_uk <- df_country %>% filter(gender=="overall", age_bucket=="overall", country_agg=="United Kingdom")
plot(df_uk$new_confirmed_prop, type="l", main="United Kingdom")
findpeaks(df_uk$new_confirmed_prop, nups=7, ndowns = 0)
findpeaks(df_uk$new_confirmed_prop, nups=0, ndowns = 7)

# Sweden
df_sweden <- df_country %>% filter(gender=="overall", age_bucket=="overall", country_agg=="Sweden")
plot(df_sweden$new_confirmed_prop, type="l", main="Sweden")
findpeaks(df_sweden$new_confirmed_prop, nups=7, ndowns = 0)
findpeaks(df_sweden$new_confirmed_prop, nups=0, ndowns = 7)

# South Korea
df_korea <- df_country %>% filter(gender=="overall", age_bucket=="overall", country_agg=="South Korea")
plot(df_korea$new_confirmed_prop, type="l", main="South Korea")

findpeaks(df_sample$new_confirmed_prop, nups=, ndowns=10)


# ------------ Time-series Clustering <<< IN PROGRESS

# Prepare the data for the dtwclust
tslist_countries <- df_country %>%
  filter(gender=="overall", age_bucket=="overall") %>%
  dplyr::select(date, country_agg, new_confirmed_prop) %>%
  pivot_wider(., names_from=country_agg, values_from=new_confirmed_prop) %>%
  arrange(date) %>%
  dplyr::select(-date) %>%
  as.list()

# Different parameters to try
k <- 4
method <- c("ward.D2", "average", "single", 
            "complete", "median", "mcquitty")

# Create clustering solution with all select parameters
hc_par <- tsclust(
  tslist_countries,
  k = k,
  type = "hierarchical",
  distance = "dtw",
  seed = 42,
  control = hierarchical_control(method = method),
  args = tsclust_args(dist = list(window.size = 14)),
  trace = TRUE)

# Find the best one in terms of Silhouette, Dunn, Calinski–Harabasz
lapply(hc_par, cvi, type = c("Sil", "D", "CH")) %>% 
  do.call(rbind, .) %>%
  apply(., MARGIN = 2, FUN = which.max)

# Look at the select solutions
plot(hc_par[[1]], type = "series")
plot(hc_par[[3]], type = "series")
plot(hc_par[[6]], type = "series")

# First one seems more appropriate to use
plot(hc_par[[6]])
hc_par[[6]]



# ------------ Save the final dataset
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))
saveRDS(df_country, "df_country.rds")
# Load the data
df_country <- readRDS("df_country.rds")


