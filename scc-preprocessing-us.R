
#####################################################################################################
################### DATA LOADING AND PREPROCESSING
#####################################################################################################

### Load the packages
# Basic
library(ggplot2)
library(ggrepel)
library(ggfortify)
library(reshape2)
library(stringr)
library(rio)
library(dtwclust)
library(gridExtra)
library(tidyr)
library(dplyr)
library(zoo)
# Extra
library(covidcast)

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data/CMU_survey_data"))

# ------------ Main data

# Load the smoothed data by states
df_state <- import("overall-state-smoothed.csv")
df_state$date <- as.Date(df_state$date)
df_state <- df_state %>% filter(date < "2020-09-01",
                                date > "2020-04-24")
# Select only observations that represent "overall" category and remove puerto rico
df_state <- df_state %>% filter(state_code!="pr")

# Drop unweighted columns and rename others
keep_columns <- c("date", "state_code", "gender", "age_bucket", "summed_n")
df_state <- df_state[,(colnames(df_state) %in% keep_columns) | grepl("weighted", colnames(df_state))]
colnames(df_state) <- str_remove(colnames(df_state), "smoothed_")
colnames(df_state) <- str_remove(colnames(df_state), "_weighted")


# ------------ Oxford response data
# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data/Oxford_response_data"))
df_oxford <- import("OxCGRT_latest.csv")
df_oxford <- df_oxford %>%
  filter(RegionCode!="", CountryCode=="USA") %>%
  mutate(state_code=tolower(str_remove(RegionCode, "US_")),
         date=as.Date(paste(str_sub(Date, 1, 4), str_sub(Date, 5, 6), str_sub(Date, 7, 8), sep="-"))) %>%
  dplyr::select(-c("Date", "CountryName", "CountryCode", "RegionName", "RegionCode", "M1_Wildcard",
            "ConfirmedCases", "ConfirmedDeaths")) %>%
  dplyr::select(!ends_with("_Flag"))

### Append additional data to the main database
df_state <- df_state %>%
  left_join(df_oxford, by=c("state_code"="state_code", "date"="date"))

# Change the column name
df_state <- df_state %>% rename(pct_avoid_contact=pct_avoid_contact_all_or_most_time)


# ------------ Number of cases per population
# Load the number of cases per population
df_cases <- covidcast_signal("jhu-csse", "confirmed_7dav_incidence_prop",
                               min(df_state$date),
                               max(df_state$date),
                               geo_type = "state")
df_cases <- df_cases %>%
  data.frame() %>%
  rename("cases_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  dplyr::select("state_code", "date", "cases_prop")
df_cases$date <- as.Date(df_cases$date)

# Smooth the COVID casess
list_cases <- list()
i <- 1
for (st_code in unique(df_cases$state_code)){
  
  # Select the country of interest
  df_cases_selected <- df_cases %>% filter(state_code==st_code)
  # Smoothing
  df_cases_selected <- df_cases_selected[order(df_cases_selected$date),]

  # First smoothing
  df_cases_selected$cases_prop <- rollapply(df_cases_selected$cases_prop,
                           width=7, FUN=function(x) mean(x, na.rm=TRUE),
                           by=1, by.column=F, partial=F, fill=NA, align="right")
  
  # Save smoothed subdataset to list
  list_cases[[i]] <- df_cases_selected
  i <- i + 1
  print(i)
  
}
df_cases <- do.call(rbind, list_cases)

# Append additional data to the main database
df_state <- df_state %>%
  left_join(df_cases, by=c("state_code"="state_code", "date"="date"))

# ------------ Number of deaths per population
# Load the number of deaths per population
df_deaths <- covidcast_signal("jhu-csse", "deaths_7dav_incidence_prop", 
                              min(df_state$date),
                              max(df_state$date),
                              geo_type = "state")
df_deaths <- df_deaths %>% 
  data.frame() %>%
  rename("deaths_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  dplyr::select("state_code", "date", "deaths_prop")
df_deaths$date <- as.Date(df_deaths$date)

# Smooth the COVID deaths
list_deaths <- list()
i <- 1
for (st_code in unique(df_deaths$state_code)){
  
  # Select the country of interest
  df_deaths_selected <- df_deaths %>% filter(state_code==st_code)
  # Smoothing
  df_deaths_selected <- df_deaths_selected[order(df_deaths_selected$date),]
  
  # First smoothing
  df_deaths_selected$deaths_prop <- rollapply(df_deaths_selected$deaths_prop,
                                            width=7, FUN=function(x) mean(x, na.rm=TRUE),
                                            by=1, by.column=F, partial=F, fill=NA, align="right")
  
  # Save smoothed subdataset to list
  list_deaths[[i]] <- df_deaths_selected
  i <- i + 1
  print(i)
  
}
df_deaths <- do.call(rbind, list_deaths)

# Append additional data to the main database
df_state <- df_state %>%
  left_join(df_deaths, by=c("state_code"="state_code", "date"="date"))

# ------------ Percentage of COVID-related doctor’s visits

# Load the percentage of COVID-related doctor’s visits
df_doctors <- covidcast_signal("doctor-visits", "smoothed_adj_cli",
                               min(df_state$date),
                               max(df_state$date),
                               geo_type = "state")
df_doctors <- df_doctors %>% 
  data.frame() %>%
  rename("doctor_visits_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  dplyr::select("state_code", "date", "doctor_visits_prop")
df_doctors$date <- as.Date(df_doctors$date)

# Smooth the Doctors visists
list_doctors <- list()
i <- 1
for (st_code in unique(df_doctors$state_code)){
  
  # Select the country of interest
  df_doctors_selected <- df_doctors %>% filter(state_code==st_code)
  # Smoothing
  df_doctors_selected <- df_doctors_selected[order(df_doctors_selected$date),]
  
  # First smoothing
  df_doctors_selected$doctor_visits_prop <- rollapply(df_doctors_selected$doctor_visits_prop,
                                                      width=7, FUN=function(x) mean(x, na.rm=TRUE),
                                                      by=1, by.column=F, partial=F, fill=NA, align="right")

  # Save smoothed subdataset to list
  list_doctors[[i]] <- df_doctors_selected
  i <- i + 1
  print(i)
  
}
df_doctors <- do.call(rbind, list_doctors)
# Append additional data to the main database
df_state <- df_state %>%
  left_join(df_doctors, by=c("state_code"="state_code", "date"="date"))


# ------------ Percentage of new hospital admissions with a COVID-associated diagnosis
# Load the percentage of new hospital admissions with a COVID-associated diagnosis
df_hospital_admissions <- covidcast_signal("hospital-admissions", "smoothed_adj_covid19",
                                           min(df_state$date),
                                           max(df_state$date),
                                           geo_type = "state")
df_hospital_admissions <- df_hospital_admissions %>% 
  data.frame() %>%
  rename("hospital_admissions_prop"="value", "state_code"="geo_value", "date"="time_value") %>%
  dplyr::select("state_code", "date", "hospital_admissions_prop")
df_hospital_admissions$date <- as.Date(df_hospital_admissions$date)

# Smooth the COVID deaths
list_hospital_admissions <- list()
i <- 1
for (st_code in unique(df_hospital_admissions$state_code)){
  
  # Select the country of interest
  df_hospital_admissions_selected <- df_hospital_admissions %>% filter(state_code==st_code)
  # Smoothing
  df_hospital_admissions_selected <- df_hospital_admissions_selected[order(df_hospital_admissions_selected$date),]
  
  # First smoothing
  df_hospital_admissions_selected$hospital_admissions_prop <- rollapply(df_hospital_admissions_selected$hospital_admissions_prop,
                                             width=7, FUN=function(x) mean(x, na.rm=TRUE),
                                             by=1, by.column=F, partial=F, fill=NA, align="right")
  
  # Save smoothed subdataset to list
  list_hospital_admissions[[i]] <- df_hospital_admissions_selected
  i <- i + 1
  print(i)
  
}
df_hospital_admissions <- do.call(rbind, list_hospital_admissions)

# Append additional data to the main database
df_state <- df_state %>%
  left_join(df_hospital_admissions, by=c("state_code"="state_code", "date"="date"))



# ------------ Additional Smoothing on the survey data and COVID-cases

# Drop intersected groups between gender and age (for ex. 18-34 males). Samples are too small.
df_state <- df_state %>% filter(age_bucket=="overall" | gender=="overall")

# Smooth the data
smoothSurveyData <- function(st_cd, df_state){
  
  list_result <- list()
  
  # Columns to smooth
  smoothed_columns <- colnames(df_state)[grepl("pct", colnames(df_state))]
  # Select the state of interest
  df_state_selected <- df_state %>% filter(state_code==st_cd)
  nrow(df_state_selected)
  
  # Process the smoothing for each respective subgroup
  i <- 1
  
  for(gender_i in c("female", "male")){
    # Select specific gender and age group
    df_state_selected_sub <- df_state_selected %>% filter(gender==gender_i)
    # Smoothing
    df_state_selected_sub <- df_state_selected_sub[order(df_state_selected_sub$date),]
    df_state_selected.7 <- df_state_selected_sub[7:nrow(df_state_selected_sub),]
    
    for (colname in smoothed_columns){
      df_state_selected.7[colname] <- as.numeric(rollmean(df_state_selected_sub[colname], 7))
    }
    # Save smoothed subdataset to list
    list_result[[i]] <- df_state_selected.7
    i <- i + 1
  }
  
  for(age_bucket_i in c("18-34","35-54","55+")){
    # Select specific gender and age group
    df_state_selected_sub <- df_state_selected %>% filter(age_bucket==age_bucket_i)
    # Smoothing
    df_state_selected_sub <- df_state_selected_sub[order(df_state_selected_sub$date),]
    df_state_selected.7 <- df_state_selected_sub[7:nrow(df_state_selected_sub),]
    
    for (colname in smoothed_columns){
      df_state_selected.7[colname] <- as.numeric(rollmean(df_state_selected_sub[colname], 7))
    }
    # Save smoothed subdataset to list
    list_result[[i]] <- df_state_selected.7
    i <- i + 1
  }
  
  # Select specific gender and age group
  df_state_selected_sub <- df_state_selected %>% filter(age_bucket=="overall", gender=="overall")
  # Smoothing
  df_state_selected_sub <- df_state_selected_sub[order(df_state_selected_sub$date),]
  df_state_selected.7 <- df_state_selected_sub[7:nrow(df_state_selected_sub),]
  
  for (colname in smoothed_columns){
    df_state_selected.7[colname] <- as.numeric(rollmean(df_state_selected_sub[colname], 7))
  }
  # Save smoothed subdataset to list
  list_result[[i]] <- df_state_selected.7
  i <- i + 1
  
  df_result <- do.call(rbind, list_result)
  return(df_result)
  
}

# Obtain smoothed data for each country respectively 
list_states <- list()
state_codes <- unique(df_state$state_code)
for(j in 1:length(state_codes)){
  list_states[[j]] <- smoothSurveyData(state_codes[j], df_state)
  print(j)
}
df_state <- do.call(rbind, list_states)


# ------------ Filtering states

# Load the number of cases per population
df_total_cases <- covidcast_signal("jhu-csse", "confirmed_cumulative_num",
                             max(df_state$date),
                             max(df_state$date),
                             geo_type = "state")
df_total_cases <- df_total_cases %>%
  data.frame() %>%
  rename("total_cases"="value", "state_code"="geo_value") %>%
  dplyr::select("state_code", "total_cases")
# Select states with more than 20,000 cases
df_total_cases <- df_total_cases %>% filter(total_cases > 20000)
df_state <- df_state %>% filter(state_code %in% df_total_cases$state_code)

# Remove states with down trend in the number of new cases
down_trends_states <- c("ri", "ny", "nj", "nh", "me", "ma", "dc", "ct")
df_state <- df_state %>% filter(!(state_code %in% down_trends_states))
print(length(unique(df_state$state_code)))


# ------------ Time-series Clustering

# Prepare the data for the dtwclust
tslist_state <- df_state %>%
  filter(gender=="overall", age_bucket=="overall") %>%
  dplyr::select(date, state_code, cases_prop) %>%
  pivot_wider(., names_from=state_code, values_from=cases_prop) %>%
  arrange(date) %>%
  dplyr::select(-date) %>%
  as.list()

### Find the best clustering algorithm with grid-search type of approach

# Different parameters to try
k <- 2
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
  args = tsclust_args(dist = list(window.size = 14)),
  trace = TRUE)

# Find the best one in terms of Silhouette, Dunn, Calinski–Harabasz
lapply(hc_par, cvi, type = c("Sil", "D", "CH")) %>% 
  do.call(rbind, .) %>%
  apply(., MARGIN = 2, FUN = which.max)

# Look at the select solutions
plot(hc_par[[2]], type = "series")

# First one seems more appropriate to use
plot(hc_par[[2]])
best_solution <- hc_par[[2]] 

# Add cluster number to the main dataframe
state_cluster <- data.frame(cluster=as.character(best_solution@cluster),
                               state_code=best_solution$labels)
df_state <- df_state %>% left_join(state_cluster)

# ------------ Save the data

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))

# Save the data
saveRDS(df_state, "df_state.rds")



