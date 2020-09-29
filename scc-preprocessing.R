
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
library(gridExtra)
library(tidyr)
library(dplyr)
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
# df_state <- df_state %>% filter(date > "2020-04-24")
# Select only observations that represent "overall" category and remove puerto rico
df_state <- df_state %>%
  # filter(gender=="overall", age_bucket=="overall", state_code!="pr")
  filter(state_code!="pr")

# Drop unweighted columns and rename others
keep_columns <- c("date", "state_code", "gender", "age_bucket", "summed_n")
df_state <- df_state[,(colnames(df_state) %in% keep_columns) | grepl("weighted", colnames(df_state))]
colnames(df_state) <- str_remove(colnames(df_state), "smoothed_")
colnames(df_state) <- str_remove(colnames(df_state), "_weighted")


# ------------ Oxford response data
# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data/Oxford_response_data"))
df_oxford <- import("OxCGRT_21_09_2020.csv")
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


# ------------ Save the data

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))

# Save the data
saveRDS(df_state, "df_state.rds")

# Load the data
df_state <- readRDS("df_state.rds")

