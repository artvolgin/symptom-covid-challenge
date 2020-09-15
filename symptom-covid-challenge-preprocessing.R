
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
             "/YandexDisk/covid_facebook/data/CMU US symptom survey aggregates"))

#####################################################################################################
################### DATA LOADING and PREPROCESSING
#####################################################################################################

# ------------ Main data

# Load the smoothed data by states
df_state <- import("overall-state-smoothed.csv")

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

# Save the data
saveRDS(df_state, "df_state.rds")

# Load the data
df_state <- readRDS("df_state.rds")

#####################################################################################################
################### EDA
#####################################################################################################


# ------------ 1. The Average daily number and the Total number of respondents in each state

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
  left_join(df_state_names %>% select(state_code_up, state_name)) %>%
  select(-state_code_up)
# Add name to District Columbia
df_state_agg[is.na(df_state_agg$state_name),]$state_name <- "District of Columbia"

# ! Select only states with more than a 10,000 respondents each day
df_state_agg <- df_state_agg %>% filter(mean_n > 10000)

# ------------ 2. Plot time-series graphs for each state

# vec_cor_cases_prop <- c()
# vec_cor_pct_avoid_contact <- c()

for (i in 1:nrow(df_state_agg)){
  
# Select only one state
df_state_one <- df_state %>% filter(state_code==df_state_agg$state_code[i])

# Remove redundant columns
df_state_one <- df_state_one %>%
  filter(gender=="overall") %>%
  select(date, age_bucket, cases_prop, pct_cmnty_cli,
         pct_tested_and_negative, pct_tested_no_result,
         pct_could_not_get_tested, pct_did_not_try_to_get_tested)

# Select only observations from 2020-04-21 onward
df_state_one <- df_state_one %>% filter(date >= "2020-04-21")

# Plot the data
df_melt = melt(df_state_one, id.vars = c('date', "age_bucket"))

print(ggplot(df_melt, aes(x = date, y = value, color=age_bucket)) + 
  geom_line() + 
  facet_wrap(~ variable, scales = 'free_y', ncol = 1) +
  ggtitle(paste(df_state_agg$state_name[i], round(df_state_agg$mean_n[i], 0))) + 
  theme(plot.title = element_text(hjust = 0.5, size=24)))

# Save correlation between pct_cmnty_cli and cases_prop
# vec_cor_cases_prop <- c(vec_cor_cases_prop, cor(df_state_one$pct_cmnty_cli, df_state_one$cases_prop))
# Save correlation between pct_cmnty_cli and pct_avoid_contact_all
# vec_cor_pct_avoid_contact <- c(vec_cor_pct_avoid_contact, cor(df_state_one$pct_cmnty_cli, df_state_one$pct_avoid_contact))

}


# Add correlations to the df_state_agg
df_state_agg$cor_pct_cmnty_cli_cases_prop <- vec_cor_cases_prop
df_state_agg$cor_pct_cmnty_cli_pct_avoid_contact <- vec_cor_pct_avoid_contact

# Mean level of avoid_contact through out the time by state
df_state_agg <- df_state_agg %>% left_join((df_state %>%
                           group_by(state_code) %>%
                           summarize(mean_avoid_contact=mean(pct_avoid_contact))))

# Total number of cases by state
df_state_agg <- df_state_agg %>% left_join((df_state %>%
                                            group_by(state_code) %>%
                                            summarize(total_cases=max(cases_cum_prop))))

# ------------ 3. Scatter plots

# Initiate a ggplot
b <- ggplot(df_state_agg, aes(x = mean_avoid_contact, y = cor_pct_cmnty_cli_pct_avoid_contact))
# Add text to the plot
b + geom_point() +
  geom_text_repel(aes(label = state_name), size = 4) + 
  theme_bw()

# Initiate a ggplot
b <- ggplot(df_state_agg, aes(x = total_cases, y = cor_pct_cmnty_cli_pct_avoid_contact))
# Add text to the plot
b + geom_point() +
  geom_text_repel(aes(label = state_name), size = 4) + 
  theme_bw()

# Initiate a ggplot
b <- ggplot(df_state_agg, aes(x = total_cases, y = cor_pct_cmnty_cli_cases_prop))
# Add text to the plot
b + geom_point() +
  geom_text_repel(aes(label = state_name), size = 4) + 
  theme_bw()



# Minmax scaling for plotting
# normalize <- function(x) { return((x- min(x)) /(max(x)-min(x))) }
# df_state_ca$cases_prop.scaled <- normalize(df_state_ca$cases_prop)
# df_state_ca$deaths_prop.scaled <- normalize(df_state_ca$deaths_prop)
# df_state_ca$doctor_visits_prop.scaled <- normalize(df_state_ca$doctor_visits_prop)
# df_state_ca$hospital_admissions_prop.scaled <- normalize(df_state_ca$hospital_admissions_prop)
# df_state_ca$google_trend.scaled <- normalize(df_state_ca$google_trend)


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





#####################################################################################################
################### COVIDCAST API
#####################################################################################################


# ------------ Example 1

cli <- suppressMessages(
  covidcast_signal(data_source = "fb-survey", signal = "smoothed_cli",
                   start_day = "2020-05-01", end_day = "2020-05-07",
                   geo_type = "state")
)
head(cli)


# ------------ Example 2

# Fetch Facebook % CLI-in-community signal and JHU confirmed case incidence
# numbers at the county level
start_day = "2020-06-01"
end_day = "2020-07-15"
df_fb = covidcast_signal("fb-survey", "smoothed_hh_cmnty_cli",
                         start_day, end_day, geo_type = "county")
df_in = covidcast_signal("jhu-csse", "confirmed_7dav_incidence_num",
                         start_day, end_day, geo_type = "county")

# Function to transform from one range to another
trans = function(x, from_range, to_range) {
  (x - from_range[1]) / (from_range[2] - from_range[1]) *
    (to_range[2] - to_range[1]) + to_range[1]
}

# Red, blue (similar to ggplot defaults), then yellow
ggplot_colors = c("#FC4E07", "#00AFBB", "#E7B800")

# Function to produce a plot comparing the signals for one county
plot_one = function(geo_value, title = NULL, xlab = NULL,
                    ylab1 = NULL, ylab2 = NULL, legend =  TRUE) {
  # Filter down the signal data frames
  given_geo_value = geo_value
  df_fb_one = df_fb %>% filter(geo_value == given_geo_value)
  df_in_one = df_in %>% filter(geo_value == given_geo_value)
  
  # Compute ranges of the two signals
  range1 = df_in_one %>% select("value") %>% range
  range2 = df_fb_one %>% select("value") %>% range
  
  # Convenience functions for our two signal ranges
  trans12 = function(x) trans(x, range1, range2)
  trans21 = function(x) trans(x, range2, range1)
  
  # Find state name, find abbreviation, then set title
  state_name = fips_to_name(paste0(substr(geo_value, 1, 2), "000"))
  state_abbr = name_to_abbr(state_name)
  title = paste0(fips_to_name(geo_value), ", ", state_abbr)
  
  # Transform the combined signal to the incidence range, then stack
  # these rowwise into one data frame
  df = select(rbind(df_fb_one %>% mutate_at("value", trans21),
                    df_in_one), c("time_value", "value"))
  df$signal = c(rep("% CLI-in-community", nrow(df_fb_one)),
                rep("New COVID-19 cases", nrow(df_in_one)))
  
  # Finally, plot both signals
  pos = ifelse(legend, "bottom", "none")
  return(ggplot(df, aes(x = time_value, y = value)) +
           geom_line(aes(color = signal)) +
           scale_color_manual(values = ggplot_colors[1:2]) +
           scale_y_continuous(name = ylab1, limits = range1,
                              sec.axis = sec_axis(trans = trans12,
                                                  name = ylab2)) +
           labs(title = title, x = xlab) + theme_bw() +
           theme(legend.pos = pos, legend.title = element_blank()))
}

# Produce a plot for Miami-Dade, and add vertical lines
plot_one(name_to_fips("Miami-Dade"), xlab = "Date",
         ylab1 = "Daily new confirmed COVID-19 cases",
         ylab2 = "% of people who know someone with CLI") +
  geom_vline(xintercept = as.numeric(as.Date("2020-06-19")),
             linetype = 2, size = 1, color = ggplot_colors[1]) +
  geom_vline(xintercept = as.numeric(as.Date("2020-06-25")),
             linetype = 2, size = 1, color = ggplot_colors[2])

num = 20
geo_values = df_in %>% group_by(geo_value) %>%
  summarize(diff = last(value) - first(value)) %>%
  arrange(desc(diff)) %>% head(num) %>% pull(geo_value)

p_list = vector("list", num)
for (i in 1:num) {
  p_list[[i]] = plot_one(geo_values[i], legend = FALSE)
}
do.call(grid.arrange, c(p_list, nrow = 5, ncol = 4))


