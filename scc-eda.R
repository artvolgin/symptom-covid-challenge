
#####################################################################################################
################### TIME SERIES PLOTS FOR EDA 
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
library(usmap)
# Extra
library(covidcast)

minmax_normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

# Set the working directory
setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))

# Load the data
df_state <- readRDS("df_state.rds")

# Function for plotting

plot.eda.ts <- function(df, st_code=F){
  
  if (st_code==F){
    
    df <- df %>%
      filter(gender=="overall", age_bucket=="overall") %>%
      dplyr::select(date, pct_cmnty_cli, pct_tested_no_result, pct_worked_outside_home,
                    pct_avoid_contact, StringencyIndex) %>%
      group_by(date) %>%
      summarise_all(mean) %>% 
      mutate(pct_cmnty_cli=minmax_normalize(pct_cmnty_cli),
             pct_avoid_contact=minmax_normalize(pct_avoid_contact),
             pct_tested_no_result=minmax_normalize(pct_tested_no_result),
             pct_worked_outside_home=minmax_normalize(pct_worked_outside_home),
             StringencyIndex=minmax_normalize(StringencyIndex))
    
  } else {
    
    df <- df %>% filter(state_code==st_code) %>% 
      filter(gender=="overall", age_bucket=="overall") %>%
      dplyr::select(date, pct_cmnty_cli, pct_tested_no_result, pct_worked_outside_home,
                    pct_avoid_contact, StringencyIndex) %>% 
      mutate(pct_cmnty_cli=minmax_normalize(pct_cmnty_cli),
             pct_avoid_contact=minmax_normalize(pct_avoid_contact),
             pct_tested_no_result=minmax_normalize(pct_tested_no_result),
             pct_worked_outside_home=minmax_normalize(pct_worked_outside_home),
             StringencyIndex=minmax_normalize(StringencyIndex))
  }
  
  df <- gather(df, group, value, -date)
  
  ggplot(df, aes(x = date, y = value)) + 
    geom_line(aes(color = group, linetype = group), size = 1.5) +
    scale_color_manual(values = c("coral2",
                                  "gray40",
                                  adjustcolor("cadetblue3", 0.5),
                                  adjustcolor("plum2", 0.5),
                                  "dodgerblue2")) +
    scale_linetype_manual(values=c("solid", "dashed", "solid", "solid", "solid")) +
    theme_classic() + 
    labs(x = "Date",
         y = "Normalized Value") + 
    theme(legend.position = "none",
          axis.title=element_text(size=18,face="bold"),
          axis.text=element_text(size=12))
  
}

# Plot for the US
plot.eda.ts(df_state)
# Plot for Texas
plot.eda.ts(df_state, 'tx')
# Plot for Florida
plot.eda.ts(df_state, 'fl')
# Plot for New York
plot.eda.ts(df_state, 'ny')

  
