
#####################################################################################################
################### EDA - US STATES
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
             "/YandexDisk/covid_facebook/data"))

# Load the data
df_state <- readRDS("df_state.rds")


# ------------ 1. Preprocessing for EDA

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
  left_join(df_state_names %>% dplyr::select(state_code_up, state_name)) %>%
  dplyr::select(-state_code_up)
# Add name to District Columbia
df_state_agg[is.na(df_state_agg$state_name),]$state_name <- "District of Columbia"

# Add state names to the the df_state
df_state <- df_state %>% left_join(df_state_agg %>% dplyr::select(state_code, state_name))

# Add additional time-invariant aggregates for each state
df_state_agg <- df_state_agg %>% left_join(df_state %>%
                             filter(gender=="overall", age_bucket=="overall") %>%
                             group_by(state_code) %>%
                             summarize(mean_pct_cmnty_cli=mean(pct_cmnty_cli),
                                       mean_pct_avoid_contact=mean(pct_avoid_contact),
                                       total_cases=max(cases_cum_prop),
                                       max_stringency_index=max(StringencyIndex)))
# Add variable for usmap package
df_state_agg$state <- toupper(df_state_agg$state_code)

# ! Select only states with more than a 10,000 respondents each day
# df_state_agg <- df_state_agg %>% filter(mean_n > 5000)

# ------------ 2. Time Series Plots

plot.state.ts <- function(state_name, agg_by_age=TRUE, variables_to_plot, ncol_plot, df_full){
  
  df_temp <- df_full[df_full$state_name==state_name,]
  
  if (agg_by_age) {
    
    df_temp <- df_temp %>% filter(gender=="overall") %>% 
      dplyr::select(c(c("date", "age_bucket"),  variables_to_plot))
    
    df_temp <- melt(df_temp, id.vars = c('date', "age_bucket"))
    print(
      ggplot(
        df_temp,
        aes(x = date, y = value, color=age_bucket)) + 
        geom_line(size=1) + 
        facet_wrap(~ variable, scales = 'free_y', ncol = ncol_plot) + 
        scale_color_manual(values=c('forestgreen','gold', 'coral', 'darkgrey')) +
        ggtitle(state_name) + 
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
        geom_line(size=1) + 
        facet_wrap(~ variable, scales = 'free_y', ncol = ncol_plot) + 
        ggtitle(state_name) + 
        theme(plot.title = element_text(hjust=0.5, size=24)) + 
        theme_bw()
    )
    
  }
}


variables_to_plot <- c("cases_prop", "StringencyIndex", "pct_cmnty_cli", "pct_avoid_contact")
for(st_name in unique(df_state_agg$state_name)){
  plot.state.ts(st_name, variables_to_plot, agg_by_age=T, 1, df_state)
}



# ------------ 3. Spatial Plots

df_vecm_est_by_state <- readRDS("vecm_est_by_state.rds")
df_vecm_est_by_state$state <- toupper(df_vecm_est_by_state$state_code)

t <- df_vecm_est_by_state %>% filter(names=="pct_avoid_contact_55:StringencyIndex")
t <- df_vecm_est_by_state %>% filter(names=="pct_avoid_contact_18:StringencyIndex")
t <- df_vecm_est_by_state %>% filter(names=="pct_cmnty_cli_55:StringencyIndex")
t <- df_vecm_est_by_state %>% filter(names=="pct_cmnty_cli_18:StringencyIndex")
t <- df_vecm_est_by_state %>% filter(names=="pct_cmnty_cli_55:pct_avoid_contact_18")
t <- df_vecm_est_by_state %>% filter(names=="pct_cmnty_cli_55:pct_cmnty_cli_18")

t[t$p_short > 0.1,]$est_short <- NA
t[t$p_long > 0.1,]$est_long <- NA

# Plot 0.1 est_short
plot_usmap(data = t, values = "est_short",
           color = "white", labels = T, label_color = "black") + 
  scale_fill_continuous(type = "viridis", name = "est_short", label = scales::comma) + 
  theme(legend.position = "right",
        plot.title = element_text(hjust=0.5, size=24),
        plot.subtitle = element_text(hjust=0.5, size=14)) + 
  labs(title = "est_short", subtitle = "est_short")

# Plot 0.2 est_long
plot_usmap(data = t, values = "est_long",
           color = "white", labels = T, label_color = "black") + 
  scale_fill_continuous(type = "viridis", name = "est_long", label = scales::comma) + 
  theme(legend.position = "right",
        plot.title = element_text(hjust=0.5, size=24),
        plot.subtitle = element_text(hjust=0.5, size=14)) + 
  labs(title = "est_long", subtitle = "est_long")






# Plot 1. total_cases
plot_usmap(data = df_state_agg, values = "total_cases",
           color = "white", labels = T, label_color = "black") + 
  scale_fill_continuous(type = "viridis", name = "total_cases", label = scales::comma) + 
  theme(legend.position = "right",
        plot.title = element_text(hjust=0.5, size=24),
        plot.subtitle = element_text(hjust=0.5, size=14)) + 
  labs(title = "total_cases", subtitle = "total_cases")

# Plot 2. mean_pct_cmnty_cli
plot_usmap(data = df_state_agg, values = "mean_pct_cmnty_cli",
           color = "white", labels = T, label_color = "black") + 
  scale_fill_continuous(type = "viridis", name = "mean_pct_cmnty_cli", label = scales::comma) + 
  theme(legend.position = "right",
        plot.title = element_text(hjust=0.5, size=24),
        plot.subtitle = element_text(hjust=0.5, size=14)) + 
  labs(title = "mean_pct_cmnty_cli", subtitle = "mean_pct_cmnty_cli")

# Plot 3. mean_pct_avoid_contact
plot_usmap(data = df_state_agg, values = "mean_pct_avoid_contact",
           color = "white", labels = T, label_color = "black") + 
  scale_fill_continuous(type = "viridis", name = "mean_pct_avoid_contact", label = scales::comma) + 
  theme(legend.position = "right",
        plot.title = element_text(hjust=0.5, size=24),
        plot.subtitle = element_text(hjust=0.5, size=14)) + 
  labs(title = "mean_pct_avoid_contact", subtitle = "mean_pct_avoid_contact")

# Plot 4. max_stringency_index
plot_usmap(data = df_state_agg, values = "max_stringency_index",
           color = "white", labels = T, label_color = "black") + 
  scale_fill_continuous(type = "viridis", name = "max_stringency_index", label = scales::comma) + 
  theme(legend.position = "right",
        plot.title = element_text(hjust=0.5, size=24),
        plot.subtitle = element_text(hjust=0.5, size=14)) + 
  labs(title = "max_stringency_index", subtitle = "max_stringency_index")





#####################################################################################################
################### OLD CODE
#####################################################################################################

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
