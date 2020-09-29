library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(urca)
library(tsDyn)

setwd("C:/Users/1/YandexDisk/covid_facebook/data")

# Load the data
df_state <- readRDS("df_state.rds") %>% filter (gender == 'overall') %>%
  dplyr::select(c(date, state_code, StringencyIndex, age_bucket, pct_cmnty_cli, cases_prop, pct_avoid_contact))
# colnames(df_country)
# hist(df_country$stringency_index)

# Date
df_state$date  = as.Date(df_state$date)
table(df_state$state_code)

# Long to wide format
df_state_wide <- pivot_wider(df_state, id_cols = c(date, state_code, cases_prop, StringencyIndex),
                             names_from = age_bucket,
                             values_from = c(pct_cmnty_cli, pct_avoid_contact))
colnames(df_state_wide)[5:ncol(df_state_wide)] <- c('pct_cmnty_cli_18', 'pct_cmnty_cli_35', 'pct_cmnty_cli_55',
                                                    'pct_cmnty_cli_overall', 'pct_avoid_contact_18',
                                                    'pct_avoid_contact_35', 'pct_avoid_contact_55',
                                                    'pct_avoid_contact_overall')
vecm_est_by_state <- c()
for (i in unique(df_state_wide$state_code)){
  
  # Filtering state and arranging
  df_sub = df_state_wide %>%
    filter(state_code == i) %>%
    arrange(date)

  # Lag selection
  lagselect <- VARselect(df_sub[, c('pct_cmnty_cli_18',
                                    'pct_cmnty_cli_55'
                                    # 'pct_avoid_contact_18',
                                    # 'pct_avoid_contact_55',
                                    # 'StringencyIndex'
                                    )], 
                         lag.max = 14, type = 'both')
  
  # Johansen Test for cointegration
  #jotest <- ca.jo(df_sub[, c('pct_cmnty_cli_18',
  #                           'pct_cmnty_cli_55',
  #                           'pct_avoid_contact_18',
  #                           'pct_avoid_contact_55',
  #                           'StringencyIndex')],
  #                type = "eigen", K = (max(lagselect$selection) - 1), ecdet = "trend", spec = "longrun")
  #summary(jotest)
  
  # Variables of interest
  Vars <- sort(c('pct_cmnty_cli_18',
                 'pct_cmnty_cli_55'
                 # 'pct_avoid_contact_18',
                 # 'pct_avoid_contact_55',
                 # 'StringencyIndex'
                 ))
  
  ###### VECM Modeling
  vecm <- VECM(df_sub[, Vars],
               lag = (max(lagselect$selection) - 1),
               estim = "ML", r = 1, include = 'both')
  coefMat.short <- summary(vecm)$coefMat[, c('Estimate', 'Pr(>|t|)')]
  
  # Extracting short-run coefficients
  names.short <- expand.grid(Vars, c('pct_cmnty_cli_18-1',
                                     'pct_cmnty_cli_55-1'
                                     # 'pct_avoid_contact_18-1',
                                     # 'pct_avoid_contact_55-1',
                                     # 'StringencyIndex-1'
                                     ))
  names.short.selected <- sort(paste0(names.short$Var1, ':', names.short$Var2))
  coefMat.short.selected <- as.data.frame(coefMat.short[names.short.selected, ])
  colnames(coefMat.short.selected) <- c('est_short', 'p_short')
  coefMat.short.selected$names <- substr(rownames(coefMat.short.selected), 1,
                                         nchar(rownames(coefMat.short.selected))-2)
  rownames(coefMat.short.selected) <- NULL
  
  # Extracting long-run coefficients
  names.ect <- expand.grid(Vars, 'ECT')
  names.ect.selected <- sort(paste0(names.ect$Var1, ':', names.ect$Var2))
  coefMat.long <- coefMat.short[names.ect.selected,]
  
  # Multiplying estimated kappas by gammas
  est_long <- c(outer(coefMat.long[, 'Estimate'], vecm$model.specific$coint))
  names.long <- expand.grid(Vars, Vars)
  coefMat.long.fin <- cbind.data.frame('names' = paste0(names.long$Var1, ':',
                                                        names.long$Var2),
                                       est_long, 'p_long' = rep(coefMat.long[,2],
                                                                times = length(Vars)))
  coefMat.long.fin$names <- as.character(coefMat.long.fin$names)
  
  # Combining long- and short-run estimates
  vecm_est <- coefMat.short.selected %>%
    left_join(coefMat.long.fin, by = 'names')
  vecm_est <- vecm_est[, c(3,1,2,4,5)]
  vecm_est_by_state <- rbind.data.frame(vecm_est_by_state, vecm_est)
}

# Adding state id
vecm_est_by_state <- cbind.data.frame(state_code = rep(unique(df_state_wide$state_code),
                                                       each = nrow(vecm_est)), vecm_est_by_state)
# Rounding
vecm_est_by_state[,-(1:2)] <- round(vecm_est_by_state[,-(1:2)], 3)

saveRDS(vecm_est_by_state, 'vecm_est_by_state.rds')


###################### PLOT THE RESULTS

# Function for plotting coefficents on the map
plot.coef.map <- function(selected_vars, vecm_est_by_state=vecm_est_by_state){
  
  selected_coefs <- vecm_est_by_state %>%
    filter(names==selected_vars)
  
  plot_usmap(data = selected_coefs, values = "est_long",
             color = "white", labels = T, label_color = "black") + 
    scale_fill_continuous(type = "viridis", name = "coef", label = scales::comma) + 
    theme(legend.position = "right",
          plot.title = element_text(hjust=0.5, size=18),
          plot.subtitle = element_text(hjust=0.5, size=14)) + 
    labs(title = paste0(selected_vars), subtitle = "")
  
}

vecm_est_by_state$state <- vecm_est_by_state$state_code
vecm_est_by_state[(vecm_est_by_state$p_long > 0.1),]$est_long <- NA

plot.coef.map("pct_cmnty_cli_55:pct_cmnty_cli_18", vecm_est_by_state)


