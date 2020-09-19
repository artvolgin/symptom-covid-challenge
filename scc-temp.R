
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
df_state <- readRDS("df_state.rds")

# Date
df_state$date <- as.Date(df_state$date)
df_state <- df_state %>% filter(gender=="overall")

df_state <- df_state %>%
  dplyr::select(c(date, state_code, StringencyIndex, age_bucket,
                  pct_cmnty_cli, cases_prop, pct_avoid_contact))

# Long to wide format
df_state_wide <- pivot_wider(df_state, id_cols = c(date, state_code, cases_prop, StringencyIndex),
                             names_from = age_bucket,
                             values_from = c(pct_cmnty_cli, pct_avoid_contact))
colnames(df_state_wide)[5:ncol(df_state_wide)] <- c('pct_cmnty_cli_18', 'pct_cmnty_cli_35', 'pct_cmnty_cli_55',
                                                    'pct_cmnty_cli_overall', 'pct_avoid_contact_18',
                                                    'pct_avoid_contact_35', 'pct_avoid_contact_55',
                                                    'pct_avoid_contact_overall')
df_state_wide <- df_state_wide %>% dplyr::arrange(state_code, date)

# Estimate using correlated random effects:
fit1 <- mlVAR(df_state_wide,
              vars = c('pct_cmnty_cli_18', 'pct_cmnty_cli_55',
                       'pct_avoid_contact_18', 'pct_avoid_contact_55',
                       'StringencyIndex'),
              idvar = "state_code", lags = 7,
              temporal = "correlated", nCores = 12)

# Print some pointers:
plot(fit1, "temporal", title = "Estimated temporal relationships", layout = "circle")



