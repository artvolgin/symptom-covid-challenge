library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

setwd("C:/Users/1/YandexDisk/covid_facebook/data/CMU US symptom survey aggregates")

# Load the data
df_state <- readRDS("df_state.rds")
df_state$date  = as.Date(df_state$date)

# Texas subset
tx = df_state %>%
  filter(state_code == 'ny' & gender == 'overall' & age_bucket == 'overall') %>%
  arrange(date)

tx$cases_prop = ts(tx$cases_prop)
tx$pct_avoid_contact = ts(tx$pct_avoid_contact)

plot(tx$cases_prop)
plot(tx$pct_avoid_contact)

# Dickey-Fuller test for unit root 
pp.test(tx$cases_prop)
pp.test(tx$pct_avoid_contact)

# Detrending as we have unit root
tx <- tx %>% mutate(day = 1:n())

trend = stl(tx$cases_prop)$time.series[,2]
detrend_ts = demand - (trend - trend[1])
plot(detrend_ts)

tx$detrend_cases_prop <- residuals(lm(cases_prop ~ pct_avoid_contact + day + day^2, data = tx))
tx$detrend_pct_avoid_contact <- residuals(lm(pct_avoid_contact ~ cases_prop + day + day^2, data = tx))

# structural break
#tx$brk <- ifelse(tx$day <= 60, 1, 0)
lagselect <- VARselect(tx[, c('detrend_cases_prop', 'detrend_pct_avoid_contact')], 
                       lag.max = 5, type = "const")
lagselect$selection

# VAR
Model1 <- VAR(tx[, c('detrend_cases_prop', 'detrend_pct_avoid_contact')],
                 p = 5, type = "const") 
summary(Model1)

# Diagnostics
Serial1 <- serial.test(Model1, lags.pt = 5, type = "PT.asymptotic")
Serial1
Arch1 <- arch.test(Model1, lags.multi = 15, multivariate.only = TRUE)
Arch1
Norm1 <- normality.test(Model1, multivariate.only = TRUE)
Norm1
Stability1 <- stability(Model1, type = "OLS-CUSUM")
plot(Stability1)

# Granger causality
GrangerRRP<- causality(Model1, cause = "detrend_cases_prop")
GrangerRRP
GrangerM1 <- causality(Model1, cause = "detrend_pct_avoid_contact")
GrangerM1

# Impulse Response

irf1 <- irf(Model1, impulse = "detrend_cases_prop",
              response = "detrend_pct_avoid_contact", n.ahead = 20, boot = TRUE)
plot(irf1, ylab = "detrend_cases_prop")

irf2 <- irf(Model1, impulse = "detrend_pct_avoid_contact",
            response = "detrend_cases_prop", n.ahead = 20, boot = TRUE)
plot(irf2, ylab = "detrend_pct_avoid_contact")


