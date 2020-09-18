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
df_country <- readRDS("df_country.rds")
# colnames(df_country)
# hist(df_country$stringency_index)

# Date
df_country$date  = as.Date(df_country$date)
table(df_country$country_agg)

# Texas subset
df_sub = df_country %>%
  filter(country_agg == 'France' & gender == 'overall' & age_bucket == 'overall') %>%
  arrange(date)

# Mask wearing variable
df_sub$pct_wear_mask_all_most <- rowSums(df_sub[, c("pct_wear_mask_all_time",
                                              "pct_wear_mask_most_time")], na.rm = T)

# Filtering first 60 days
# Trend
df_sub <- df_sub %>% mutate(day = 1:n()) %>% filter(day > 10)

# Time series format
df_sub$new_confirmed = ts(df_sub$new_confirmed)
df_sub$pct_wear_mask_all_most = ts(df_sub$pct_wear_mask_all_most)

# Viewing
plot(df_sub$new_confirmed)
plot(df_sub$pct_wear_mask_all_most)

# Dickey-Fuller test for unit root 
summary(ur.df(df_sub$new_confirmed, type="none"))
summary(ur.df(df_sub$pct_wear_mask_all_most, type="none"))
# we have a unit root problem

# Detrending
df_sub$detrend_new_confirmed <- residuals(lm(new_confirmed ~ day + day^2, data = df_sub))
df_sub$detrend_pct_wear_mask_all_most <- residuals(lm(pct_wear_mask_all_most ~ day + day^2, data = df_sub))

# Time series format
df_sub$detrend_new_confirmed = ts(df_sub$detrend_new_confirmed)
df_sub$detrend_pct_wear_mask_all_most = ts(df_sub$detrend_pct_wear_mask_all_most)

# Viewing
plot(df_sub$detrend_new_confirmed)
plot(df_sub$detrend_pct_wear_mask_all_most)

# Unit root test
summary(ur.df(df_sub$detrend_new_confirmed, type="none"))
summary(ur.df(df_sub$detrend_pct_wear_mask_all_most, type="none"))
# the data seem non-stationary

df_sub <- df_sub[which(is.na(df_sub$stringency_index) == F),]

# Lag selection
lagselect <- VARselect(df_sub[, c('pct_cmnty_sick', 'pct_wear_mask_all_most')], 
                       lag.max = 20, type = 'both')
lagselect$selection

# Johansen Test
#jotest <- ca.jo(df_sub[, c('detrend_new_confirmed', 'detrend_pct_wear_mask_all_most')],
#             type = "eigen", K = 18, ecdet = "const", spec = "longrun")
#summary(jotest)
# Our variables are cointegrating

###### VECM Modeling
#vecm <- VECM(df_sub[, c('detrend_new_confirmed', 'detrend_pct_wear_mask_all_most')],
#             lag = 4, estim = "2OLS", include = 'const')
#vecm <- cajorls(jotest)
#vecm
#confint(vecm$rlm)
# Transform VEC to VAR with r = 1
#var <- vec2var(jotest, r = 1)


# VAR
Model1 <- VAR(df_sub[, c('pct_cmnty_sick', 'pct_wear_mask_all_most')],
                 p = 8, type = "trend", exogen = df_sub[,'stringency_index']) 
summary(Model1)

# Diagnostics
Serial1 <- serial.test(Model1)
Serial1
#plot(Serial1, names = "detrend_pct_wear_mask_all_most")
Arch1 <- arch.test(Model1, lags.multi = 15, multivariate.only = TRUE)
Arch1
Norm1 <- normality.test(Model1, multivariate.only = TRUE)
Norm1
Stability1 <- stability(Model1)
plot(Stability1)

# Granger causality
GrangerRRP<- causality(Model1, cause = "new_confirmed")
GrangerRRP
GrangerM1 <- causality(Model1, cause = "pct_wear_mask_all_most")
GrangerM1

# Impulse Response

irf1 <- irf(Model1, impulse = "new_confirmed",
              response = "pct_wear_mask_all_most", n.ahead = 20, boot = TRUE)
plot(irf1)

irf2 <- irf(Model1, impulse = "pct_wear_mask_all_most",
            response = "new_confirmed", n.ahead = 20, boot = TRUE)
plot(irf2)

irf3 <- irf(Model1, impulse = "pct_wear_mask_all_most",
            response = "pct_wear_mask_all_most", n.ahead = 20, boot = TRUE)
plot(irf3)

irf4 <- irf(Model1, impulse = "new_confirmed",
            response = "new_confirmed", n.ahead = 20, boot = TRUE)
plot(irf4)

# Cum
irf1c <- irf(Model1, impulse = "new_confirmed",
            response = "pct_wear_mask_all_most", n.ahead = 20, boot = TRUE, cumulative = T)
plot(irf1c)

irf2c <- irf(Model1, impulse = "pct_wear_mask_all_most",
            response = "new_confirmed", n.ahead = 20, boot = TRUE, cumulative = T)
plot(irf2c)

irf3c <- irf(Model1, impulse = "pct_wear_mask_all_most",
            response = "pct_wear_mask_all_most", n.ahead = 20, boot = TRUE, cumulative = T)
plot(irf3c)

irf4c <- irf(Model1, impulse = "new_confirmed",
            response = "new_confirmed", n.ahead = 20, boot = TRUE, cumulative = T)
plot(irf4c)
