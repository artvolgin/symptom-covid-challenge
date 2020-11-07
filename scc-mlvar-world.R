
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
df_country <- readRDS("df_country.rds")

# Date
# df_country$date <- as.Date(df_country$date)
# df_country <- df_country %>% filter(date < "2020-09-01")

df_country <- df_country %>% filter(gender=="overall")
colnames(df_country) <- str_replace_all(colnames(df_country), " ", "_")

# Select only high sampled countries
# df_country_agg <- df_country %>%
#   group_by(country_agg) %>%
#   summarize(mean_total_responses=mean(total_responses)) %>%
#   filter(mean_total_responses>1000)
# df_country <- df_country %>% filter(country_agg %in% df_country_agg$country_agg)


### Transform to wide format
# selected_vars <- c("pct_cmnty_sick", "pct_ever_tested", "pct_worked_outside_home", "pct_grocery_outside_home",
#                    "pct_ate_outside_home", "pct_attended_public_event", "pct_used_public_transit",
#                    "pct_wear_mask_most_time", "pct_wear_mask_half_time", "pct_feel_nervous_all_time",
#                    "pct_feel_nervous_most_time", "pct_feel_nervous_some_time", "pct_feel_depressed_all_time",
#                    "pct_feel_depressed_most_time", "pct_feel_depressed_some_time", "pct_worried_ill_covid19_very",
#                    "pct_worried_ill_covid19_somewhat", "stringency_index", "school_closing", "workplace_closing",
#                    "cancel_public_events", "restrictions_on_gatherings", "public_transport_closing",
#                    "stay_at_home_requirements")
# 
# df_country <- df_country %>% dplyr::select(c(c("country_agg", "date", "age_bucket"), selected_vars))


# Aggregate top 2 categories for the variables of interest
df_country <- df_country %>%
  mutate(pct_wear_mask=pct_wear_mask_half_time+pct_wear_mask_most_time,
         pct_feel_nervous=pct_feel_nervous_all_time+pct_feel_nervous_most_time+pct_feel_nervous_some_time,
         pct_feel_depressed=pct_feel_depressed_all_time+pct_feel_depressed_most_time+pct_feel_depressed_some_time,
         pct_feel_worried_ill_covid19=pct_worried_ill_covid19_very+pct_worried_ill_covid19_somewhat,
         pct_finances_worried=pct_finances_very_worried+pct_finances_somewhat_worried,
         pct_enough_toEat_worried=pct_enough_toEat_very_worried+pct_enough_toEat_somewhat_worried)

# df_country <- df_country[!(duplicated(df_country[, c("date", "country_agg", 'age_bucket')])),]

# Long to wide format
df_country_wide <- pivot_wider(df_country,
                               id_cols = c("date", "country_agg", "stringency_index", "school_closing",
                                           "workplace_closing", "cancel_public_events", "restrictions_on_gatherings",
                                           "public_transport_closing", "stay_at_home_requirements"),
                               names_from = age_bucket,
                               # values_from = pct_wear_mask)
                               values_from = colnames(df_country)[startsWith(colnames(df_country), "pct_")])
colnames(df_country_wide) <- str_replace(colnames(df_country_wide), "-", "_")
colnames(df_country_wide) <- str_remove(colnames(df_country_wide), "\\+")
selected_vars <- colnames(df_country_wide)[3:ncol(df_country_wide)]

# Make stationary by regressing variables on time
list_countries <- list()
i <- 1
for (cntry in unique(df_country_wide$country_agg)){
  
  df_sub <- df_country_wide %>% filter(country_agg==cntry)
  
  for (var_name in selected_vars){
    
    df_sub[var_name] <- residuals(lm(as.formula(paste(var_name, " ~ date")), data = df_sub, na.action=na.exclude))
    
  }
  list_countries[[i]] <- df_sub
  i <- i + 1
  
}
df_country_wide <- do.call(rbind, list_countries)

# Arrange for modelling
df_country_wide <- df_country_wide %>% arrange(country_agg, date)

### -------------------------- MVAR MODELLING

# TODO:
# 1. What to use as the indicator for the mask wearing?


### --- MODEL 1

# - 1.1 Overall
variables.1.1 <- c("pct_cmnty_sick_overall", # "pct_wear_mask_overall",
                   "stringency_index", "pct_ever_tested_overall",
                   "pct_attended_public_event_overall", "pct_no_public_overall")
mlvar.1.1 <- mlVAR(df_country_wide,
                 vars = variables.1.1,
                 idvar = "country_agg", lags = 14,
                 temporal = "correlated", nCores = 12)
plot(mlvar.1.1, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")



# --- TESTING: Compute confidence intervals for random effects >>>>>>>>>>>>>>>>>>>>>>>
library(lme4)

lmer_model <- mlvar.1.1$output$temporal$pct_cmnty_sick_overall

cc <- coef(lmer_model)$country_agg
cc <- cc[1:6]

## variances of fixed effects
fixed.vars <- diag(vcov(lmer_model))
fixed.vars <- fixed.vars[1:6]

## extract variances of conditional modes
r1 <- ranef(lmer_model, condVar=TRUE)

cmode.vars <- t(apply(cv <- attr(r1[[1]],"postVar"),3,diag))
seVals <- sqrt(sweep(cmode.vars,2,fixed.vars,"+"))
res <- cbind(cc,seVals)
# Drop intercept and SE for intercept
res <- res %>% dplyr::select(-c("(Intercept)", "1"))
# Calculate significance
res_mean <- res[1:(ncol(res)/2)]
colnames(res_mean) <- paste0("V", as.character(c(1:ncol(res_mean))), "_mean")
res_se <- res[((ncol(res)/2)+1):ncol(res)]
colnames(res_se) <- paste0("V", as.character(c(1:ncol(res_se))), "_se")
res_low <- res_mean - (2*res_se)
res_high <- res_mean + (2*res_se)
res_signif <- as.data.frame(sign(res_low) == sign(res_high))
colnames(res_signif) <- paste0("V", as.character(c(1:ncol(res_signif))), "_signif")
colSums(res_signif)
res_full <- cbind(res_mean, res_se, res_signif)

# --- TESTING: Compute confidence intervals for random effects >>>>>>>>>>>>>>>>>>>>>>>



# res2 <- setNames(res[,c(1,3,2,4)],
#                 c("int","int_se","slope","slope_se"))

# Obtain coeficents for each country
coef_by_countries <- list()
for (i in 1:length(mlvar.1.1$IDs)){
  temp <- melt(getNet(mlvar.1.1, subject=i))
  temp$country <- mlvar.1.1$IDs[i]
  coef_by_countries[[i]] <- temp
}
coef_by_countries <- do.call(rbind, coef_by_countries)




# --- TESTING: Different lags

# - Compute the same model with different number of lags
list_mlvars <- list()
for (i in c(1:14)){
  
  mlvar.temp <- mlVAR(df_country_wide,
                      vars = variables.1.1,
                      idvar = "country_agg", lags = i,
                      temporal = "correlated", nCores = 12)
  list_mlvars[[i]] <- mlvar.temp
  print(i)
  
}

for (i in c(1:14)){
  
  plot(list_mlvars[[i]], vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")
  print(i)
  
}




# - 1.2 18-34
variables.1.2 <- c("pct_cmnty_sick_18_34", "pct_wear_mask_18_34", "stringency_index", "pct_ever_tested_18_34",
                   "pct_attended_public_event_18_34", "pct_no_public_18_34")
mlvar.1.2 <- mlVAR(df_country_wide,
                   vars = variables.1.2,
                   idvar = "country_agg", lags = 14,
                   temporal = "correlated", nCores = 12)
plot(mlvar.1.2, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

# - 1.3 55+
variables.1.3 <- c("pct_cmnty_sick_55", "pct_wear_mask_55", "stringency_index", "pct_ever_tested_55",
                   "pct_attended_public_event_55", "pct_no_public_55")
mlvar.1.3 <- mlVAR(df_country_wide,
                   vars = variables.1.3,
                   idvar = "country_agg", lags = 14,
                   temporal = "correlated", nCores = 12)
plot(mlvar.1.3, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")


### --- MODEL 2

# - 2.1 Overall
variables.2.1 <- c("pct_cmnty_sick_overall",  "stringency_index", "pct_ever_tested_overall",
                 "pct_feel_nervous_overall", "pct_feel_depressed_overall", "pct_feel_worried_ill_covid19_overall")
mlvar.2.1 <- mlVAR(df_country_wide,
                 vars = variables.2.1,
                 idvar = "country_agg", lags = 14,
                 temporal = "correlated", nCores = 12)
plot(mlvar.2.1, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

# - 2.2 18-34
variables.2.2 <- c("pct_cmnty_sick_18_34",  "stringency_index", "pct_ever_tested_18_34",
                   "pct_feel_nervous_18_34", "pct_feel_depressed_18_34", "pct_feel_worried_ill_covid19_18_34")
mlvar.2.2 <- mlVAR(df_country_wide,
                   vars = variables.2.2,
                   idvar = "country_agg", lags = 14,
                   temporal = "correlated", nCores = 12)
plot(mlvar.2.2, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

# - 2.3 55+
variables.2.3 <- c("pct_cmnty_sick_55",  "stringency_index", "pct_ever_tested_55",
                   "pct_feel_nervous_55", "pct_feel_depressed_55", "pct_feel_worried_ill_covid19_55")
mlvar.2.3 <- mlVAR(df_country_wide,
                   vars = variables.2.3,
                   idvar = "country_agg", lags = 14,
                   temporal = "correlated", nCores = 12)
plot(mlvar.2.3, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")


### --- MODEL 3

# - 3.1 Overall
variables.3.1 <- c("pct_cmnty_sick_overall", "pct_no_public_overall", "pct_feel_depressed_overall",
                 "pct_feel_worried_ill_covid19_overall", "pct_finances_worried_overall",
                 "pct_enough_toEat_worried_overall")
mlvar.3.1 <- mlVAR(df_country_wide,
                 vars = variables.3.1,
                 idvar = "country_agg", lags = 14,
                 temporal = "correlated", nCores = 12)
plot(mlvar.3.1, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

# - 3.2 18-34
variables.3.2 <- c("pct_cmnty_sick_18_34", "pct_no_public_18_34", "pct_feel_depressed_18_34",
                   "pct_feel_worried_ill_covid19_18_34", "pct_finances_worried_18_34",
                   "pct_enough_toEat_worried_18_34")
mlvar.3.2 <- mlVAR(df_country_wide,
                   vars = variables.3.2,
                   idvar = "country_agg", lags = 14,
                   temporal = "correlated", nCores = 12)
plot(mlvar.3.2, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

# - 3.3 55+
variables.3.3 <- c("pct_cmnty_sick_55", "pct_no_public_55", "pct_feel_depressed_55",
                   "pct_feel_worried_ill_covid19_overall", "pct_finances_worried_55",
                   "pct_enough_toEat_worried_55")
mlvar.3.3 <- mlVAR(df_country_wide,
                   vars = variables.3.3,
                   idvar = "country_agg", lags = 14,
                   temporal = "correlated", nCores = 12)
plot(mlvar.3.3, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")


### --- MODEL 4: Different answers about wearing the mask

# - 4.1 Overall
variables.4.1 <- c("pct_cmnty_sick_overall", "pct_wear_mask_all_time_overall",
                   "stringency_index", "pct_ever_tested_overall",
                   "pct_attended_public_event_overall", "pct_no_public_overall")
mlvar.4.1 <- mlVAR(df_country_wide,
                   vars = variables.4.1,
                   idvar = "country_agg", lags = 10,
                   temporal = "correlated", nCores = 12)
plot(mlvar.4.1, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")


### --- MODEL 5: COVID symptoms separately

# - 5.1 Overall
variables.5.1 <- c("pct_cmnty_sick_overall", "pct_wear_mask_all_time_overall",
                   "stringency_index", "pct_ever_tested_overall",
                   "pct_attended_public_event_overall", "pct_no_public_overall")
mlvar.5.1 <- mlVAR(df_country_wide,
                   vars = variables.5.1,
                   idvar = "country_agg", lags = 7,
                   temporal = "correlated", nCores = 12)
plot(mlvar.5.1, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")






### OLD

### Block-boostraping for time-series
# df_temp <- df_country_wide %>% filter(country_agg=="Austria")
# ts_vec <- df_temp$pct_cmnty_sick_overall
# t <- tsbootstrap(ts_vec, n=100, statistic=mean, m=2)

### Trying to obtain the confidence level

# # Obtain coeficents for each country
# coef_by_countries <- list()
# for (i in 1:length(mlvar.1$IDs)){
#   temp <- melt(getNet(mlvar.1, subject=i))
#   temp$state <- mlvar.1$IDs[i]
#   coef_by_countries[[i]] <- temp
# }
# coef_by_countries <- do.call(rbind, coef_by_countries)
# 
# vals <- coef_by_countries %>%
#   filter(Var1=="pct_ever_tested_overall", Var2=="pct_cmnty_sick_overall") %>%
#   dplyr::select(value)
# mean(vals$value)
# mean_matrix <- as.data.frame(mlvar.1$results$Beta$mean)
# 
# # Calculate standard errors for the the coefficients
# matrix_varcov <- mlvar.1$results$Omega_mu$cov$mean
# mlvar.1$results$Theta$cov$mean
# mlvar.1$results$Beta$
# 
# t <- as.data.frame(mlvar.1$results$Beta$mean)
# mlvar.1$results$Omega_mu$cov
# mlvar.1$results$Beta$subject[1]
# 
# matrix_varcov
# 
# t <- mlvar.1$results$mu
# t$mean
# 
# matrix_varcov[1]


# ### SEPARATE VAR MODELS

# df_uk <- df_country_wide %>% filter(country_agg=="France")
# 
# # VAR
# Model1 <- VAR(df_uk[, c('pct_wear_mask_18_34', 'pct_cmnty_sick_18_34',
#                         'pct_feel_worried_ill_covid19_18_34', 'pct_ever_tested_18_34',
#                         'pct_feel_depressed_18_34')],
#               p = 7) 
# summary(Model1)
# df_uk$
#   
#   # Diagnostics
#   Serial1 <- serial.test(Model1, lags.pt = 7, type = "PT.asymptotic")
# Serial1
# Arch1 <- arch.test(Model1, lags.multi = 15, multivariate.only = TRUE)
# Arch1
# Norm1 <- normality.test(Model1, multivariate.only = TRUE)
# Norm1
# Stability1 <- stability(Model1, type = "OLS-CUSUM")
# plot(Stability1)
# 
# # Granger causality
# GrangerRRP<- causality(Model1, cause = "detrend_cases_prop")
# GrangerRRP
# GrangerM1 <- causality(Model1, cause = "detrend_pct_avoid_contact")
# GrangerM1
# 
# # Impulse Response
# 
# irf1 <- vars::irf(Model1,
#                   impulse = "pct_ever_tested_18_34",
#                   response = "pct_wear_mask_18_34", n.ahead = 20, boot = TRUE)
# plot(irf1, ylab = "pct_wear_mask_18_34")
# 
# irf2 <- vars::irf(Model1, impulse = "pct_ever_tested_18_34",
#                   response = "pct_feel_worried_ill_covid19_18_34", n.ahead = 20, boot = TRUE)
# plot(irf2, ylab = "pct_feel_worried_ill_covid19_18_34")
# 
# irf3 <- vars::irf(Model1, impulse = "pct_cmnty_sick_18_34",
#                   response = "pct_feel_depressed_18_34", n.ahead = 20, boot = TRUE)
# plot(irf3, ylab = "pct_feel_depressed_18_34")















