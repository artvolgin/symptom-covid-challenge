
### Load the packages
# Basic
library(ggplot2)
library(ggrepel)
library(ggfortify)
library(reshape2)
library(qgraph)
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
library(lme4)
# Extra
library(covidcast)
library(mlVAR)


setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))

### -------------------------- PREPROCESSING

# Load the data
df_country <- readRDS("df_country.rds")
# Preporcessing
df_country <- df_country %>% filter(gender=="overall")
colnames(df_country) <- str_replace_all(colnames(df_country), " ", "_")

# Aggregate top 2 categories for the variables of interest
df_country <- df_country %>%
  mutate(pct_wear_mask=pct_wear_mask_half_time+pct_wear_mask_most_time,
         pct_feel_nervous=pct_feel_nervous_all_time+pct_feel_nervous_most_time+pct_feel_nervous_some_time,
         pct_feel_depressed=pct_feel_depressed_all_time+pct_feel_depressed_most_time+pct_feel_depressed_some_time,
         pct_feel_worried_ill_covid19=pct_worried_ill_covid19_very+pct_worried_ill_covid19_somewhat,
         pct_finances_worried=pct_finances_very_worried+pct_finances_somewhat_worried,
         pct_enough_toEat_worried=pct_enough_toEat_very_worried+pct_enough_toEat_somewhat_worried)

# Long to wide format
df_country_wide <- pivot_wider(df_country,
                               id_cols = c("date", "country_agg", "stringency_index", "school_closing",
                                           "workplace_closing", "cancel_public_events", "restrictions_on_gatherings",
                                           "public_transport_closing", "stay_at_home_requirements",
                                           "new_confirmed_prop", "new_deceased_prop", "new_deceased"
                                           ),
                               names_from = age_bucket,
                               # values_from = pct_wear_mask)
                               values_from = colnames(df_country)[startsWith(colnames(df_country), "pct_")])
colnames(df_country_wide) <- str_replace(colnames(df_country_wide), "-", "_")
colnames(df_country_wide) <- str_remove(colnames(df_country_wide), "\\+")
selected_vars <- colnames(df_country_wide)[3:ncol(df_country_wide)]


# Plots for Sweden, Peru and Iraq
minmax_normalize <- function(x)
{
  return((x- min(x, na.rm=T)) /(max(x, na.rm=T)-min(x, na.rm=T)))
}


countries_to_plot <- c("Sweden", "Peru", "Iraq")
for (country_name in countries_to_plot){
  
  sweden <- df_country_wide %>% filter(country_agg==country_name) %>%
    dplyr::select(date, pct_cli_overall, pct_wear_mask_overall,
                  stringency_index) %>%
    mutate(pct_cli_overall=minmax_normalize(pct_cli_overall),
           pct_wear_mask_overall=minmax_normalize(pct_wear_mask_overall),
           stringency_index=minmax_normalize(stringency_index))
  
  sweden <- gather(sweden, group, value, -date)
  plot(ggplot(sweden, aes(x = date, y = value)) + 
         geom_line(aes(color = group, linetype = group), size = 1.5) +
         scale_color_manual(values = c("gray40", 
                                       "coral2",
                                       "dodgerblue2")) +
         scale_linetype_manual(values=c("dashed", "solid", "solid")) +
         theme_classic() + 
         labs(x = "Date",
              y = "Normalized Value",
              title = country_name) + # <<< Remove later
         theme(legend.position = "none",
               axis.title=element_text(size=18,face="bold"),
               axis.text=element_text(size=12)))
  
  print(country_name)
  
}


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

# - 1 Overall 
variables.1 <- c("pct_cmnty_sick_overall", "stringency_index",
                 "pct_feel_worried_ill_covid19_overall", "pct_ever_tested_overall",
                 "pct_wear_mask_overall", "pct_attended_public_event_overall",
                 "pct_worked_outside_home_overall")
mlvar.1 <- mlVAR(df_country_wide,
                 vars = variables.1,
                 idvar = "country_agg", lags = 14,
                 temporal = "correlated", nCores = 12)

# - 1.1 18-34
variables.1.1 <- c("pct_cmnty_sick_18_34", "stringency_index",
                   "pct_feel_worried_ill_covid19_18_34", "pct_ever_tested_18_34",
                   "pct_wear_mask_18_34", "pct_attended_public_event_18_34", "pct_worked_outside_home_18_34")
mlvar.1.1 <- mlVAR(df_country_wide,
                   vars = variables.1.1,
                   idvar = "country_agg", lags = 14,
                   temporal = "correlated", nCores = 12)

# - 1.2 55+
variables.1.2 <- c("pct_cmnty_sick_55", "stringency_index",
                   "pct_feel_worried_ill_covid19_55", "pct_ever_tested_55",
                   "pct_wear_mask_55", "pct_attended_public_event_55", "pct_worked_outside_home_55")
mlvar.1.2 <- mlVAR(df_country_wide,
                   vars = variables.1.2,
                   idvar = "country_agg", lags = 14,
                   temporal = "correlated", nCores = 12)


# --- Compute confidence intervals for random effects 

lmer_model <- mlvar.1$output$temporal$pct_cmnty_sick_overall

cc <- coef(lmer_model)$country_agg
cc <- cc[1:8]

## variances of fixed effects
fixed.vars <- diag(vcov(lmer_model))
fixed.vars <- fixed.vars[1:8]

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
res_low <- res_mean - (1.96*res_se)
res_high <- res_mean + (1.96*res_se)
res_signif <- as.data.frame(sign(res_low) == sign(res_high))
colnames(res_signif) <- paste0("V", as.character(c(1:ncol(res_signif))), "_signif")
colSums(res_signif)
res_full <- cbind(res_mean, res_se, res_signif)

### -------------------------- NETWORK PLOTS


### --- PLOT 1. OVERALL NETWORK
mlvar.1$input$vars <- c("CLI in\nCommunity", "Stringency\nIndex", "Worried\nCOVID", "Ever\nTested",
                         "Wear\nMask", "Attended\nPublic Event", "Worked\nOutside")

plot(mlvar.1,
     vsize=10,
     esize=5,
     label.prop=0.22,
     label.cex=6,
     usePCH=F,
     alpha=0.01,
     node.resolution=300,
     label.scale.equal=T,
     layoutScale=c(0.8,0.8),
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color="gray45",
     color=c("white","gainsboro", "white", "white", "white", "white", "white"),
     shape="circle",
     asize=5,  edge.labels=T, edge.label.cex=0.8)


### --- PLOT 1.1 18-34
mlvar.1.1$input$vars <- c("CLI in\nCommunity", "Stringency\nIndex", "Worried\nCOVID", "Ever\nTested",
                        "Wear\nMask", "Attended\nPublic Event", "Worked\nOutside")
plot(mlvar.1.1,
     vsize=10,
     esize=5,
     label.prop=0.22,
     label.cex=6,
     usePCH=F,
     alpha=0.01,
     node.resolution=300,
     label.scale.equal=T,
     layoutScale=c(0.8,0.8),
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color="gray45",
     color=c("white","gainsboro", "white", "white", "white", "white", "white"),
     shape="circle",
     asize=5, edge.labels=T, edge.label.cex=0.8)

### --- PLOT 1.2 55+
mlvar.1.2$input$vars <- c("CLI in\nCommunity", "Stringency\nIndex", "Worried\nCOVID", "Ever\nTested",
                          "Wear\nMask", "Attended\nPublic Event", "Worked\nOutside")
plot(mlvar.1.2,
     vsize=10,
     esize=5,
     label.prop=0.22,
     label.cex=6,
     usePCH=F,
     alpha=0.01,
     node.resolution=300,
     label.scale.equal=T,
     layoutScale=c(0.8,0.8),
     type="temporal", layout="circle", labels=T,
     border.width=3,
     border.color="gray45",
     color=c("white","gainsboro", "white", "white", "white", "white", "white"),
     shape="circle",
     asize=5, edge.labels=T, edge.label.cex=0.8)


### --- PLOT 2. Stringency -> CLI and  Masks -> CLI by countries

# Obtain coeficents for each country
coef_by_countries <- list()
for (i in 1:length(mlvar.1$IDs)){
  temp <- melt(getNet(mlvar.1, subject=i))
  temp$country <- mlvar.1$IDs[i]
  coef_by_countries[[i]] <- temp
}
coef_by_countries <- do.call(rbind, coef_by_countries)


# Preprocessing
df_plot <- data.frame(
  Masks.CLI=(coef_by_countries %>% filter(Var1=="Wear\nMask",
                                             Var2=="CLI in\nCommunity"))$value,
  Stringency.CLI=(coef_by_countries %>% filter(Var1=="Stringency\nIndex",
                                                Var2=="CLI in\nCommunity"))$value,
  Event.CLI=(coef_by_countries %>% filter(Var1=="Attended\nPublic Event",
                                                  Var2=="CLI in\nCommunity"))$value,
  country_agg=unique(coef_by_countries$country)
)


# Add SE of coefficents to the dataset
res_full$country_agg <- rownames(res_full)
df_plot <- df_plot %>% left_join(res_full %>%
                                   dplyr::select(country_agg,
                                                 V2_se, V5_se,
                                                 V2_signif, V5_signif) %>%
                                   rename(Stringency.CLI.se=V2_se,
                                          Masks.CLI.se=V5_se,
                                          Stringency.CLI.signif=V2_signif,
                                          Masks.CLI.signif=V5_signif))

# Plotting
df_plot.together <- df_plot %>% filter(Stringency.CLI.signif | Masks.CLI.signif)
df_plot.together <- df_plot.together %>% arrange(Masks.CLI)

ggplot(df_plot.together, aes(x = reorder(country_agg, Masks.CLI),
                             y = Masks.CLI)) + 
  
  geom_errorbar(aes(ymin = Masks.CLI-(Masks.CLI.se*1.96),
                    ymax = Masks.CLI+(Masks.CLI.se*1.96)),
                width=0.5, color=adjustcolor("brown2", alpha.f = 0.8)) + 
  geom_point(data=df_plot.together, mapping=aes(x=country_agg, y=Masks.CLI),
             size=2, color=adjustcolor("brown2", alpha.f = 0.8)) +
  
  
  geom_errorbar(data = df_plot.together, mapping=aes(ymin = Stringency.CLI-(Stringency.CLI.se*1.96),
                    ymax = Stringency.CLI+(Stringency.CLI.se*1.96)),
                width=0.5, color=adjustcolor("cornflowerblue", alpha.f = 0.8)) + 
  geom_point(data=df_plot.together, mapping=aes(x=country_agg, y=Stringency.CLI),
             size=2, color=adjustcolor("cornflowerblue", alpha.f = 0.8)) +
  
  labs(x = "",
       y = "Coefficient") +
  
  geom_hline(aes(yintercept=0), 
             linetype="dashed", color = "gray40") +
  coord_flip() + theme_minimal() + 
  theme(axis.title=element_text(size=24),
        legend.text=element_text(size=16),
        # legend.position = c(0.9, 0.1),
        legend.title=element_text(size=16),
        text = element_text(size=20),
        axis.text.y = element_text(face = ifelse(df_plot.together$country_agg %in% c("Sweden", "Peru", "Iraq"),
                                                 "bold", "plain"),
                                   size =ifelse(df_plot.together$country_agg %in% c("Sweden", "Peru", "Iraq"),
                                                16, 12)))
