
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
                                           "new_confirmed_prop", "new_deceased_prop"),
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

### --- MODEL 1: Actions againts COVID-19

# - 1 Overall 
variables.1 <- c("pct_cmnty_sick_overall", "stringency_index",
                 "pct_feel_worried_ill_covid19_overall", "pct_ever_tested_overall",
                 "pct_wear_mask_overall", "pct_attended_public_event_overall", "pct_worked_outside_home_overall")
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
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gainsboro","white", "white", "white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle", "circle", "circle"),
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
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gainsboro","white", "white", "white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle", "circle", "circle"),
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
     border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey"),
     color=c("gainsboro","white", "white", "white", "white", "white", "white"),
     shape=c("square", "circle", "circle", "circle", "circle", "circle", "circle"),
     asize=5, edge.labels=T, edge.label.cex=0.8)


### --- PLOT 2. The Effects of the Attended Public Event and Wear Mask on CLI & Time-Clusters

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
  CLI.Worries=(coef_by_countries %>% filter(Var1=="CLI in\nCommunity",
                                            Var2=="Worried\nCOVID"))$value,
  country_agg=unique(coef_by_countries$country)
)


# Additional stats
df_stats <- df_country %>%
  group_by(country_agg) %>%
  summarize(total_cases=max(total_confirmed, na.rm=T),
            population=max(population, na.rm = T),
            gdp_per_capita=max(gdp_per_capita, na.rm = T)) %>%
  mutate(total_cases_prop=(total_cases/population)*1000)
df_plot <- df_plot %>% left_join(df_stats)


# ---------------------------- SCATTERPLOTS

### ScatterPlot 1: Stringency.CLI and Masks.CLI
ggplot(df_plot, aes(x = Stringency.CLI, y = Masks.CLI)) +
  geom_point(aes(size = gdp_per_capita), color="cadetblue", alpha=0.75) +
  geom_point(aes(size = gdp_per_capita), alpha = 0.75, shape = 21, colour = "black") +
  scale_size(range = c(0, 8)) + 
  geom_text_repel(aes(label = country_agg), size = 4) + 
  labs(x = "Stringency Index  →  CLI in Community",
       y = "Wear Mask  →  CLI in Community",
       size='GDP per capita') + 
  geom_hline(aes(yintercept=0), 
             linetype="dashed", color = "gray40") +
  geom_vline(aes(xintercept=0), 
             linetype="dashed", color = "gray40") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title=element_text(size=22),
        legend.text=element_text(size=14),
        legend.position = c(0.85, 0.85),
        legend.title=element_text(size=14),
        text = element_text(size=20))

### ScatterPlot 2: CLI.Worries and gdp_per_capita
# Remove the outlier for plotting the regression line
data.lm <- lm(formula = CLI.Worries ~ gdp_per_capita,
              data=df_plot %>% filter(country_agg!="Switzerland"))
# Plot
ggplot(df_plot, aes(x = gdp_per_capita, y = CLI.Worries)) +
  geom_point(aes(color = total_cases_prop), size=5, alpha=0.75, stroke=1) +
  geom_point(size = 5, alpha = 0.75, shape = 21, colour = "black") +
  scale_colour_gradient2(low = "green", high = "red") + 
  geom_text_repel(aes(label = country_agg), size = 5) + 
  labs(x = "GDP per capita",
       y = "CLI in Community  →  Worried COVID",
       color = "Cases per 1,000") + 
  geom_abline(slope = coef(data.lm)[[2]], intercept = coef(data.lm)[[1]],
              size=1, alpha=0.3, color="gray40", linetype=2) + 
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title=element_text(size=24),
        legend.text=element_text(size=14),
        legend.position = c(0.1, 0.85),
        legend.title=element_text(size=14),
        text = element_text(size=20))


