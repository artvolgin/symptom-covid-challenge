
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
library(lme4)
# Extra
library(covidcast)
library(mlVAR)


setwd(paste0("C:/Users/", Sys.getenv("USERNAME"),
             "/YandexDisk/covid_facebook/data"))

### -------------------------- PREPROCESSING

# Load the data
df_country <- readRDS("df_country.rds")
length(unique(df_country$country_agg))
# Drop Kazakhstan due to poor data
df_country <- df_country %>% filter(country_agg != 'Kazakhstan')
max(df_country$date)
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

# df_country <- df_country[!(duplicated(df_country[, c("date", "country_agg", 'age_bucket')])),]

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
# plot(mlvar.1, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

# - 1.1 18-34
variables.1.1 <- c("pct_cmnty_sick_18_34", "stringency_index",
                   "pct_feel_worried_ill_covid19_18_34", "pct_ever_tested_18_34",
                   "pct_wear_mask_18_34", "pct_attended_public_event_18_34", "pct_worked_outside_home_18_34")
mlvar.1.1 <- mlVAR(df_country_wide,
                   vars = variables.1.1,
                   idvar = "country_agg", lags = 14,
                   temporal = "correlated", nCores = 12)
# plot(mlvar.1.1, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")

# - 1.2 55+
variables.1.2 <- c("pct_cmnty_sick_55", "stringency_index",
                   "pct_feel_worried_ill_covid19_55", "pct_ever_tested_55",
                   "pct_wear_mask_55", "pct_attended_public_event_55", "pct_worked_outside_home_55")
mlvar.1.2 <- mlVAR(df_country_wide,
                   vars = variables.1.2,
                   idvar = "country_agg", lags = 14,
                   temporal = "correlated", nCores = 12)
# plot(mlvar.1.2, vsize=12, label.cex=3, label.scale.equal=T, type="temporal", layout="circle")


# --- TESTING: Compute confidence intervals for random effects >>>>>>>>>>>>>>>>>>>>>>>


### -------------------------- PLOTTING
library(qgraph)

### --- PLOT 1. OVERALL NETWORK
mlvar.1$input$vars <- c("CLI in\nCommunity", "Stringency\nIndex", "Worried\nCOVID", "Ever\nTested",
                        "Wear\nMask", "Attended\nPublic Event", "Worked\nOutside")
# plot(mlvar.1, vsize=7, esize=7, label.cex=3.5, label.scale.equal=T,
#      type="temporal", layout="circle",
#      labels=T,
#      border.width=3,
#      border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey"),
#      color=c("gainsboro","white", "white", "white", "white", "white", "white"),
#      shape=c("square", "circle", "circle", "circle", "circle", "circle", "circle"),
#      asize=5,  edge.labels=T, edge.label.cex=1,
#      alpha=0.01)

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



### --- PLOT 1. OVERALL NETWORK, contemporaneous
# plot(mlvar.1, vsize=7, esize=7, label.cex=3.5, label.scale.equal=T,
#      type="contemporaneous", layout="circle", labels=T,
#      border.width=3,
#      border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey"),
#      color=c("gainsboro","white", "white", "white", "white", "white", "white"),
#      shape=c("square", "circle", "circle", "circle", "circle", "circle", "circle"),
#      asize=5,  edge.labels=T, edge.label.cex=1,
#      alpha=0.01)

### --- PLOT 1.1 18-34
mlvar.1.1$input$vars <- c("CLI in\nCommunity", "Stringency\nIndex", "Worried\nCOVID", "Ever\nTested",
                        "Wear\nMask", "Attended\nPublic Event", "Worked\nOutside")
# plot(mlvar.1.1, vsize=7, esize=7, label.cex=3.5, label.scale.equal=T,
#      type="temporal", layout="circle", labels=T,
#      border.width=3,
#      border.color=c("gray30","lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey", "lightgrey"),
#      color=c("gainsboro","white", "white", "white", "white", "white", "white"),
#      shape=c("square", "circle", "circle", "circle", "circle", "circle", "circle"),
#      asize=5,  edge.labels=T, edge.label.cex=1,
#      alpha=0.01)
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

# # Obtain coeficents for each country
# coef_by_countries <- list()
# for (i in 1:length(mlvar.1$IDs)){
#   temp <- melt(getNet(mlvar.1, subject=i))
#   temp$country <- mlvar.1$IDs[i]
#   coef_by_countries[[i]] <- temp
# }
# coef_by_countries <- do.call(rbind, coef_by_countries)

# # Preprocessing
# df_plot <- data.frame(
#   
#   AttendedPublicEvent.CLI = (coef_by_countries %>% filter(Var1=="Attended\nPublic Event",
#                                                       Var2=="CLI"))$value,
#   WearMask.CLI=(coef_by_countries %>% filter(Var1=="Wear\nMask",
#                                                               Var2=="CLI"))$value,
#   StringencyIndex.CLI=(coef_by_countries %>% filter(Var1=="Stringency\nIndex",
#                                                 Var2=="CLI"))$value,
#   CLI.AttendedPublicEvent = (coef_by_countries %>% filter(Var1=="CLI",
#                                                           Var2=="Attended\nPublic Event"))$value,
#   CLI.WearMask = (coef_by_countries %>% filter(Var1=="CLI",
#                                                Var2=="Wear\nMask"))$value,
#   CLI.StringencyIndex=(coef_by_countries %>% filter(Var1=="CLI",
#                                                     Var2=="Stringency\nIndex"))$value,
#   CLI.WorriedCOVID=(coef_by_countries %>% filter(Var1=="CLI",
#                                                     Var2=="Worried\nill COVID"))$value,
#   
#   country=unique(coef_by_countries$country)
# )
# 
# 
# # Plot
# ggplot(df_plot, aes(x = CLI.WorriedCOVID, y = CLI.WearMask)) +
#   geom_point(size=5, alpha=0.75) +
#   geom_text_repel(aes(label = country), size = 5) + 
#   labs(x = "CLI.WorriedCOVID",
#        y = "CLI.WearMask") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))
# 
# # Plot
# ggplot(df_plot, aes(x = CLI.AttendedPublicEvent, y = CLI.WearMask)) +
#   geom_point(size=5, alpha=0.75) +
#   geom_text_repel(aes(label = country), size = 5) + 
#   labs(x = "CLI.AttendedPublicEvent",
#        y = "CLI.WearMask") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))
# 
# # Plot
# ggplot(df_plot, aes(x = CLI.StringencyIndex, y = StringencyIndex.CLI)) +
#   geom_point(size=5, alpha=0.75) +
#   geom_text_repel(aes(label = country), size = 5) + 
#   labs(x = "CLI.StringencyIndex",
#        y = "StringencyIndex.CLI") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))
# 
# # Plot
# ggplot(df_plot, aes(x = CLI.WearMask, y = WearMask.CLI)) +
#   geom_point(size=5, alpha=0.75) +
#   geom_text_repel(aes(label = country), size = 5) + 
#   labs(x = "CLI.WearMask",
#        y = "WearMask.CLI") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))
# 
# # Plot
# ggplot(df_plot, aes(x = CLI.AttendedPublicEvent, y = AttendedPublicEvent.CLI)) +
#   geom_point(size=5, alpha=0.75) +
#   geom_text_repel(aes(label = country), size = 5) + 
#   labs(x = "CLI.AttendedPublicEvent",
#        y = "AttendedPublicEvent.CLI") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))
# 
# 

# ------------------------------------------------------- PLOT: EFFECTS ON CLI

lmer_model <- mlvar.1$output$temporal$pct_cmnty_sick

cc <- coef(lmer_model)$country_agg
cc <- cc[1:(length(mlvar.1$output$temporal)+1)]

## variances of fixed effects
fixed.vars <- diag(as.matrix(vcov(lmer_model)))
fixed.vars <- fixed.vars[1:(length(mlvar.1$output$temporal)+1)]

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
colMeans(res_full)

# Additional stats
df_stats <- df_country %>%
  group_by(country_agg) %>%
  summarize(total_cases=max(total_confirmed, na.rm=T),
            population=max(population, na.rm = T),
            gdp=max(gdp, na.rm=T),
            gdp_per_capita=max(gdp_per_capita, na.rm = T),
            urban_population=max(urban_population, na.rm=T),
            population_density=max(population_density, na.rm=T),
            human_development_index=max(human_development_index, na.rm=T)) %>%
  mutate(total_cases_prop=total_cases/population)


# Dataframe for plotting
df_plot <- res_full %>% dplyr::select(V2_mean, V5_mean, V6_mean, V7_mean, V2_se, V5_se, V6_se, V7_se)
df_plot$country_agg <- rownames(df_plot)
df_plot <- df_plot %>% rename(Stringency.CLI=V2_mean, Masks.CLI=V5_mean, Events.CLI=V6_mean, WorkedOutside.CLI=V7_mean,
                              Stringency.CLI_se=V2_se, Masks.CLI_se=V5_se, Events.CLI_se=V6_se, WorkedOutside.CLI_se=V7_se)
df_plot <- df_plot %>% left_join(df_stats)



# ---------------------------- SCATTERPLOTS

library(PerformanceAnalytics)

# Elements of the plot
hist.panel = function (x, ...) {
  par(new = TRUE)
  hist(x,
       col = "light gray",
       probability = T,
       axes = FALSE,
       main = "",
       breaks = "FD")
}
panel.cor <- function(x, y, digits=2, prefix="", use="pairwise.complete.obs",
                      method = 'pearson', cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use=use, method=method) # MG: remove abs here
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 1/strwidth(txt)
  
  test <- cor.test(x,y, method=method)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  # MG: add abs here and also include a 30% buffer for small numbers
  text(0.5, 0.5, txt, cex = cex)
  text(.8, .8, Signif, cex=cex, col=2)
}

colnames(df_plot)
df_plot_cor <- df_plot %>%
  dplyr::select(-c(Stringency.CLI_se, Masks.CLI_se,
                   Events.CLI_se, WorkedOutside.CLI_se, country_agg))
df_plot_cor <- df_plot_cor %>%
  mutate(Stringency.CLI_Masks.CLI = Stringency.CLI*Masks.CLI,
         Stringency.CLI_Events.CLI = Stringency.CLI*Events.CLI,
         Stringency.CLI_WorkedOutside.CLI = Stringency.CLI*WorkedOutside.CLI)

# # ScatterPlot 1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< SWEDEN and Stringency CLI, Poor countries and Masks CLI
# APPROVED <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
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


# # # ScatterPlot 2 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< PERU & BOLIVIA, SWEDEN. Correlation between variables
# ggplot(df_plot, aes(x = Stringency.CLI, y = Events.CLI)) +
#   geom_point(aes(size = gdp_per_capita),  color="gray40", alpha=0.75) +
#   geom_text_repel(aes(label = country_agg), size = 3) + 
#   labs(x = "Stringency.CLI",
#        y = "Events.CLI") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))
# 
# # ScatterPlot 3 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< NOTHING :(
# ggplot(df_plot, aes(x = Stringency.CLI, y = WorkedOutside.CLI)) +
#   geom_point(aes(size = gdp_per_capita),  color="gray40", alpha=0.75) +
#   geom_text_repel(aes(label = country_agg), size = 3) + 
#   labs(x = "Stringency.CLI",
#        y = "WorkedOutside.CLI") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))
# 
# # ScatterPlot 4 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< Correlation between cases and Worked Outside -> CLI
# ggplot(df_plot, aes(x = total_cases_prop, y = WorkedOutside.CLI)) +
#   geom_point(aes(size = gdp_per_capita),  color="gray40", alpha=0.75) +
#   geom_text_repel(aes(label = country_agg), size = 3) + 
#   labs(x = "total_cases_prop",
#        y = "WorkedOutside.CLI") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))
# 
# # ScatterPlot 5 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MASKS and GDP Version №2
# ggplot(df_plot, aes(x = gdp_per_capita, y = Masks.CLI)) +
#   geom_point(aes(size = total_cases_prop), alpha=0.75) +
#   geom_text_repel(aes(label = country_agg), size = 3) + 
#   labs(x = "gdp_per_capita",
#        y = "Masks.CLI") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))


# ---------------------------- ERRORBAR PLOTS

# # WEAR MASK --> CLI
# ggplot(df_plot, aes(x = reorder(country_agg, Masks.CLI), y = Masks.CLI)) + 
#   geom_errorbar(aes(ymin = Masks.CLI - 1.96*Masks.CLI_se, ymax = Masks.CLI + 1.96*Masks.CLI_se), width=0.2) +
#   geom_point(data=df_plot, mapping=aes(x=country_agg, y=Masks.CLI), size=2, shape=21, fill="black") +
#   coord_flip() + theme_minimal()
# 
# # ATTENDING EVENTS --> CLI
# ggplot(df_plot, aes(x = reorder(country_agg, Events.CLI), y = Events.CLI)) + 
#   geom_errorbar(aes(ymin = Events.CLI - 1.96*Events.CLI_se, ymax = Events.CLI + 1.96*Events.CLI_se), width=0.2) +
#   geom_point(data=df_plot, mapping=aes(x=country_agg, y=Events.CLI), size=2, shape=21, fill="black") +
#   coord_flip() + theme_minimal()

# WORKING OUTSIDE --> CLI
# ggplot(df_plot, aes(x = reorder(country_agg, WorkedOutside.CLI), y = WorkedOutside.CLI)) + 
#   geom_errorbar(aes(ymin = WorkedOutside.CLI - 1.96*WorkedOutside.CLI_se, ymax = WorkedOutside.CLI + 1.96*WorkedOutside.CLI_se), width=0.2) +
#   geom_point(data=df_plot, mapping=aes(x=country_agg, y=WorkedOutside.CLI), size=2, shape=21, fill="black") +
#   coord_flip() + theme_minimal()

# # ScatterPlot 1
# ggplot(df_plot, aes(x = Masks.CLI, y = total_cases_prop)) +
#   geom_point(size=5, alpha=0.75) +
#   geom_text_repel(aes(label = country_agg), size = 3) + 
#   labs(x = "Masks.CLI",
#        y = "total_cases_prop") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))
# 
# # ScatterPlot 2
# ggplot(df_plot, aes(x = Events.CLI, y = total_cases_prop)) +
#   geom_point(size=5, alpha=0.75) +
#   geom_text_repel(aes(label = country_agg), size = 3) + 
#   labs(x = "Events.CLI",
#        y = "total_cases_prop") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))
# 
# # ScatterPlot 3
# ggplot(df_plot, aes(x = WorkedOutside.CLI, y = total_cases_prop)) +
#   geom_point(size=5, alpha=0.75) +
#   geom_text_repel(aes(label = country_agg), size = 3) + 
#   labs(x = "WorkedOutside.CLI",
#        y = "total_cases_prop") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))


# ----------------------------PLOT: EFFECTS ON WORRIED ILL COVID

lmer_model <- mlvar.1$output$temporal$pct_feel_worried_ill_covid19_overall

cc <- coef(lmer_model)$country_agg
cc <- cc[1:(length(mlvar.1$output$temporal)+1)]

## variances of fixed effects
fixed.vars <- diag(as.matrix(vcov(lmer_model)))
fixed.vars <- fixed.vars[1:(length(mlvar.1$output$temporal)+1)]

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
colMeans(res_full)


# Dataframe for plotting
df_plot.2 <- res_full %>% dplyr::select(V1_mean, V2_mean, V1_se, V2_se, V1_signif, V2_signif)
df_plot.2$country_agg <- rownames(df_plot.2)
df_plot.2 <- df_plot.2 %>% rename(CLI.Worries=V1_mean, Stringency.Worries=V2_mean,
                                  CLI.Worries_se=V1_se, Stringency.Worries_se=V2_se,)
df_plot.2 <- df_plot.2 %>% left_join(df_stats)
df_plot.2 <- df_plot.2 %>% mutate(total_cases_prop=total_cases_prop*1000)

# df_plot.2_cor <- df_plot.2 %>%
#   dplyr::select(-c("CLI.Worries_se", "Stringency.Worries_se",
#                    "V1_signif", "V2_signif", "country_agg"))
# df_plot.2_cor <- df_plot.2_cor %>% mutate(CLI.Worries_Stringency.Worries=CLI.Worries*Stringency.Worries)

# Plotting cor matrix
# pairs(df_plot.2_cor, gap=0, lower.panel=panel.smooth,
#       upper.panel=panel.cor, diag.panel=hist.panel,
#       cex.labels = 2.1, font.labels = 2)

# ScatterPlot 6 CLI -> WORRIES AND GDP <<<<<< People in rich countries fear the coronavirus, number of cases doesn't matter
# APPROVED <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Remove the outlier for plotting the regression line
data.lm <- lm(formula = CLI.Worries ~ gdp_per_capita,
              data=df_plot.2 %>% filter(country_agg!="Switzerland"))
ggplot(df_plot.2, aes(x = gdp_per_capita, y = CLI.Worries)) +
  geom_point(aes(color = total_cases_prop), size=5, alpha=0.75, stroke=1) +
  geom_point(size = 5, alpha = 0.75, shape = 21, colour = "black") +
  scale_colour_gradient2(low = "green", high = "red") + 
  geom_text_repel(aes(label = country_agg), size = 5) + 
  labs(x = "GDP per capita",
       y = "CLI in Community  →  Worried COVID",
       color = "Cases per 1,000") + 
  geom_abline(slope = coef(data.lm)[[2]], intercept = coef(data.lm)[[1]],
              size=1, alpha=0.3, color="gray40", linetype=2) + 
  # scale_fill_continuous(palette="Greens") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title=element_text(size=24),
        legend.text=element_text(size=14),
        legend.position = c(0.1, 0.85),
        legend.title=element_text(size=14),
        text = element_text(size=20))


# ScatterPlot 7 STRINGENCY -> WORRIES AND GDP <<<<<<<<<<<<<<<< ???
# ggplot(df_plot.2, aes(x = CLI.Worries, y = Stringency.Worries)) +
#   geom_point(aes(size = gdp_per_capita), alpha=0.75) +
#   geom_text_repel(aes(label = country_agg), size = 3) + 
#   labs(x = "CLI.Worries",
#        y = "Stringency.Worries") + 
#   theme(panel.background = element_rect(fill = "white", colour = "grey50"),
#         axis.title=element_text(size=22,face="bold"))


# CLI --> WORRIES
# ggplot(df_plot.2, aes(x = reorder(country_agg, CLI.Worries), y = CLI.Worries)) + 
#   geom_errorbar(aes(ymin = CLI.Worries - 1.96*CLI.Worries_se, ymax = CLI.Worries + 1.96*CLI.Worries_se), width=0.2) +
#   geom_point(data=df_plot.2, mapping=aes(x=country_agg, y=CLI.Worries), size=2, shape=21, fill="black") +
#   coord_flip() + theme_minimal()
# # TODO: Scatterplot by GDP
# 
# # STRINGENCY --> WORRIES
# ggplot(df_plot.2, aes(x = reorder(country_agg, Stringency.Worries), y = Stringency.Worries)) + 
#   geom_errorbar(aes(ymin = Stringency.Worries - 1.96*Stringency.Worries_se, ymax = Stringency.Worries + 1.96*Stringency.Worries_se), width=0.2) +
#   geom_point(data=df_plot.2, mapping=aes(x=country_agg, y=Stringency.Worries), size=2, shape=21, fill="black") +
#   coord_flip() + theme_minimal()


# ----------------------------PLOT: EFFECT of WORRIED COVID on ATTENDED PUBLIC EVENTS


lmer_model <- mlvar.1$output$temporal$pct_attended_public_event_overall

cc <- coef(lmer_model)$country_agg
cc <- cc[1:(length(mlvar.1$output$temporal)+1)]

## variances of fixed effects
fixed.vars <- diag(as.matrix(vcov(lmer_model)))
fixed.vars <- fixed.vars[1:(length(mlvar.1$output$temporal)+1)]

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
colMeans(res_full)


# Dataframe for plotting
df_plot.3 <- res_full %>% dplyr::select(V1_mean, V2_mean, V3_mean)
df_plot.3$country_agg <- rownames(df_plot.3)
df_plot.3 <- df_plot.3 %>% rename(CLI.Events=V1_mean,
                                  Stringency.Events=V2_mean,
                                  Worried.Events=V3_mean,
                                  )
df_plot.3 <- df_plot.3 %>% left_join(df_stats)

df_plot.3_cor <- df_plot.3 %>%
  dplyr::select(-c("country_agg"))

# Plotting cor matrix
pairs(df_plot.3_cor, gap=0, lower.panel=panel.smooth,
      upper.panel=panel.cor, diag.panel=hist.panel,
      cex.labels = 2.1, font.labels = 2)

# ScatterPlot 8  Worried -> Events AND Stringency -> Events <<<<<<<<<<<<<<<< ???
ggplot(df_plot.3, aes(x = Stringency.Events, y = Worried.Events)) +
  geom_point(aes(size = gdp_per_capita), alpha=0.75) +
  geom_text_repel(aes(label = country_agg), size = 3) + 
  labs(x = "Stringency.Events",
       y = "Worried.Events") + 
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        axis.title=element_text(size=22,face="bold"))








