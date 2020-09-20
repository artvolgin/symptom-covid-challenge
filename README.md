# The COVID-19 Symptom Data Challenge

## TODO:
1. mlVAR model. Compare the effect of StringencyIndex in different states and in the US in general. Also check the efficiency of separate policies.
2. mlVAR model. Look at age related relationships. pct_cmnty_cli_18-34 causes increase in pct_cmnty_cli_55 and pct_avoid_contact_55 and other examples like this.
3. mlVAR model. Coefficent of pct_cmnty_cli --> pct_avoid_contact as measure of how people react to increase in covid cases. Coefficent of pct_avoid_contact --> pct_cmnty_cli as how their behaivour decrease new cases. Coefficent of pct_cmnty_cli --> restrictions as measure how government react to increase in covid cases. Coefficent of restrictions --> pct_cmnty_cli as how efficient were these measures.

6. mlVAR model. Take counties as the lower level of observations. If we don't have enough sample to obtain survey indicators for specific county i in time t than take the indicators of the "rest of state X" in the time t as the proxy for these indicators.

## DONE:
1. EDA of the Oxford state response data. OxCGRT_US_states_temp.csv
2. VAR model estimation
5. mlVAR model. Add time-ivariant exogenous variables into the model (gdp, population density, health-related indicators etc.). DOES NOT WORK THIS WAY

## IDEAS:
1. EDA of the household pulse survey data. https://www.census.gov/programs-surveys/household-pulse-survey/data.html (open from TOR)
2. EDA of the COVID-19 Social Media Counts & Sentiment. COVID-19_Social_Media_Counts_and_Sentiment.csv
1. We need to account for different age cohorts and gender.
3. Compare CLI-in-community with other CLI’s for states with large N to prove that they are almost perfectly correlated than we control for the sample size.
4. A lot of outliers and very high values in “mean” variables.
5. Multilevel structure with states-counties. Survey data on the second level, medical statistics, safegraph and other indicators on the second level.
6. If we are going to use safegraph data we need to remove weekly-seasonality from it.
7. County level research: look at the differences between urban and rural counties (NCHS Urban-Rural Classification Scheme for Counties).
8. Temporal correlation between “CLI-in-community” and “pct_avoid_contact” as the measure of irresponsibility.
9.	Model the overall US agenda as the proportion of new cases/growth rate of cases in the US in general. Add this variable to each district to control for the effect of federal situation on the attitude towards COVID-19.
10. Difference/correlation between avoid_contact between age buckets for each state.
11. Completely remove low-sampled states.
12. pct_cmnty_cli in the age-group 18-34 as a good predictor of the number of cases in the future. Young people are mostly spreading COVID => CIL for 55+ = CLI for 18-34 with a lag.
13. Create dashboard in Shiny.
14. How does avoid_contact affected by the introduction of different restrictions. Main question: Does people pay attetion more to the restriction or to the number of cases when adjusting their behaviour.
15. Show the usage of the method for three places: 1) County-level in Florida/California, 2) State-level for US, 3) Region-level for Western Europe (oxford data only for regions in the UK)
16. Switch from days to weeks as time intervals?
17. avoid_contact as the main variable of interest that indicate how serious people view a threat from COVID-19. Changes in that variable can be caused by different factors: 1) initial reaction to pandemic, 2) number of cases in the district, 3) number of cases in the US, 4) proportion of people who already have had COVID-19 and have no worries about it now, 5) irresponsible actions/overall mistrust towards COVID-19 threat. We should focus our attention on the 5th factor and show the districts with higher proportion of people who acts this way. The government should pay more attention to these states in terms of promoting COVID-related information and fighting against fake-news about coronavirus.



