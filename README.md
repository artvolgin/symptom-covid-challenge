# The COVID-19 Symptom Data Challenge
Current agenda: avoid_contact as the main variable of interest that indicate how serious people view a threat from COVID-19. Changes in that variable can be caused by different factors: 1) initial reaction to pandemic, 2) number of cases in the district, 3) number of cases in the US, 4) proportion of people who already have had COVID-19 and have no worries about it now, 5) irresponsible actions/overall mistrust towards COVID-19 threat. We should focus our attention on the 5th factor and show the districts with higher proportion of people who acts this way. The government should pay more attention to these states in terms of promoting COVID-related information and fighting against fake-news about coronavirus.


## DONE:
1. EDA of the Oxford state response data. OxCGRT_US_states_temp.csv
1. VAR model estimation

## IN PROGRESS:
- T

## TODO:
1. EDA of the household pulse survey data. https://www.census.gov/programs-surveys/household-pulse-survey/data.html (open from TOR)
3. EDA of the COVID-19 Social Media Counts & Sentiment. COVID-19_Social_Media_Counts_and_Sentiment.csv

## NOTES:
1. We need to account for different age cohorts and gender.
2. The story about the shortage of tests based on the pct_tested_no_result and pct_could_not_get_tested indicators.
3. Compare CLI-in-community with other CLI’s for states with large N to prove that they are almost perfectly correlated than we control for the sample size.
4. A lot of outliers and very high values in “mean” variables.
6. Multilevel structure with states-counties. Survey data on the second level, medical statistics, safegraph and other indicators on the second level.
7. If we are going to use safegraph data we need to remove weekly-seasonality from it.
8. County level research: look at the differences between urban and rural counties (NCHS Urban-Rural Classification Scheme for Counties).
9. Temporal correlation between “CLI-in-community” and “pct_avoid_contact” as the measure of irresponsibility.
10.	Model the overall US agenda as the proportion of new cases/growth rate of cases in the US in general. Add this variable to each district to control for the effect of federal situation on the attitude towards COVID-19.
11. Difference/correlation between avoid_contact between age buckets for each state.
12. Completely remove low-sampled states.
13. pct_cmnty_cli in the age-group 18-34 as a good predictor of the number of cases in the future. Young people are mostly spreading COVID => CIL for 55+ = CLI for 18-34 with a lag.
14. Create dashboard in Shiny.
15. How does avoid_contact affected by the introduction of different restrictions. Main question: Does people pay attetion more to the restriction or to the number of cases when adjusting their behaviour.
16. Show the usage of method for three places: 1) County-level in Florida/California, 2) State-level for US, 3) Region-level for Western Europe
17. Switch from days to weeks as time intervals?


