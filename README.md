# The COVID-19 Symptom Data Challenge

## DONE:

## IN PROGRESS:
1. VAR model estimation

## TODO:
1. EDA of the household pulse survey data. https://www.census.gov/programs-surveys/household-pulse-survey/data.html (open from TOR)
2. EDA of the Oxford state response data. OxCGRT_US_states_temp.csv
3. EDA of the COVID-19 Social Media Counts & Sentiment. COVID-19_Social_Media_Counts_and_Sentiment.csv

## NOTES:
1. We need to account for different age cohorts and gender.
2. The story about the shortage of tests based on the pct_ever_tested and pct_tested_recently.
3. Compare CLI-in-community with other CLI’s for states with large N to prove that they are almost perfectly correlated than we control for the sample size.
6. Multilevel structure with states and corresponding regions. Survey data on the second level, medical statistics, safegraph and other indicators on the second level.
7. If we are going to use safegraph data we need to remove weekly-seasonality from it.
10.	Model the overall world agenda as the proportion of new cases/growth rate of cases in the world in general. Add this variable to each state to control for the effect of world-wide panic on the attitude towards COVID-19.
11. Difference/correlation between  pct_wear_mask, pct_attended_public_event, etc. between age buckets for each country.
12. Completely remove low-sampled countries.
13. pct_cmnty_cli in the age-group 18-34 as a good predictor of the number of cases in the future. Young people are mostly spreading COVID => CIL for 55+ = CLI for 18-34 with a lag.
14. Create dashboard in Shiny.
15. How does  pct_wear_mask, pct_attended_public_event, etc. affected by the introduction of different restrictions. Main question: Does people pay attetion more to the restriction or to the number of cases when adjusting their behaviour.



