# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("readr")
library("countrycode")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------

adult_mortality_clean <- read_tsv(file = "data/02_adult_mortality_clean.tsv")
air_pollution_clean <- read_tsv(file = "data/02_air_pollution_clean.tsv")
bmi_above30_clean <- read_tsv(file = "data/02_bmi_above30_clean.tsv")
COVID_test_clean <- read_tsv(file = "data/02_COVID_test_clean.tsv")
handwashing_facilities_clean <- read_tsv(file = "data/02_handwashing_facilities_clean.tsv")
health_expenditure_clean <- read_tsv(file = "data/02_health_expenditure_clean.tsv")
health_infrastructure_clean <- read_tsv(file = "data/02_health_infrastructure_clean.tsv")
household_pollution_clean <- read_tsv(file = "data/02_household_pollution_clean.tsv")
JH_conftime_clean <- read_tsv(file = "data/02_JH_conftime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
JH_deadtime_clean <- read_tsv(file = "data/02_JH_deadtime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
JH_recotime_clean <- read_tsv(file = "data/02_JH_recotime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
life_expectancy_clean <- read_tsv(file = "data/02_life_expectancy_clean.tsv")
measles_cases_clean <- read_tsv(file = "data/02_measles_cases_clean.tsv")
medical_doctors_clean <- read_tsv(file = "data/02_medical_doctors_clean.tsv")
mortality_pollution_related_clean <- read_tsv(file = "data/02_mortality_pollution_related_clean.tsv")
nurses_midwifes_clean <- read_tsv(file = "data/02_nurses_midwifes_clean.tsv")
POP_demo_clean <- read_tsv(file = "data/02_POP_demo_clean.tsv")
smoking_clean <- read_tsv(file = "data/02_smoking_clean.tsv")
UN_gdp_clean <- read_tsv(file = "data/02_UN_gdp_clean.tsv")
UN_pop_clean <- read_tsv(file = "data/02_UN_pop_clean.tsv")

# Wrangle data
# ------------------------------------------------------------------------------

#Anti-join for test
JH_conftime_clean <- JH_conftime_clean %>% 
  rename('Country' = 'Country/Region')

extra_countries_POP_demo <- POP_demo_clean %>% 
  anti_join(JH_conftime_clean, by = 'Country') %>% 
  count(Country, sort = T)

extra_countries_JH <- JH_conftime_clean %>% 
  anti_join(POP_demo_clean, by = 'Country') %>% 
  count(Country, sort = T)

#WHO datasets antijoin
WHO_extra_countries <- POP_demo_clean %>% 
  rename(country = Country) %>% 
  anti_join(smoking_clean, by = 'country') %>% 
  count(country, sort = T)


#WHO-UN datasets antijoin
UN_pop_clean <- UN_pop_clean %>% 
  rename('Country' = 'Country_Region')

WHO_UN_extra_countries <- POP_demo_clean %>% 
  anti_join(UN_pop_clean, by = 'Country') %>% 
  count(Country, sort = T)


#UN JH datasets antijoin
UN_extra_countries <- UN_pop_clean %>% 
  anti_join(JH_conftime_clean, by = 'Country') %>% 
  count(Country, sort = T)


#Function for country corrections
country.translate <- function(x, y) {
if (y == 'WHO') {
  WHO_country <- list(
    'Argentinna' = 'Argentina',
    'Republic of Laos' = 'Laos',
    'Viet Nam' = 'Vietnam')
  return(WHO_country[x])
}
else if (y == 'UN') {
  UN_country <- list(
    'Arg' = 'Argentina',
    'Lao' = 'Laos',
    'Viet Nam' = 'Vietnam')
  return(UN_country[x])
}
else if (y == 'ourworld') {
  ourworld_country <- list(
    'Argen' = 'Argentina',
    'Lao Republic' = 'Laos',
    'Viet_Nam' = 'Vietnam')
  return(ourworld_country[x])
}
else 
  print("please enter type of dataset (WHO/UN/ourworld)")
  }
  
country.translate('Arg', 'UN')


as.keyvalue(country, Argentina, UN)
is.keyvalue(ex)




# Write data
# ------------------------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          path = "data/03_my_data_clean_aug.tsv")