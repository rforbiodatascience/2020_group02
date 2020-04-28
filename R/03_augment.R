# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("readr")
library("forcats")
library("countrycode")


# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
adult_mortality_clean <- read_tsv(file = "data/02_adult_mortality_clean.tsv")
air_pollution_clean <- read_tsv(file = "data/02_air_pollution_clean.tsv")
bmi_above30_clean <- read_tsv(file = "data/02_BMI_above30_clean.tsv")
COVID_test_clean <- read_tsv(file = "data/02_COVID_test_clean.tsv")
POP_demo_clean <- read_tsv(file = "data/02_POP_demo_clean.tsv")
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
mortality_causes_clean <- read_tsv(file = "data/02_mortality_causes_clean.tsv")
mortality_pollution_related_clean <- read_tsv(file = "data/02_mortality_pollution_related_clean.tsv")
nurses_midwifes_clean <- read_tsv(file = "data/02_nurses_midwifes_clean.tsv")
POP_demo_clean <- read_tsv(file = "data/02_POP_demo_clean.tsv")
smoking_clean <- read_tsv(file = "data/02_smoking_clean.tsv")
UN_gdp_clean <- read_tsv(file = "data/02_UN_gdp_clean.tsv")
UN_pop_clean <- read_tsv(file = "data/02_UN_pop_clean.tsv")
sex_leader_clean <- read_tsv(file = "data/02_sex_leader_clean.tsv")

# Wrangle data
# ------------------------------------------------------------------------------

#Anti-join for test of differences in naming of countries - Johns Hopkins used as reference

countries_diff_adult_mortality <- adult_mortality_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_air_pollution <- air_pollution_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_bmi <- bmi_above30_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_covid_test <- COVID_test_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_handwashing_facilities <- handwashing_facilities_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_health_expenditure <- health_expenditure_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_health_infrastructure <- health_infrastructure_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_household_pollution <- household_pollution_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_JH_deadtime <- JH_deadtime_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_JH_recotime <- JH_recotime_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_life_expectancy <- life_expectancy_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_measles_cases <- measles_cases_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_medical_doctors <- medical_doctors_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_mortality_causes <- mortality_causes_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_mortality_pollution <- mortality_pollution_related_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_nurses_midwifes <- nurses_midwifes_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_pop_demo <- POP_demo_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_sex_leaders <- sex_leader_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_smoking <- smoking_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_un_gdp <- UN_gdp_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_un_pop <- UN_pop_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

#Combine countries from different sources 
country_diff_dfs <- mget(ls(pattern = "countries_diff_.+"))

countries_diffs_combined <- bind_rows(country_diff_dfs, .id = "origin") %>% 
  group_by(country) %>% 
  arrange(origin) %>% 
  slice(1) %>%  
  ungroup() %>% 
  arrange(country) %>% 
  select(origin, country)


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


#Function for alligning to Country to Johns Hopkins data
#-------------------------------------------------------------------------------


country_translate <- function(x){
con <- c("Afghanistan" ="Afghanistan", "Bolivia (Plurinational State of)"= "Bolivia", "Brunei Darussalam" = "Brunei", "Comoros" = "not in JH", "Congo" = "Congo (Brazzaville)" , 
         "Cook Islands" = "not in JH", "Côte d'Ivoire, Democratic People's Republic of Korea" = "not in JH" , "Democratic Republic of the Congo" = "Congo (Kinshasa)", 	"Iran (Islamic Republic of)" = "Iran",
         "Kiribati" = "not in JH", "Lao People's Democratic Republic" = "Laos" , "Lesotho, Marshall Islands"= "not in JH", "Micronesia (Federated States of)" = "not in JH", "Myanmar"="Burma", "Nauru" = "not in JH", "Niue" = "not in JH",
         "Palau" = "not in JH", "Republic of Korea" = "Korea, South", "Republic of Moldova" = "Moldova", "Republic of North Macedonia" = "North Macedonia", "Russian Federation"= "Russia", "Samoa" = "not in JH", 
         "Solomon Islands" = "not in JH", "Syrian Arab Republic" = "Syria", "Tajikistan" = "not in JH", "Tonga"= "not in JH", "Turkmenistan" = "not in JH", "Tuvalu"="not in JH", "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom", 
         "United Republic of Tanzania" = "Tanzania", "United States of America" = "US", "Vanuatu" = "not in JH", "Venezuela (Bolivarian Republic)" = "Venezuela", "Viet Nam" = "Vietnam") 
return(as.list(con[x]))
}


country_transverter_test <- POP_demo_clean %>%
  mutate(Country1 =(country_translate(Country))) %>% 
  mutate(Country = ifelse(!is.na(Country1),Country1,Country))
  

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          path = "data/03_my_data_clean_aug.tsv")
write_tsv(x = countries_diffs_combined,
          path = "data/country_differences.tsv")