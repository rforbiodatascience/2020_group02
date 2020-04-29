# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("readr")
library("forcats")
library("purrr")


# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
adult_mortality_clean <- read_tsv(file = "data/02_adult_mortality_clean.tsv")
air_pollution_clean <- read_tsv(file = "data/02_air_pollution_clean.tsv")
bmi_above30_clean <- read_tsv(file = "data/02_BMI_above30_clean.tsv")
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
dfs <- mget(ls(pattern = ".+_clean"))

list_of_dataframes <- replicate(22, data.frame())
for (i in seq_along(dfs)) {
  list_of_dataframes[[i]] <- anti_join(dfs[[i]], JH_conftime_clean, by = "country") %>% 
    count(country, sort = TRUE)
}

country_differences <- bind_rows(list_of_dataframes, .id = "origin") %>% 
  mutate(origin = recode(origin, "1" = "adult_mortality", "2" = "air_pollution", "3" = "bmi", "4" = "COVID_test", "5" = "handwashing_facilities",
                         "6" = "health_expenditure", "7" = "health_infrastructure", "8" = "household_pollution", "12" = "life_expectancy",
                         "13" = "measles_cases", "14" = "medical_doctors", "15" = "mortality_causes", "16" = "mortality_pollution", "17" = "nurses_midwifes",
                         "18" = "pop_demo", "19" = "sex_leader",  "20" = "smoking", "21" = "un_gdp", "22" = "un_pop")) %>% 
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


#-------------------------------------------------------------------------------
#Preparing for merging of datasets to JH - alligning var(country) to JH using country_translate()
  
adult_mortality_clean_aug <- adult_mortality_clean %>% 
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

air_pollution_clean_aug <- air_pollution_clean %>% 
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

bmi_above30_clean_aug <- bmi_above30_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

COVID_test_clean_aug <- COVID_test_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

handwashing_facilities_clean_aug <- handwashing_facilities_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

health_expenditure_clean_aug <- health_expenditure_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

health_infrastructure_clean_aug <- health_infrastructure_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

household_pollution_clean_aug <- household_pollution_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

JH_conftime_clean_aug <- JH_conftime_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

JH_deadtime_clean_aug <- JH_deadtime_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

JH_recotime_clean_aug <- JH_recotime_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

life_expectancy_clean_aug <- life_expectancy_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

measles_cases_clean_aug <- measles_cases_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

medical_doctors_clean_aug <- medical_doctors_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

mortality_causes_clean_aug <- mortality_causes_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

mortality_pollution_related_clean_aug <- mortality_pollution_related_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

mortality_pollution_related_clean_aug <- mortality_pollution_related_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

nurses_midwifes_clean_aug <- nurses_midwifes_clean %>% 
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

POP_demo_clean_aug <- POP_demo_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

sex_leader_clean_aug <- sex_leader_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

smoking_clean_aug <- smoking_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

UN_gdp_clean_aug <- UN_gdp_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

UN_pop_clean_aug <- UN_pop_clean %>%
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

#-----------------------------------------------------------------------------
#Performing left-join to JH dataset


# Write data
# ------------------------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          path = "data/03_my_data_clean_aug.tsv")
write_tsv(x = country_differences,
          path = "data/country_differences.tsv")