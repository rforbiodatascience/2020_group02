# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("readr")
library("forcats")
library("purrr")
library("survivalAnalysis")

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


#-------------------------------------------------------------------------------
#Preparing for merging of datasets to JH - alligning var(country) to JH using country_translate()
dfs_corr_countries <- dfs %>%
  map(~mutate(., country_diff = (country_translate(country))) %>% 
        mutate(country = if_else(!is.na(country_diff), country_diff, country)) %>% 
        select(-country_diff))

dfs_corr_countries <- map(dfs_corr_countries, tibble::as_tibble)
list2env(dfs_corr_countries, envir = .GlobalEnv)


#-----------------------------------------------------------------------------
#Performing left-join to JH dataset

covid_join <- JH_conftime_clean %>% 
  left_join(., JH_deadtime_clean, by=c('country', "Lat", "Long", "date")) %>% 
  left_join(., JH_recotime_clean, by=c('country', "Lat", "Long", "date")) %>%
  left_join(., adult_mortality_clean, by=c('country')) %>% 
  left_join(., air_pollution_clean, by=c('country')) %>% 
  left_join(., bmi_above30_clean, by=c('country')) %>%
  left_join(., handwashing_facilities_clean, by=c('country')) %>% 
  left_join(., health_expenditure_clean, by=c('country')) %>% 
  left_join(., health_infrastructure_clean, by=c('country')) %>% 
  left_join(., life_expectancy_clean, by=c('country')) %>%
  left_join(., measles_cases_clean, by=c('country')) %>% 
  left_join(., medical_doctors_clean, by=c('country')) %>% 
  left_join(., mortality_pollution_related_clean, by=c('country')) %>% 
  left_join(., nurses_midwifes_clean, by=c('country')) %>% 
  left_join(., POP_demo_clean, by=c('country'))%>% 
  left_join(., sex_leader_clean, by=c('country'))%>% 
  left_join(., smoking_clean, by=c('country')) %>% 
  left_join(., UN_pop_clean, by=c('country')) %>% 
  left_join(., mortality_causes_clean, by=c('country')) %>%  
  left_join(., UN_gdp_clean, by=c('country')) %>%
  left_join(., COVID_test_clean, by=c('country', c("date" = "Date"))) 

covid_join <- covid_join %>% 
  arrange(country, date)

#Generate outcome variables
#------------------------------------------------------------------------------

covid_join <- covid_join %>% 
  mutate(confirmed_cases_per_100000 = (`Number of confirmed COVID-19`/(`Population (in thousands) total`/100))) %>%
  mutate(confirmed_cases_per_100000 = round(confirmed_cases_per_100000, 2)) %>% 
  mutate(dead_cases_per_100000 = `Number of COVID-19 related deaths`/(`Population (in thousands) total`/100)) %>%  
  mutate(dead_cases_per_100000 = round(dead_cases_per_100000, 2)) %>%         
  mutate(test_cases_per_100000 = `cumulative_covid_test`/(`Population (in thousands) total`/100)) %>% 
  mutate(test_cases_per_100000 = round(test_cases_per_100000, 1)) %>% 
  mutate(recov_cases_per_100000 = `Recovered from COVID-19 (no.)`/(`Population (in thousands) total`/100)) %>% 
  mutate(recov_cases_per_100000 = round(recov_cases_per_100000, 2))
  
first_case_by_country <- covid_join %>% 
  group_by(country) %>% 
  filter(`Number of confirmed COVID-19` > 0) %>%
  arrange(date) %>% 
  summarise(first_case=head(date,1))

hundred_cases_by_country <- covid_join %>% 
  group_by(country) %>% 
  filter(`Number of confirmed COVID-19` > 100) %>%
  arrange(date) %>% 
  summarise(hundred_cases=head(date,1))

thousand_cases_by_country <- covid_join %>% 
  group_by(country) %>% 
  filter(`Number of confirmed COVID-19` > 1000) %>%
  arrange(date) %>% 
  summarise(thousand_cases=head(date,1))

first_death_by_country <- covid_join %>% 
  group_by(country) %>% 
  filter(`Number of COVID-19 related deaths` > 0) %>%
  arrange(date) %>% 
  summarise(first_death=head(date,1))

hundred_deaths_by_country <- covid_join %>% 
  group_by(country) %>% 
  filter(`Number of COVID-19 related deaths` > 100) %>%
  arrange(date) %>% 
  summarise(hundred_deaths=head(date,1))

thousand_deaths_by_country <- covid_join %>% 
  group_by(country) %>% 
  filter(`Number of COVID-19 related deaths` > 1000) %>%
  arrange(date) %>% 
  summarise(thousand_deaths=head(date,1))


covid_join <- covid_join %>% 
  left_join(., first_case_by_country, by=c('country')) %>% 
  left_join(., first_death_by_country, by=c('country')) %>%
  left_join(., hundred_cases_by_country, by=c('country')) %>% 
  left_join(., hundred_deaths_by_country, by=c('country')) %>% 
  left_join(., thousand_cases_by_country, by=c('country')) %>% 
  left_join(., thousand_deaths_by_country, by=c('country')) 

covid_join <- covid_join %>% 
  mutate(days_to_hundred_cases = hundred_cases - first_case) %>% 
  mutate(days_to_thousand_cases = thousand_cases - first_case) %>% 
  mutate(days_from_100_cases_to_100_deaths = hundred_deaths - hundred_cases) %>% 
  mutate(days_from_dec1_to_100_cases = hundred_cases - ymd(20191201)) %>% 
  mutate(date_28_days_after_100_cases = hundred_cases + 28) 

deaths_28_days_after_100_cases_by_country <- covid_join %>% 
  group_by(country) %>% 
  filter(date >= date_28_days_after_100_cases) %>% 
  arrange(date) %>% 
  summarise(deaths_28_days_after_100_cases = head(`Number of COVID-19 related deaths`,1))
  
covid_join <- covid_join %>% 
  left_join(., deaths_28_days_after_100_cases_by_country, by=c('country')) %>% 
  mutate(deaths_28_days_per_100000 = deaths_28_days_after_100_cases/(`Population (in thousands) total`/100)) %>% 
  mutate(deaths_28_days_per_100000 = round(deaths_28_days_per_100000, 2))

#Generating tertiles of covariates
covid_join <- covid_join %>% 
  mutate(deaths_28_days_ter = as_factor(ntile(deaths_28_days_after_100_cases, 3))) %>% 
  mutate(deaths_28_days_per_100000_ter = as_factor(ntile(deaths_28_days_per_100000, 3))) %>% 
  mutate(adult_mortality_rate_ter = as_factor(ntile(adult_mortality_rate, 3))) %>% 
  mutate(concentration_fine_particles_ter = as_factor(ntile(concentration_fine_particles, 3))) %>%
  mutate(BMI_above30_prevalence_all_ter = as_factor(ntile(BMI_above30_prevalence_all, 3))) %>% 
  mutate(current_health_expenditure_per_person_USD_ter = as_factor(ntile(current_health_expenditure_per_person_USD, 3))) %>% 
  mutate(density_of_hospitals_ter = as_factor(ntile(density_of_hospitals, 3))) %>% 
  mutate(life_expectancy_ter = fct_relevel(as_factor(ntile(life_expectancy, 3)), sort)) %>% 
  mutate(density_medical_doctors_ter = as_factor(ntile(density_of_medical_doctors, 3))) %>% 
  mutate(prevalence_smoking_ter = as_factor(ntile(prevalence_smoking, 3))) 

#cleaning variable names 
covid_join <- covid_join %>% 
  rename_all(~str_to_lower(.)) %>% 
  rename_all(~str_replace_all(., " ", "_"))
  

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = country_differences,
          path = "data/country_differences.tsv")

write_tsv(x = covid_join,
          path = "data/03_covid_aug.tsv")
