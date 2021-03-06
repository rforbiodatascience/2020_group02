# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
adult_mortality_clean <- read_tsv(file = "data/02_adult_mortality_clean.tsv")
air_pollution_clean <- read_tsv(file = "data/02_air_pollution_clean.tsv")
bmi_above30_clean <- read_tsv(file = "data/02_bmi_above30_clean.tsv")
covid_test_clean <- read_tsv(file = "data/02_covid_test_clean.tsv")
handwashing_facilities_clean <- read_tsv(file = "data/02_handwashing_facilities_clean.tsv")
health_expenditure_clean <- read_tsv(file = "data/02_health_expenditure_clean.tsv")
health_infrastructure_clean <- read_tsv(file = "data/02_health_infrastructure_clean.tsv")
household_pollution_clean <- read_tsv(file = "data/02_household_pollution_clean.tsv")
jh_conftime_clean <- read_tsv(file = "data/02_jh_conftime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
jh_deadtime_clean <- read_tsv(file = "data/02_jh_deadtime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
jh_recotime_clean <- read_tsv(file = "data/02_jh_recotime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
life_expectancy_clean <- read_tsv(file = "data/02_life_expectancy_clean.tsv")
measles_cases_clean <- read_tsv(file = "data/02_measles_cases_clean.tsv")
medical_doctors_clean <- read_tsv(file = "data/02_medical_doctors_clean.tsv")
mortality_causes_clean <- read_tsv(file = "data/02_mortality_causes_clean.tsv")
mortality_pollution_related_clean <- read_tsv(file = "data/02_mortality_pollution_related_clean.tsv")
nurses_midwifes_clean <- read_tsv(file = "data/02_nurses_midwifes_clean.tsv")
pop_demo_clean <- read_tsv(file = "data/02_pop_demo_clean.tsv")
smoking_clean <- read_tsv(file = "data/02_smoking_clean.tsv")
un_gdp_clean <- read_tsv(file = "data/02_un_gdp_clean.tsv")
un_pop_clean <- read_tsv(file = "data/02_un_pop_clean.tsv")
sex_leader_clean <- read_tsv(file = "data/02_sex_leader_clean.tsv")


# Wrangle data ------------------------------------------------------------


# Anti-join for test of differences in naming of countries - Johns --------
dfs <- mget(ls(pattern = ".+_clean"))

list_of_dataframes <- replicate(22, data.frame())
for (i in seq_along(dfs)) {
  list_of_dataframes[[i]] <- anti_join(dfs[[i]], jh_conftime_clean, by = "country") %>% 
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


# Preparing for merging of datasets to JH  --------

# Aligning var(country) to JH using country_translate()
dfs_corr_countries <- dfs %>%
  map(~mutate(., country_diff = (country_translate(country))) %>% 
        mutate(country = if_else(!is.na(country_diff), country_diff, country)) %>% 
        select(-country_diff))

dfs_corr_countries <- map(dfs_corr_countries, tibble::as_tibble)
list2env(dfs_corr_countries, envir = .GlobalEnv)


# Performing left-join to JH dataset --------------------------------------
covid_join <- jh_conftime_clean %>% 
  left_join(., jh_deadtime_clean, by=c('country', "Lat", "Long", "date")) %>% 
  left_join(., jh_recotime_clean, by=c('country', "Lat", "Long", "date")) %>%
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
  left_join(., pop_demo_clean, by=c('country'))%>% 
  left_join(., sex_leader_clean, by=c('country'))%>% 
  left_join(., smoking_clean, by=c('country')) %>% 
  left_join(., un_pop_clean, by=c('country')) %>% 
  left_join(., mortality_causes_clean, by=c('country')) %>%  
  left_join(., un_gdp_clean, by=c('country')) %>%
  left_join(., covid_test_clean, by=c('country', c("date" = "Date"))) 

covid_join <- covid_join %>% 
  arrange(country, date)


# Generate outcome variables ----------------------------------------------

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
  mutate(`days_from_100_cases_to_1000_deaths` = thousand_deaths - hundred_cases) %>% 
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


dfs <- mget(ls(pattern = ".+_clean"))

#cleaning variable names 
covid_join <- covid_join %>% 
  rename_all(~str_to_lower(.)) %>% 
  rename_all(~str_replace_all(., " ", "_")) %>% 
  rename_all(~str_replace_all(., "\\(", "_")) %>% 
  rename_all(~str_replace_all(., "\\)", "_")) %>% 
  rename_all(~str_replace_all(., "%", "")) %>% 
  rename_all(~str_replace_all(., "\\+", "")) %>% 
  rename_all(~str_replace_all(., "__", "_")) %>% 
  rename_all(~str_replace_all(., "_+$", ""))


# Constructing additional variables ---------------------------------------

#changing disease cases to relative
covid_join <- covid_join %>% 
  mutate(respiratory_infectious = (respiratory_infectious/population_in_thousands_total*100)) %>% 
  mutate(malignant_neoplasms = (malignant_neoplasms/population_in_thousands_total*100)) %>% 
  mutate(cardiovascular_diseases = (cardiovascular_diseases/population_in_thousands_total*100)) %>% 
  mutate(ischaemic_heart_disease = (ischaemic_heart_disease/population_in_thousands_total*100)) %>% 
  mutate(respiratory_diseases = (respiratory_diseases/population_in_thousands_total*100)) %>% 
  mutate(kidney_diseases = (kidney_diseases/population_in_thousands_total*100)) %>% 
  mutate(road_injury = (road_injury/population_in_thousands_total*100))


#Generating tertiles of covariates
list_of_cov <- names(covid_join)[8:40]
for(i in list_of_cov) {
  new_col_name <- paste0(i, "_ter")
  covid_join <- covid_join %>% 
    mutate(!!(new_col_name) := as_factor(ntile(covid_join[i], 3)))
}


# Rendering covid_join for use in shiny app -------------------------------

df_shiny <- covid_join %>% 
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  select(country, dead_cases_per_100000, confirmed_cases_per_100000, days_from_dec1_to_100_cases, 
         days_from_100_cases_to_100_deaths, population_proportion_over_60, 
         life_expectancy, population_living_in_urban_areas, respiratory_diseases, pollution_attributable_death_rate) %>%
  rename("No. dead cases per 100.000" = dead_cases_per_100000, 
         "No. confirmed cases pr. 100.000" = confirmed_cases_per_100000, 
         "No. days from December 1st to 100 cases" = days_from_dec1_to_100_cases,
         "No. days from confirmation of 100 cases to 100 deaths " = days_from_100_cases_to_100_deaths, 
         "Proportion of population > 60 years (%)" = population_proportion_over_60,
         "Life expectancy (years)" = life_expectancy, 
         "Population living in urban areas (%)" = population_living_in_urban_areas, 
         "Respiratory diseases" = respiratory_diseases,
         "Pollution attributable death rate" = pollution_attributable_death_rate)  
  

# Write data --------------------------------------------------------------

write_tsv(x = country_differences,
          path = "data/country_differences.tsv")

write_tsv(x = covid_join,
          path = "data/03_covid_aug.tsv")

#writing to Shiny_app covid_app
write_tsv(x = covid_join,
          path = "covid_app/03_covid_aug.tsv")

write_tsv(x = df_shiny, 
          path = "covid_app/03_df_shiny_aug.tsv")