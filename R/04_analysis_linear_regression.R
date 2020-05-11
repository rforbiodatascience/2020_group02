# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("patchwork")
library("modelr")
library("broom")


# Define functions
# ------------------------------------------------------------------------------
# source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
levels <- c("1", "2", "3")
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv",
                      col_types = cols(thousand_deaths = col_date(),
                                       `days_from_100_cases_to_1000_deaths` = col_double(),
                                       sex = col_factor(),
                                       adult_mortality_rate_ter = col_factor(levels = levels),
                                       concentration_fine_particles_ter = col_factor(levels = levels),
                                       bmi_above30_prevalence_all_ter = col_factor(levels = levels),
                                       current_health_expenditure_per_person_usd_ter = col_factor(levels = levels),
                                       proportion_basic_handwashing_facilities_ter = col_factor(levels = levels),
                                       current_health_expenditure_per_person_usd_ter = col_factor(levels = levels),
                                       density_of_hospitals_ter = col_factor(levels = levels),
                                       life_expectancy_ter = col_factor(levels = levels),
                                       healthy_life_expectancy_ter = col_factor(levels = levels),
                                       measles_reported_cases_ter = col_factor(levels = levels),
                                       density_of_medical_doctors_ter = col_factor(levels = levels),
                                       pollution_attributable_death_rate_ter = col_factor(levels = levels),
                                       pollution_attributable_death_rate_std_ter = col_factor(levels = levels),
                                       density_of_nurses_midwifes_ter = col_factor(levels = levels),
                                       population_in_thousands_total_ter = col_factor(levels = levels),
                                       population_proportion_under_15_ter = col_factor(levels = levels),
                                       population_proportion_over_60_ter = col_factor(levels = levels),
                                       population_median_age_years_ter = col_factor(levels = levels),
                                       population_living_in_urban_areas_ter = col_factor(levels = levels),
                                       prevalence_smoking_ter = col_factor(levels = levels),
                                       population_density_ter = col_factor(levels = levels),
                                       sex_ratio_males_per_100_females_ter = col_factor(levels = levels),
                                       population_aged_60_years_old_percentage_ter = col_factor(levels = levels),
                                       respiratory_infectious_ter = col_factor(levels = levels),
                                       malignant_neoplasms_ter = col_factor(levels = levels),
                                       cardiovascular_diseases_ter = col_factor(levels = levels),
                                       ischaemic_heart_disease_ter = col_factor(levels = levels),
                                       respiratory_diseases_ter = col_factor(levels = levels),
                                       kidney_diseases_ter = col_factor(levels = levels),
                                       road_injury_ter = col_factor(levels = levels),
                                       gdp_in_current_prices_millions_of_us_dollars_ter = col_factor(levels = levels),
                                       gdp_per_capita_us_dollars_ter = col_factor(levels = levels),
                                       cumulative_covid_test_ter = col_factor(levels = levels)))


#Which factors affect number of COVID-19 confirmed cases and COVID-19 related deaths across countries?

covid_aug_by_country <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date)) 


# Linear regression - COVID-19 cases and covariates
# ------------------------------------------------------------------------------

#Check for correlation between covariates

model1 <- lm(days_from_dec1_to_100_cases ~ life_expectancy + pollution_attributable_death_rate_std +
               current_health_expenditure_per_person_usd + population_living_in_urban_areas +
               population_aged_60_years_old_percentage + respiratory_diseases, data = covid_aug_by_country)

summary(model1)

model1_final <- lm(days_from_dec1_to_100_cases ~ life_expectancy + 
               population_living_in_urban_areas + respiratory_diseases, data = covid_aug_by_country)

summary(model1_final)
tidy(model_final)


model2 <- lm(days_from_100_cases_to_100_deaths ~ life_expectancy + pollution_attributable_death_rate_std +
               current_health_expenditure_per_person_usd + population_living_in_urban_areas +
               population_aged_60_years_old_percentage + respiratory_diseases, data = covid_aug_by_country)

summary(model2)

