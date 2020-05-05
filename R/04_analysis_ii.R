# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("patchwork")
library("modelr")


# Define functions
# ------------------------------------------------------------------------------
# source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv",
                      col_types = cols(thousand_deaths = col_date(),
                                       adult_mortality_rate_ter = col_factor(),
                                       concentration_fine_particles_ter = col_factor(),
                                       bmi_above30_prevalence_all_ter = col_factor(),
                                       current_health_expenditure_per_person_usd_ter = col_factor(),
                                       proportion_basic_handwashing_facilities_ter = col_factor(),
                                       current_health_expenditure_per_person_usd_ter = col_factor(),
                                       density_of_hospitals_ter = col_factor(),
                                       life_expectancy_ter = col_factor(),
                                       healthy_life_expectancy_ter = col_factor(),
                                       measles_reported_cases_ter = col_factor(),
                                       density_of_medical_doctors_ter = col_factor(),
                                       pollution_attributable_death_rate_ter = col_factor(),
                                       pollution_attributable_death_rate_std_ter = col_factor(),
                                       density_of_nurses_midwifes_ter = col_factor(),
                                       'population__in_thousands__total_ter' = col_factor(),
                                       'population_proportion_under_15__%__ter' = col_factor(),
                                       'population_proportion_over_60__%__ter' = col_factor(),
                                       'population_median_age__years__ter' = col_factor(),
                                       'population_living_in_urban_areas__%__ter' = col_factor(),
                                       prevalence_smoking_ter = col_factor(),
                                       population_density_ter = col_factor(),
                                       'sex_ratio__males_per_100_females__ter' = col_factor(),
                                       'population_aged_60+_years_old__percentage__ter' = col_factor(),
                                       respiratory_infectious_ter = col_factor(),
                                       malignant_neoplasms_ter = col_factor(),
                                       cardiovascular_diseases_ter = col_factor(),
                                       ischaemic_heart_disease_ter = col_factor(),
                                       respiratory_diseases_ter = col_factor(),
                                       kidney_diseases_ter = col_factor(),
                                       road_injury_ter = col_factor(),
                                       'gdp_in_current_prices__millions_of_us_dollars__ter' = col_factor(),
                                       'gdp_per_capita__us_dollars__ter' = col_factor(),
                                       cumulative_covid_test_ter = col_factor()))


#Which factors affect number of COVID-19 confirmed cases and COVID-19 related deaths across countries?

covid_aug_by_country <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date))  

# Model data
# ------------------------------------------------------------------------------
#my_data_clean_aug %>% ...

# Exploratory data analyses
# ------------------------------------------------------------------------------

mean_cov <- covid_aug %>% 
  map_dbl(mean, na.rm = T) %>% 
  round(digits = 2)
mean_cov



#Summary statistics for variables
list_of_cov <- names(covid_aug)[58:70]
plot_list <- list()
for(i in list_of_cov){
  plot_name <- i
  plt <- ggplot(covid_aug_by_country, aes_string(x=i, y = 'days_from_100_cases_to_100_deaths')) +
    geom_boxplot()
  plot_list[[i]] = plt
}

for(i in list_of_cov){
  file_name = paste("results/", i, ".png", sep="")
  png(file_name, width=8.5, height = 6.5,unit='in',res=300)
  print(plot_list[[i]])
  dev.off()
}



#Deaths 28 days after first 100 cases - crude and standardized for population size - as a function of country
covid_aug %>%
  group_by(country) %>% 
  filter(deaths_28_days_per_100000>1) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x=reorder(country, deaths_28_days_per_100000), y = deaths_28_days_per_100000)) +
  geom_bar(stat = "Identity") +
  theme(axis.text.x=element_text(angle=40,hjust=1,vjust=0.5))

covid_aug %>%
  group_by(country) %>% 
  filter(deaths_28_days_after_100_cases>50) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x=reorder(country, deaths_28_days_after_100_cases), y = deaths_28_days_after_100_cases)) +
  geom_bar(stat = "Identity") +
  theme(axis.text.x=element_text(angle=40,hjust=1,vjust=0.5))


#Days from 100 cases to 100 deaths - as a function of country
covid_aug %>%
  group_by(country) %>% 
  filter(days_from_100_cases_to_100_deaths != "NA") %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x=reorder(country, -days_from_100_cases_to_100_deaths), y = days_from_100_cases_to_100_deaths)) +
  geom_bar(stat = "Identity") +
  theme(axis.text.x=element_text(angle=40,hjust=1,vjust=0.5))

#Days from December 1st to 100 cases - as a function of country
covid_aug %>%
  group_by(country) %>% 
  filter(days_from_dec1_to_100_cases != "NA") %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x=reorder(country, -days_from_dec1_to_100_cases), y = days_from_dec1_to_100_cases)) +
  geom_bar(stat = "Identity") +
  theme(axis.text.x=element_text(angle=40,hjust=1,vjust=0.5))

# Utilizing the patchwork package for plot assembly

conf_cases_vs_urban / conf_deaths_vs_urban / conf_recov_vs_urban + 
  plot_annotation(
    title = "COVID-19 confirmed cases, COVID-19 deaths, COVID-19 recovered (cummulative April 16th) vs. urbanisation in countries", 
    subtitle = "No correlation, all variables increases in highly urbanised countries"
  )

# Write data
# ------------------------------------------------------------------------------
write_tsv(...)
ggsave(...)