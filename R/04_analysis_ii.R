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
levels <- c("1", "2", "3")
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv",
                      col_types = cols(thousand_deaths = col_date(),
                                       `days_from_100_cases_to_1000_deaths` = col_double(),
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
list_of_cov <- names(covid_aug)[59:91]
plot_list <- list()
for(i in list_of_cov){
  plot_name <- i
  plt <- covid_aug_by_country %>% 
    drop_na(i) %>% 
    ggplot(aes_string(x=i, y = 'days_from_100_cases_to_100_deaths')) +
    geom_boxplot() +
    theme_bw()
  plot_list[[i]] = plt
  print(plot_list[[i]])
}

for(i in list_of_cov){
  file_name = paste("results/Deaths_", i, ".png", sep="")
  png(file_name, width=8.5, height = 6.5,unit='in',res=300)
  print(plot_list[[i]])
  dev.off()
}



plot_list2 <- list()
for(i in list_of_cov){
  plot_name <- i
  plt <- covid_aug_by_country %>% 
    drop_na(i) %>% 
    ggplot(aes_string(x=i, y = 'days_from_dec1_to_100_cases')) +
    geom_boxplot() +
    theme_bw()
  plot_list2[[i]] = plt
}

for(i in list_of_cov){
  file_name = paste("results/Cases_", i, ".png", sep="")
  png(file_name, width=8.5, height = 6.5,unit='in',res=300)
  print(plot_list2[[i]])
  dev.off()
}

kruskal.test(days_from_100_cases_to_100_deaths ~ adult_mortality_rate_ter, data = covid_aug_by_country)
kruskal.test(days_from_100_cases_to_100_deaths ~ prevalence_smoking_ter, data = covid_aug_by_country)
kruskal.test(days_from_100_cases_to_100_deaths ~ respiratory_diseases_ter, data = covid_aug_by_country)
kruskal.test(days_from_100_cases_to_100_deaths ~ population_density_ter, data = covid_aug_by_country)
kruskal.test(days_from_100_cases_to_100_deaths ~ population_proportion_over_60_ter, data = covid_aug_by_country)
kruskal.test(days_from_100_cases_to_100_deaths ~ population_in_thousands_total_ter, data = covid_aug_by_country)


#Patchwork package for combining plots
plot_list2$population_proportion_over_60_ter + plot_list$population_proportion_over_60_ter


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
plot1 <- covid_aug %>%
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