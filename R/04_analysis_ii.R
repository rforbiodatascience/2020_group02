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
                                       deaths_28_days_ter = col_factor(),
                                       deaths_28_days_per_100000_ter = col_factor(),
                                       adult_mortality_rate_ter = col_factor(),
                                       concentration_fine_particles_ter = col_factor(),
                                       BMI_above30_prevalence_all_ter = col_factor(),
                                       current_health_expenditure_per_person_USD_ter = col_factor(),
                                       density_of_hospitals_ter = col_factor(),
                                       life_expectancy_ter = col_factor(),
                                       density_medical_doctors_ter = col_factor(),
                                       prevalence_smoking_ter = col_factor()))


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
list_of_cov <- names(covid_aug)[60:67]
for(i in list_of_cov){
  png("results/[i].png", width=8.5,height = 6.5,unit='in',res=300)
  plt <- ggplot(covid_aug_by_country, aes_string(x=i, y = 'deaths_28_days_per_100000')) +
    geom_boxplot()
  dev.off()
  print(plt)
}



covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x = deaths_28_days_per_100000), na.rm = TRUE) +
  geom_histogram(binwidth = 2) 

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(y = deaths_28_days_per_100000, x = density_of_medical_doctors), na.rm = TRUE ) +
  geom_point(alpha = 0.5, size = 3) 

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x = density_medical_doctors_ter, y = deaths_28_days_after_100_cases)) +
  geom_boxplot()

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x = density_medical_doctors_ter, y = deaths_28_days_after_100_cases)) +
  geom_boxplot()

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(y = deaths_28_days_per_100000, x = BMI_above30_prevalence_all), na.rm = TRUE ) +
  geom_point(alpha = 0.5, size = 3) 

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x = BMI_above30_prevalence_all_ter, y = deaths_28_days_per_100000)) +
  geom_boxplot()

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(y = deaths_28_days_per_100000, x = life_expectancy), na.rm = TRUE ) +
  geom_point(alpha = 0.5, size = 3) 

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x = life_expectancy_ter, y = deaths_28_days_per_100000)) +
  geom_boxplot()

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(y = deaths_28_days_per_100000, x = current_health_expenditure_per_person_USD), na.rm = TRUE ) +
  geom_point(alpha = 0.5, size = 3)

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x = current_health_expenditure_per_person_USD_ter, y = deaths_28_days_after_100_cases)) +
  geom_boxplot()

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(y = deaths_28_days_per_100000, x = density_of_hospitals), na.rm = TRUE ) +
  geom_point(alpha = 0.5, size = 3)

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x = density_of_hospitals_ter, y = deaths_28_days_per_100000)) +
  geom_boxplot()

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(y = deaths_28_days_per_100000, x = prevalence_smoking), na.rm = TRUE ) +
  geom_point(alpha = 0.5, size = 3)

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x = prevalence_smoking_ter, y = deaths_28_days_per_100000)) +
  geom_boxplot()

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(y = deaths_28_days_after_100_cases, x = sex), na.rm = TRUE ) +
  geom_bar(stat = "identity")

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(y = deaths_28_days_per_100000, x = sex), na.rm = TRUE ) +
  geom_bar(stat = "identity")

covid_aug %>%
  group_by(country) %>% 
  filter(deaths_28_days_per_100000>0) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x = country, y = deaths_28_days_per_100000)) +
  geom_boxplot()

covid_aug %>%
  group_by(country) %>% 
  filter(deaths_28_days_per_100000>0) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x = country, y = deaths_28_days_per_100000)) +
  geom_bar(stat = "Identity")

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