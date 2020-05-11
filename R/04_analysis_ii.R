# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("patchwork")


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


covid_aug_by_country <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date))  


# Exploratory data analyses
# ------------------------------------------------------------------------------

#Summary statistics for variables
list_of_cov <- names(covid_aug)[59:91]

#COVID-19 mortality - days from 100 cases to 100 deaths
pvalue_list1 <- list()
for(i in list_of_cov) {
  pvalue <- cuzickTest(days_from_100_cases_to_100_deaths ~ covid_aug_by_country[[i]], data = covid_aug_by_country)
  pvalue_list1[[i]] = pvalue
}

pvalue_list1_digits <- list()
for (i in list_of_cov) {
  pvalue_digits1 <- signif(pvalue_list1[[i]][["p.value"]], digits = 2)
  pvalue_list1_digits[[i]] = pvalue_digits1
}

plot_list1 <- list()
for(i in list_of_cov){
  plot_name <- i
  plt <- covid_aug_by_country %>% 
    drop_na(i) %>% 
    ggplot(aes_string(x=i, y = 'days_from_100_cases_to_100_deaths')) +
    geom_boxplot() +
    labs(y = "Days from 100 cases \n until 100 deaths") +
    annotate("text", x=2.5, y=60, label = (paste0("P for trend = ", pvalue_list1_digits[[i]])), size=4) +
    theme_bw()
  plot_list1[[i]] = plt
  print(plot_list1[[i]])
}

for(i in list_of_cov){
  file_name = paste("results/04_analysis_ii/Deaths_", i, ".png", sep="")
  png(file_name, width=8.5, height = 6.5,unit='in',res=300)
  print(plot_list1[[i]])
  dev.off()
}


#COVID-19 spread - days from December 1st 2019 to 100 cases
pvalue_list2 <- list()
for(i in list_of_cov) {
  pvalue <- cuzickTest(days_from_dec1_to_100_cases ~ covid_aug_by_country[[i]], data = covid_aug_by_country)
  pvalue_list2[[i]] = pvalue
}

pvalue_list2_digits <- list()
for (i in list_of_cov) {
  pvalue_digits2 <- signif(pvalue_list2[[i]][["p.value"]], digits = 2)
  pvalue_list2_digits[[i]] = pvalue_digits2
}

plot_list2 <- list()
for(i in list_of_cov){
  plot_name <- i
  plt <- covid_aug_by_country %>% 
    drop_na(i) %>% 
    ggplot(aes_string(x=i, y = 'days_from_dec1_to_100_cases')) +
    geom_boxplot() +
    labs(y = "Days from Dec 1st 2019 \n until 100 cases") +
    annotate("text", x=2.5, y=160, label = (paste0("P for trend = ", pvalue_list2_digits[[i]])), size=4) +
    theme_bw()
  plot_list2[[i]] = plt
  print(plot_list2[[i]])
}

for(i in list_of_cov){
  file_name = paste("results/04_analysis_ii/Cases_", i, ".png", sep="")
  png(file_name, width=8.5, height = 6.5,unit='in',res=300)
  print(plot_list2[[i]])
  dev.off()
}


#Association with gender of the national leader
kruskal.test(days_from_dec1_to_100_cases ~ sex, data = covid_aug_by_country)
kruskal.test(days_from_100_cases_to_100_deaths ~ sex, data = covid_aug_by_country)




# Patchwork package for combining plots -----------------------------------

# Descriptive plots - spread of COVID-19 - population demographics ------------------------------
png("results/04_analysis_ii/Spread of COVID-19 by population demographics.png",
    width=12, height = 8,unit='in',res=300)
plot_list2$population_median_age_years_ter + plot_list2$population_proportion_under_15_ter + 
  plot_list2$population_aged_60_years_old_percentage_ter + plot_list2$population_density_ter + 
  plot_list2$population_living_in_urban_areas_ter + plot_list2$gdp_per_capita_us_dollars_ter + 
  plot_annotation(title = 'Association between population demographics and the spread of COVID-19',
                  subtitle = 'Defined as number of days from December 1st 2019 to reaching 100 cases per country') 
dev.off()

# Descriptive plot - spread of COVID-19 - health system -------------------
png("results/04_analysis_ii/Spread of COVID-19 by capacity of health systems.png",
    width=12, height = 8,unit='in',res=300)
plot_list2$current_health_expenditure_per_person_usd_ter + plot_list2$density_of_hospitals_ter + 
  plot_list2$density_of_medical_doctors_ter + plot_list2$density_of_nurses_midwifes_ter +
  plot_annotation(title = 'Association between capacity of health systems and the spread of COVID-19',
                  subtitle = 'Defined as number of days from December 1st 2019 to reaching 100 cases per country')
dev.off()

# Descriptive plot - spread of COVID-19 - public health -------------------
png("results/04_analysis_ii/Spread of COVID-19 by public health factors.png",
    width=12, height = 8,unit='in',res=300)
plot_list2$bmi_above30_prevalence_all_ter + plot_list2$prevalence_smoking_ter + 
  plot_list2$concentration_fine_particles_ter + plot_list2$pollution_attributable_death_rate_std_ter +
  plot_annotation(title = 'Association between public health factors and the spread of COVID-19',
                  subtitle = 'Defined as number of days from December 1st 2019 to reaching 100 cases per country')
dev.off()

# Descriptive plot - spread of COVID-19 - mortality -----------------------
png("results/04_analysis_ii/Spread of COVID-19 by life expectancy and mortality.png",
    width=12, height = 10,unit='in',res=300)
(plot_list2$adult_mortality_rate_ter + plot_list2$life_expectancy_ter + 
  plot_list2$cardiovascular_diseases_ter + plot_list2$respiratory_diseases_ter) /
  (plot_list2$respiratory_infectious_ter + plot_list2$kidney_diseases_ter +
  plot_list2$malignant_neoplasms_ter + plot_list2$road_injury_ter) +
  plot_annotation(title = 'Association between life expectancy and mortality and the spread of COVID-19',
                  subtitle = 'Defined as number of days from December 1st 2019 to reaching 100 cases per country')
dev.off()

# Descriptive plots - death from COVID-19 - population demographics ------------------------------
png("results/04_analysis_ii/Death from COVID-19 by population demographics.png",
    width=12, height = 8,unit='in',res=300)
plot_list1$population_median_age_years_ter + plot_list1$population_proportion_under_15_ter + 
  plot_list1$population_aged_60_years_old_percentage_ter + plot_list1$population_density_ter + 
  plot_list1$population_living_in_urban_areas_ter + plot_list1$gdp_per_capita_us_dollars_ter + 
  plot_annotation(title = 'Association between population demographics and death from COVID-19',
                  subtitle = 'Defined as number of days from reaching 100 cases until reaching 100 deaths per country') 
dev.off()

# Descriptive plot - death from COVID-19 - health system -------------------
png("results/04_analysis_ii/Death from COVID-19 by capacity of health systems.png",
    width=12, height = 8,unit='in',res=300)
plot_list1$current_health_expenditure_per_person_usd_ter + plot_list1$density_of_hospitals_ter + 
  plot_list1$density_of_medical_doctors_ter + plot_list1$density_of_nurses_midwifes_ter +
  plot_annotation(title = 'Association between capacity of health systems and death from COVID-19',
                  subtitle = 'Defined as number of days from reaching 100 cases until reaching 100 deaths per country')
dev.off()

# Descriptive plot - death from COVID-19 - public health -------------------
png("results/04_analysis_ii/Death from COVID-19 by public health factors.png",
    width=12, height = 8,unit='in',res=300)
plot_list1$bmi_above30_prevalence_all_ter + plot_list1$prevalence_smoking_ter + 
  plot_list1$concentration_fine_particles_ter + plot_list1$pollution_attributable_death_rate_std_ter +
  plot_annotation(title = 'Association between public health factors and death from COVID-19',
                  subtitle = 'Defined as number of days from reaching 100 cases until reaching 100 deaths per country')
dev.off()

# Descriptive plot - death from COVID-19 - mortality -----------------------
png("results/04_analysis_ii/Death from COVID-19 by life expectancy and mortality.png",
    width=12, height = 10,unit='in',res=300)
(plot_list1$adult_mortality_rate_ter + plot_list1$life_expectancy_ter + 
    plot_list1$cardiovascular_diseases_ter + plot_list1$respiratory_diseases_ter) /
  (plot_list1$respiratory_infectious_ter + plot_list1$kidney_diseases_ter +
     plot_list1$malignant_neoplasms_ter + plot_list1$road_injury_ter) +
  plot_annotation(title = 'Association between life expectancy and mortality and death from COVID-19',
                  subtitle = 'Defined as number of days from reaching 100 cases until reaching 100 deaths per country')
dev.off()

