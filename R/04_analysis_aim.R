# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------

library("tidyverse")


# Load data ---------------------------------------------------------------

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


# Which factors affect number of COVID-19 confirmed cases and deaths across countries? --------

#Days from December 1st to 100 cases - as a function of country
covid_aug %>%
  group_by(country) %>% 
  filter(days_from_dec1_to_100_cases != "NA") %>% 
  mutate(highlight = ifelse( country == "Denmark", "yes", "no" )) %>%
  mutate(fast_resp = ifelse(days_from_dec1_to_100_cases > 110, "yes", "no")) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x=reorder(country, -days_from_dec1_to_100_cases), y = days_from_dec1_to_100_cases, fill = highlight)) +
  geom_bar(stat = "Identity") +
  labs(title = "Spread of COVID-19", y = "Days from December 1st 2019 until 100 cases", x = "Country") +
  scale_fill_manual( values = c( "yes"="red", "no"="darkgray" ), guide = FALSE ) +
  theme(panel.background = element_rect(fill = "white"), axis.text.x=element_text(angle=40, hjust=1, vjust=1.2, margin=margin(-15,0,0,0)), axis.ticks = element_blank())
ggsave("results/04_analysis_aim/COVID-19 spread by country.png", width=22, height = 11, unit='in')


#Days from 100 cases to 100 deaths - as a function of country
covid_aug %>%
  group_by(country) %>% 
  filter(days_from_100_cases_to_100_deaths != "NA") %>% 
  mutate(highlight = ifelse( country == "Denmark", "yes", "no" )) %>% 
  slice(which.max(date)) %>% 
  ggplot(mapping = aes(x=reorder(country, -days_from_100_cases_to_100_deaths), y = days_from_100_cases_to_100_deaths, fill = highlight )) +
  geom_bar(stat = "Identity") +
  labs(title = "COVID-19 related mortality", y = "Days from 100 cases until 100 deaths", x = "Country") +
  scale_fill_manual( values = c( "yes"="red", "no"="darkgray" ), guide = FALSE ) +
  theme(panel.background = element_rect(fill = "white"), 
        axis.text.x=element_text(angle=40, hjust=1, vjust=1.4), axis.ticks = element_blank()) 
ggsave("results/04_analysis_aim/COVID-19 mortality by country.png", width=10, height = 6.5,unit='in')