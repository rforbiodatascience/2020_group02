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
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
##Tibble for Shiny app - see shiny app

tibble_shiny <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  select(country, dead_cases_per_100000, confirmed_cases_per_100000, `Population median age (years)`, density_of_hospitals, life_expectancy, `Population living in urban areas (%)`)



# Model data
# ------------------------------------------------------------------------------
#my_data_clean_aug %>% ...

# Visualise data
# ------------------------------------------------------------------------------

#Overview of COVID-19 confirmed cases, COVID-19 realted deaths and COVID-19 tests performed for selected countries -  see shiny app

# Is there a correlation between Covid-19 confirmed cases, deaths and recoveries and % people living in urban areas

conf_cases_vs_urban <- covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(conf_cases_vs_urban,mapping = aes(y = confirmed_cases_per_100000, x = `Population living in urban areas (%)`), na.rm = TRUE ) +
  geom_point()

conf_deaths_vs_urban <- covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(conf_deaths_vs_urban, mapping = aes(y = dead_cases_per_100000 , x = `Population living in urban areas (%)`), na.rm = TRUE) +
  geom_point()

conf_recov_vs_urban <- covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(conf_recov_vs_urban, mapping = aes(y = recov_cases_per_100000  , x = `Population living in urban areas (%)`), na.rm = TRUE) +
  geom_point()

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