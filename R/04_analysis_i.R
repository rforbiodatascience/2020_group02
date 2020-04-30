# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
#my_data_clean_aug %>% ...

# Model data
# ------------------------------------------------------------------------------
#my_data_clean_aug %>% ...

# Visualise data
# ------------------------------------------------------------------------------
#my_data_clean_aug %>% ...

#Visualising number of COVID-19 confirmed cases versus number of COVID-19 tests performed _MCHR001 
#To be continued...

conf_vs_test <- covid_aug %>%
  mutate(conf_pop = `Number of confirmed COVID-19`/`Population (in thousands) total`) %>% 
  mutate (dead_pop = `Number of COVID-19 related deaths`/`Population (in thousands) total`) %>% 
  filter(country %in% c("US", "China", "Spain", "France", "Denmark", "Italy", "Sweden", "Norway", "Austria", "Belgium", "United Kingdom")) %>% 
  group_by(date)

  ggplot(conf_vs_test, mapping = aes(x = date , y = conf_pop)) + 
  geom_point(aes(color = country))
  
  
  ggplot(conf_vs_test, mapping = aes(y = `Cumulative total per thousand`, x = date, na.rm = TRUE)) + 
    geom_point(aes(color = country))
  
  ggplot(conf_vs_test, mapping = aes(y = dead_pop, x = date, na.rm = TRUE)) + 
    geom_point(aes(color = country))
  


# Write data
# ------------------------------------------------------------------------------
write_tsv(...)
ggsave(...)