# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("stringr")
library("lubridate")
library("readxl")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
adult_mortality <- read_tsv(file = "data/01_adult_mortality_load.tsv")
life_expedtancy <- read_tsv(file = "data/01_life_expectancy_load.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
##WHO - mortality
#Adult mortality
adult_mortality_clean <- adult_mortality  %>% 
  filter(Year == 2016) %>% 
  select(Country, `Adult mortality rate`) 

#Adult_mortality rate corresponds to the probability of dying between age 15 and 60 per 1000 individuals

#Life expectancy and healthy life expectancy  
life_expectancy_clean <- life_expectancy  %>% 
  filter(Year == 2016) %>% 
  select(Country, `Life expectancy at birth (years)`, `Healthy life expectancy (HALE) at birth (years)`) 

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = adult_mortality_clean,
          path = "data/02_adult_mortality_clean.tsv")
write_tsv(x = life_expectancy_clean,
          path = "data/02_life_expectancy_clean.tsv")