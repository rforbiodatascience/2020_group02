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
##WHO - mortality
#Adult mortality
adult_mortality_raw <- read_csv(file = "data/_raw/WHO/Mortality/Adult mortality.csv",  
                                col_names = c("Country", "Year", "Adult mortality rate", "Adult male mortality rate", "Adult female mortality rate"),
                                skip = 2)

#Life expectancy and healthy life expectancy
life_expectancy_raw <- read_csv(file = "data/_raw/WHO/Mortality/Life expectancy and healthy life expectancy.csv",
                                col_names = c("Country", "Year", "Life expectancy at birth (years)", "Male life expectancy at birth (years)", "Female life expectancy at birth (years)", "Life expectancy at age 60 (years)", "Male life expectancy at age 60 (years)", "Female life expectancy at age 60 (years)", "Healthy life expectancy (HALE) at birth (years)", "Male healthy life expectancy (HALE) at birth (years)", "Female healthy life expectancy (HALE) at birth (years)", "Healthy life expectancy (HALE) at age 60 (years)", "Male healthy life expectancy (HALE) at age 60 (years)", "Female healthy life expectancy (HALE) at age 60 (years)"),
                                skip = 2)

#Cause specific mortality
col_names <- read_xls(path = "data/_raw/WHO/Mortality/Cause_specific_deaths.xls",
                      sheet = 2,
                      cell_rows(7)) %>% 
  names()

mortality_causes_raw <- read_xls(path = "data/_raw/WHO/Mortality/Cause_specific_deaths.xls",
                      sheet = 2,
                      skip = 9,
                      col_names = col_names)


# Wrangle data
# ------------------------------------------------------------------------------




# Write data
# ------------------------------------------------------------------------------
write_tsv(x = adult_mortality_raw,
          path = "data/01_adult_mortality_load.tsv")

write_tsv(x = life_expectancy_raw,
          path = "data/01_life_expectancy_load.tsv")