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
#my_data_raw <- read_tsv(file = "data/_raw/my_raw_data.tsv")

COVID_test_raw <- read_csv2(file = "data/_raw/Our world in data/covid-testing-all-observations.csv")
POP_demo_raw <- read_csv(file = "data/_raw/WHO/Population demographics/Population demographics_all years.csv")

#UN data
UN_pop_raw <- read_csv("data/_raw/UN/SYB62_1_201907_Population, Surface Area and Density (1).csv")
UN_GDP_raw <- read_csv("data/_raw/UN/SYB62_230_201904_GDP and GDP Per Capita.csv"

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

#header names are extracted from row 7 and saved in character vector
col_names <- read_xls(path = "data/_raw/WHO/Mortality/Cause_specific_deaths.xls",
                      sheet = 2,
                      cell_rows(7)) %>% 
  names()

#Rest of data is imported, first 9 rows of metadata are skipped and column names are added                       
mortality_causes_raw <- read_xls(path = "data/_raw/WHO/Mortality/Cause_specific_deaths.xls",
                                 sheet = 2,
                                 skip = 9,
                                 col_names = col_names)

# Wrangle data
# ------------------------------------------------------------------------------
# my_data <- my_data_raw # %>% ...

COVID_test <- COVID_test_raw # %>% ...

POP_demo <- POP_demo_raw 
POP_demo$`Population (in thousands) total` <- gsub('\\s+', '', POP_demo$`Population (in thousands) total`) 
POP_demo$`Population (in thousands) total` <- as.numeric(POP_demo$`Population (in thousands) total`) 
POP_demo$Year <- as.numeric(POP_demo$Year) 

POP_demo_test <- POP_demo_raw %>% 
  rename(Population = "Population (in thousands) total") %>% 
  mutate(Pop_num = str_replace(Population, " ", ""))




# Write data
# ------------------------------------------------------------------------------
# write_tsv(x = my_data, path = "data/01_my_data.tsv")

write_tsv(x = COVID_test,
          path = "data/01_COVID_test.tsv")

write_tsv(x = POP_demo,
          path = "data/01_POP_demo.tsv")

write_tsv(x = adult_mortality_raw,
          path = "data/01_adult_mortality_load.tsv")

write_tsv(x = life_expectancy_raw,
          path = "data/01_life_expectancy_load.tsv")

write_tsv(x = mortality_causes_raw,
          path = "data/01_mortality_causes_load.tsv")
