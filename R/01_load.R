# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("stringr")
library("readr")
library("lubridate")
library("readxl")


# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
#my_data_raw <- read_tsv(file = "data/_raw/my_raw_data.tsv")

##Data in our world data
#COVID-19 tests performed. Global data
COVID_test_raw <- read_csv2(file = "data/_raw/Our world in data/covid-testing-all-observations.csv")


## Johns Hopkins COVID data
# Confirmed COVID-19 cases, in time series. Global data
JH_conftime_raw <- read_csv("data/_raw/Johns Hopkins - COVID-19/time_series_covid19_confirmed_global.csv")

#Deaths due to COVID-19, in time series. Global data
JH_deadtime_raw <- read_csv("data/_raw/Johns Hopkins - COVID-19/time_series_covid19_deaths_global.csv")

#Recovered from COVID-19, in time series. Global data
JH_recotime_raw <- read_csv("data/_raw/Johns Hopkins - COVID-19/time_series_covid19_recovered_global.csv")


#UN data
UN_pop_raw <- read_csv("data/_raw/UN/SYB62_1_201907_Population, Surface Area and Density (1).csv", skip = 1)
UN_gdp_raw <- read_csv(file = "data/_raw/UN/SYB62_230_201904_GDP and GDP Per Capita.csv", col_names = FALSE, skip = 2)

#Gender-leader data
sex_leader_raw <- read_csv(file = "data/_raw/gender_leader.csv")

##WHO - mortality
#Adult mortality
adult_mortality_raw <- read_csv(file = "data/_raw/WHO/Mortality/Adult mortality.csv",  
                                col_names = c("Country", "Year", "Adult mortality rate", "Adult male mortality rate", "Adult female mortality rate"),
                                skip = 2)
## WHO -Population demographics
# Population size, median Pop age, urban distribution 
POP_demo_raw <- read_csv(file = "data/_raw/WHO/Population demographics/Population demographics_all years.csv")                       

#WHO BMI
BMI_above30_agestand_raw <- read_csv("data/_raw/WHO/BMI/NCD_BMI_above30_age_standardized.csv")

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


##WHO - public health and environment
-------------------------------------------------------------------------------------------------------
#Air pollution
air_pollution_raw <- read_csv(file = "data/_raw/WHO/Public health and environment/Air pollution.csv",
                              col_names = c("Country", "Total concentration of fine particular matter", "Urban concentration of fine particular matter", "Rural concentration of fine particular matter"),
                              skip = 3)

#Handwashing facilities
cols <- read_csv(file = "data/_raw/WHO/Public health and environment/Handwashing_facilities_percent.csv",
                 n_max = 3,
                 col_names = FALSE)

col_names <- summarise_all(cols, funs(paste(na.omit(.), collapse = "_"))) %>%
  unlist()
  

handwashing_facilities_raw <- read_csv(file = "data/_raw/WHO/Public health and environment/Handwashing_facilities_percent.csv",
                                       skip = 3,
                                       col_names = col_names) 


#Household pollution - clean fuel technologies
cols <- read_csv(file = "data/_raw/WHO/Public health and environment/Household pollution_clean fuel technologies.csv",
                 n_max = 2,
                 col_names = FALSE)

col_names <- summarise_all(cols, funs(paste(na.omit(.), collapse = "_"))) %>% 
  unlist()

household_pollution_raw <- read_csv(file = "data/_raw/WHO/Public health and environment/Household pollution_clean fuel technologies.csv",
                                    skip = 2,
                                    col_names = col_names)

#Measles reported cases
cols <- read_csv(file = "data/_raw/WHO/Public health and environment/Measles_reported_cases.csv",
                 n_max = 2,
                 col_names = FALSE)

col_names <- summarise_all(cols, funs(paste(na.omit(.), collapse = "_"))) %>% 
  unlist()

measles_cases_raw <- read_csv(file = "data/_raw/WHO/Public health and environment/Measles_reported_cases.csv",
                                    skip = 2,
                                    col_names = col_names)


#Mortality from environmental pollution
cols <- read_csv(file = "data/_raw/WHO/Public health and environment/Mortality from environmental pollution.csv",
                 n_max = 3,
                 col_names = FALSE)

col_names <- summarise_all(cols, funs(paste(na.omit(.), collapse = "_"))) %>%
  unlist()


mortality_pollution_related_raw <- read_csv(file = "data/_raw/WHO/Public health and environment/Mortality from environmental pollution.csv",
                                       skip = 3,
                                       col_names = col_names) 


##Health workforce and system
--------------------------------------------------------------------------------
#Current health expenditure

                              

# Wrangle data
# ------------------------------------------------------------------------------
# my_data <- my_data_raw # %>% ...




# Write data
# ------------------------------------------------------------------------------
# write_tsv(x = my_data, path = "data/01_my_data.tsv")

write_tsv(x = COVID_test_raw,
          path = "data/01_COVID_test.tsv")

write_tsv(x = JH_conftime_raw,
          path = "data/01_JH_conftime.tsv")

write_tsv(x = JH_deadtime_raw,
          path = "data/01_JH_deadtime.tsv")

write_tsv(x = JH_recotime_raw,
          path = "data/01_JH_recotime.tsv")

write_tsv(x = POP_demo_raw,
          path = "data/01_POP_demo.tsv")

write_tsv(x = adult_mortality_raw,
          path = "data/01_adult_mortality_load.tsv")

write_tsv(x = life_expectancy_raw,
          path = "data/01_life_expectancy_load.tsv")

write_tsv(x = UN_pop_raw,
          path = "data/01_UN_pop_raw.tsv")

write_tsv(x = UN_gdp_raw,
          path = "data/01_UN_gdp_raw.tsv")

write_tsv(x = mortality_causes_raw,
          path = "data/01_mortality_causes_load.tsv")

write_tsv(x = air_pollution_raw,
          path = "data/01_air_pollution_load.tsv")

write_tsv(x = handwashing_facilities_raw,
          path = "data/01_handwashing_facilities_load.tsv")

write_tsv(x = household_pollution_raw,
          path = "data/01_household_pollution_load.tsv")

write_tsv(x = measles_cases_raw,
          path = "data/01_measles_cases_load.tsv")

write_tsv(x = mortality_pollution_related_raw,
          path = "data/01_mortality_pollution_related_load.tsv")

write_tsv(x = sex_leader_raw,
          path = "data/01_sex_leader_raw.tsv")

write_tsv(x = BMI_above30_agestand_raw,
          path = "data/01_BMI_above30_agestand_raw.tsv")