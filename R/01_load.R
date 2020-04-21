# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("stringr")

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

# Wrangle data
# ------------------------------------------------------------------------------
# my_data <- my_data_raw # %>% ...

COVID_test <- COVID_test_raw # %>% ...

POP_demo <- POP_demo_raw 
POP_demo$`Population (in thousands) total` <- gsub('\\s+', '', POP_demo$`Population (in thousands) total`) 
POP_demo$`Population (in thousands) total` <- as.numeric(POP_demo$`Population (in thousands) total`) 
POP_demo$Year <- as.numeric(POP_demo$Year) 


# Write data
# ------------------------------------------------------------------------------
# write_tsv(x = my_data, path = "data/01_my_data.tsv")

write_csv(x = COVID_test,
          path = "data/01_COVID_test.csv")

write_csv(x = POP_demo,
          path = "data/01_POP_demo.csv")
