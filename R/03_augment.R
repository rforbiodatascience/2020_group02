# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("readr")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
#my_data_clean <- read_tsv(file = "data/02_my_data_clean.tsv")

COVID_test_clean <- read_tsv(file = "data/02_COVID_test_clean.tsv")
POP_demo_clean <- read_tsv(file = "data/02_POP_demo_clean.tsv", col_types = cols(`Population (in thousands) total` = col_double()))
JH_conftime_clean <- read_tsv(file = "data/02_JH_conftime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
JH_deadtime_clean <- read_tsv(file = "data/02_JH_deadtime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
JH_recotime_clean <- read_tsv(file = "data/02_JH_recotime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))

# Wrangle data
# ------------------------------------------------------------------------------
my_data_clean_aug <- my_data_clean # %>% ...

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          path = "data/03_my_data_clean_aug.tsv")