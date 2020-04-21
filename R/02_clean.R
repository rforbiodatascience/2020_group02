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
# my_data <- read_tsv(file = "data/01_my_data.tsv")

COVID_test <- read_csv(file = "data/01_COVID_test.csv")
POP_demo <- read_csv(file = "data/01_POP_demo.csv")

# Wrangle data
# ------------------------------------------------------------------------------
#my_data_clean <- my_data # %>% ...

COVID_test_clean <- COVID_test %>%
  rename("Country" = "Entity") %>% 
  select(Country, Date, `Cumulative total`, `Cumulative total per thousand`)

POP_demo_clean <- POP_demo %>% 
  select(-`Population living on &lt;$1 (PPP int. $) a day (%)`) %>%
 filter(Year %in% c("2020", "2013", "2016"))

UN_pop_demo_clean <- UN_pop_raw %>%
    select(X2, Year, Series, Value) %>%
  rename("Country_Region" = "X2") %>%
  filter(Year == 2019, Series == "Population density" | Series == "Sex ratio (males per 100 females)" | Series == "Population aged 60+ years old (percentage)") %>%
  pivot_wider(names_from = Series, values_from = Value)

  
# Write data
# ------------------------------------------------------------------------------
#write_tsv(x = my_data_clean, path = "data/02_my_data_clean.tsv")
