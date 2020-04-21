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
# my_data <- read_tsv(file = "data/01_my_data.tsv")

COVID_test <- read_tsv(file = "data/01_COVID_test.tsv")
POP_demo <- read_tsv(file = "data/01_POP_demo.tsv")

adult_mortality <- read_tsv(file = "data/01_adult_mortality_load.tsv")
life_expectancy <- read_tsv(file = "data/01_life_expectancy_load.tsv")
mortality_causes <- read_tsv(file = "data/01_mortality_causes_load.tsv")

UN_pop <- read_tsv(file = "data/01_UN_pop_raw.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
#my_data_clean <- my_data # %>% ...

#Data in our world data
COVID_test_clean <- COVID_test %>%
  rename("Country" = "Entity") %>% 
  select(Country, Date, `Cumulative total`, `Cumulative total per thousand`)

## WHO -Population demographics
# Population size, median Pop age, urban distribution
POP_demo_clean <- POP_demo %>% 
  select(-`Population living on &lt;$1 (PPP int. $) a day (%)`) %>%
 filter(Year %in% c("2020", "2013", "2016")) 
#skal merges ind i ovenstående - kommer
POP_demo_clean <- POP_demo %>% 
  rename(Population = `Population (in thousands) total`) %>% 
  mutate(Pop_num = str_replace(Population, " ", "")) 


#UN datasets
UN_pop_clean <- UN_pop %>%
    select(X2, Year, Series, Value) %>%
  rename("Country_Region" = "X2") %>%
  filter(Year == 2019, Series == "Population density" | Series == "Sex ratio (males per 100 females)" | Series == "Population aged 60+ years old (percentage)") %>%
  pivot_wider(names_from = Series, values_from = Value)

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


#Cause specific mortality
mortality_causes <- read_tsv(file = "data/01_mortality_causes_load.tsv") 

mortality_causes_clean <- mortality_causes %>% 
  as_tibble(mortality_causes_clean) %>% 
  rename(Cause_1 = "...5", Cause_2 = "...6") %>% 
  mutate(Cause_1_chr = str_replace(Cause_1, "\\d+\\.", ""))

mortality_causes

# Write data
# ------------------------------------------------------------------------------
#write_tsv(x = my_data_clean, path = "data/02_my_data_clean.tsv")

write_tsv(x = adult_mortality_clean,
          path = "data/02_adult_mortality_clean.tsv")
write_tsv(x = life_expectancy_clean,
          path = "data/02_life_expectancy_clean.tsv")
