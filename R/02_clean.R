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

JH_conftime <- read_tsv(file = "data/01_JH_conftime.tsv")
JH_deadtime <- read_tsv(file = "data/01_JH_deadtime.tsv")
JH_recotime <- read_tsv(file = "data/01_JH_recotime.tsv")

POP_demo <- read_tsv(file = "data/01_POP_demo.tsv")
adult_mortality <- read_tsv(file = "data/01_adult_mortality_load.tsv")
life_expectancy <- read_tsv(file = "data/01_life_expectancy_load.tsv")
mortality_causes <- read_tsv(file = "data/01_mortality_causes_load.tsv")

UN_pop <- read_tsv(file = "data/01_UN_pop_raw.tsv")
UN_gdp <- read_tsv(file = "data/01_UN_gdp_raw.tsv")

sex_leader <- read_tsv(file = "data/01_sex_leader_raw.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
#my_data_clean <- my_data # %>% ...

##Data in our world data
#COVID-19 tests performed. Global data

COVID_test_clean <- COVID_test %>%
  rename("Country" = "Entity") %>% 
  select(Country, Date, `Cumulative total`, `Cumulative total per thousand`)

#This dont work on the dataset - MCHR001 is working on this
#COVID_test_clean$Date <- as.Date(COVID_test_clean$Date, "%d-%m-%y")


## WHO -Population demographics
# Population size, median Pop age, urban distribution

POP_demo_clean <- POP_demo %>%  
  mutate(`Population (in thousands) total numeric` = str_replace(`Population (in thousands) total`, " ", "")) %>%  
  select(-c(`Population living on &lt;$1 (PPP int. $) a day (%)`, `Population (in thousands) total`)) %>%
  filter(Year %in% c("2020", "2013", "2016")) 

## Johns Hopkins COVID data
# Confirmed COVID-19 cases, in time series. Global data _MCHR001 is working on this

#JH_conftime_claen <- JH_conftime %>% 
  
  
#UN datasets
UN_pop_clean <- UN_pop %>%
  select(X2, Year, Series, Value) %>%
  rename("Country_Region" = "X2") %>%
  filter(Year == 2019, Series == "Population density" | Series == "Sex ratio (males per 100 females)" | Series == "Population aged 60+ years old (percentage)") %>%
  pivot_wider(names_from = Series, values_from = Value) %>%
  select(Country_Region, 'Population density', 'Sex ratio (males per 100 females)', 'Population aged 60+ years old (percentage)')


UN_gdp_clean <- UN_gdp %>%
  separate(X1, into = c("nr", "Country_Region", "Year", "Series", "Value", "footnotes", "source"), sep = ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")
UN_gdp_clean <- as.data.frame(sapply(UN_gdp_clean, function(x) gsub("\"", "", x))) %>%
  filter(Year == 2017, Series == "GDP in current prices (millions of US dollars)" | Series == "GDP per capita (US dollars)") %>%
  pivot_wider(names_from = Series, values_from = Value) %>%
  select(Country_Region, 'GDP in current prices (millions of US dollars)', 'GDP per capita (US dollars)')

#Gender leader
sex_leader_clean <- sex %>% 
  filter(year == 2020 & month == 4) %>%
  mutate(sex = case_when(`male` == 1 ~ "male",
                         `male` == 0 ~ "female")) %>%
  select(country, sex)


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

#Removing useless variables 
#Uniting primary and secondary causes of disease with "_" 
#Removing excess digits/letters

  mortality_causes_clean <- mortality_causes %>% 
  as_tibble(mortality_causes_clean) %>% 
  rename(Cause_1 = "...5", Cause_2 = "...6") %>% 
  select(-'Sex', -'GHE code', -'Member State
(See Notes for explanation of colour codes)', -'GHE cause', -'...3') %>% 
  unite("Cause_clean", Cause_1:Cause_2, sep = "_", remove = TRUE, na.rm = T) %>% 
  select(Cause_clean, everything()) %>% 
  mutate(Cause_clean = str_replace(Cause_clean, "^\\w+\\.", "")) %>% 
  mutate(Cause_clean = str_replace(Cause_clean, "^_", "")) %>% 
  rowid_to_column("ID") %>% 

  pivot_longer(cols = select(-"Cause_clean"), 
  names_to = "Country", 
  values_to = "Result")
  

mortality_causes_clean
rlang::last_error()
rlang::last_trace()

#Test of above regular expression as string
writeLines("^\\w+\\.")



# Write data
# ------------------------------------------------------------------------------
#write_tsv(x = my_data_clean, path = "data/02_my_data_clean.tsv")

write_tsv(x = adult_mortality_clean,
          path = "data/02_adult_mortality_clean.tsv")
write_tsv(x = life_expectancy_clean,
          path = "data/02_life_expectancy_clean.tsv")
write_tsv(x = UN_pop_clean,
          path = "data/02_UN_pop_clean.tsv")
write_tsv(x = UN_gdp_clean,
          path = "data/02_UN_gdp_clean.tsv")
write_tsv(x = sex_leader_clean,
          path = "data/02_sex_leader_clean.tsv")
