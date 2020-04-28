# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("readr")
library("forcats")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
#my_data_clean <- read_tsv(file = "data/02_my_data_clean.tsv")

COVID_test_clean <- read_tsv(file = "data/02_COVID_test_clean.tsv")
POP_demo_clean <- read_tsv(file = "data/02_POP_demo_clean.tsv")
JH_conftime_clean <- read_tsv(file = "data/02_JH_conftime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
JH_deadtime_clean <- read_tsv(file = "data/02_JH_deadtime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
JH_recotime_clean <- read_tsv(file = "data/02_JH_recotime_clean.tsv", col_types = cols(date = col_date(format="%m/%d/%y")))
POP_demo_clean <- read_tsv(file = "data/02_POP_demo_clean.tsv" )
smoking_clean <- read_tsv(file = "data/02_smoking_clean.tsv" )
UN_pop_clean <- read_tsv(file = "data/02_UN_pop_clean.tsv" )

# Wrangle data
# ------------------------------------------------------------------------------

#Anti-join for test
JH_conftime_clean <- JH_conftime_clean %>% 
  rename('Country' = 'Country/Region')

extra_countries_POP_demo <- POP_demo_clean %>% 
  anti_join(JH_conftime_clean, by = 'Country') %>% 
  count(Country, sort = T)

extra_countries_JH <- JH_conftime_clean %>% 
  anti_join(POP_demo_clean, by = 'Country') %>% 
  count(Country, sort = T)

#WHO datasets antijoin
WHO_extra_countries <- POP_demo_clean %>% 
  anti_join(smoking_clean, by = 'Country') %>% 
  count(Country, sort = T)


#WHO-UN datasets antijoin
UN_pop_clean <- UN_pop_clean %>% 
  rename('Country' = 'Country_Region')

WHO_UN_extra_countries <- POP_demo_clean %>% 
  anti_join(UN_pop_clean, by = 'Country') %>% 
  count(Country, sort = T)

#UN JH datasets antijoin
UN_extra_countries <- UN_pop_clean %>% 
  anti_join(JH_conftime_clean, by = 'Country') %>% 
  count(Country, sort = T)

rlang::last_error()


#Function for alligning to Country to Johns Hopkins data
#-------------------------------------------------------------------------------


country.translate <- function(x, y) {
  if (y == 'WHO') {
    WH("Afghanistan" = "Afghanistan", "Bolivia (Plurinational State of)" = "Bolivia", "Brunei Darussalam" = "Brunei", "Comoros" = "not in JH", "Congo" = "Congo (Brazzaville)" , 
       "Cook Islands" = "not in JH", "Côte d'Ivoire, Democratic People's Republic of Korea" = "not in JH" , "Democratic Republic of the Congo" = "Congo (Kinshasa)", 	"Iran (Islamic Republic of)" = "Iran",
       "Kiribati" = "not in JH", "Lao People's Democratic Republic" = "Laos" , "Lesotho, Marshall Islands"= "not in JH", "Micronesia (Federated States of)" = "not in JH", "Myanmar"="Burma", "Nauru" = "not in JH", "Niue" = "not in JH",
       "Palau" = "not in JH", "Republic of Korea" = "Korea, South", "Republic of Moldova" = "Moldova", "Republic of North Macedonia" = "North Macedonia", "Russian Federation"= "Russia", "Samoa" = "not in JH", 
       "Solomon Islands" = "not in JH", "Syrian Arab Republic" = "Syria", "Tajikistan" = "not in JH", "Tonga"= "not in JH", "Turkmenistan" = "not in JH", "Tuvalu"="not in JH", "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom", 
       "United Republic of Tanzania" = "Tanzania", "United States of America" = "US", "Vanuatu" = "not in JH", "Venezuela (Bolivarian Republic)" = "Venezuela", "Viet Nam" = "Vietnam")
    return(WHO_country[x])
  }
  else if (y == 'UN') {
    UN_country <- list(
      'Arg' = 'Argentina',
      'Lao' = 'Laos',
      'Viet Nam' = 'Vietnam')
    return(UN_country[x])
  }
  else if (y == 'ourworld') {
    ourworld_country <- list(
      'Argen' = 'Argentina',
      'Lao Republic' = 'Laos',
      'Viet_Nam' = 'Vietnam')
    return(ourworld_country[x])
  }
  else 
    print("please enter type of dataset (WHO/UN/ourworld)")
}


country_translate <- function(x){
con <- c("Afghanistan" ="Afghanistan", "Bolivia (Plurinational State of)"= "Bolivia", "Brunei Darussalam" = "Brunei", "Comoros" = "not in JH", "Congo" = "Congo (Brazzaville)" , 
         "Cook Islands" = "not in JH", "Côte d'Ivoire, Democratic People's Republic of Korea" = "not in JH" , "Democratic Republic of the Congo" = "Congo (Kinshasa)", 	"Iran (Islamic Republic of)" = "Iran",
         "Kiribati" = "not in JH", "Lao People's Democratic Republic" = "Laos" , "Lesotho, Marshall Islands"= "not in JH", "Micronesia (Federated States of)" = "not in JH", "Myanmar"="Burma", "Nauru" = "not in JH", "Niue" = "not in JH",
         "Palau" = "not in JH", "Republic of Korea" = "Korea, South", "Republic of Moldova" = "Moldova", "Republic of North Macedonia" = "North Macedonia", "Russian Federation"= "Russia", "Samoa" = "not in JH", 
         "Solomon Islands" = "not in JH", "Syrian Arab Republic" = "Syria", "Tajikistan" = "not in JH", "Tonga"= "not in JH", "Turkmenistan" = "not in JH", "Tuvalu"="not in JH", "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom", 
         "United Republic of Tanzania" = "Tanzania", "United States of America" = "US", "Vanuatu" = "not in JH", "Venezuela (Bolivarian Republic)" = "Venezuela", "Viet Nam" = "Vietnam") 
return(as.list(con[x]))
}


country_transverter_test <- POP_demo_clean %>%
  mutate(Country1 =(country_translate(Country))) %>% 
  mutate(Country = ifelse(!is.na(Country1),Country1,Country))
  

# Write data
# ------------------------------------------------------------------------------
write_tsv(x = my_data_clean_aug,
          path = "data/03_my_data_clean_aug.tsv")