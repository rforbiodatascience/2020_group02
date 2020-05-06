# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")


# Define functions
# ------------------------------------------------------------------------------
# source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv")

# Wrangle data
# ------------------------------------------------------------------------------


#Plotting development of cases and deaths for each country
ggplot(covid_aug, aes(x=date, y=`number_of_confirmed_covid-19`, group=country)) +
  geom_line()
ggplot(covid_aug, aes(x=date, y=`number_of_covid-19_related_deaths`, group=country, color=sex)) +
  geom_line()



#one row pr country using latest date
covid_aug_by_country <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date)) 

#plotting depending variable (x-axis) possibly affecting corona outbreak (y-axis)

  ggplot(covid_aug_by_country, aes(y = days_from_100_cases_to_100_deaths, x = density_of_medical_doctors)) +
  geom_point(aes(color=sex, size=population_in_thousands_total)) +
    geom_smooth(method="loess")
  
  
  ggplot(covid_aug_by_country, aes(y = days_from_dec1_to_100_cases, x = density_of_medical_doctors)) +
    geom_point(aes(color=sex, size=population_in_thousands_total))
  
  

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>%
  ggplot(covid_aug, mapping = aes(y = days_from_100_cases_to_100_deaths, x = density_of_medical_doctors), size = "population_in_thousands_total", color = "sex") +
  geom_point()


install.packages("plotly")
library(plotly)
g <- crosstalk::SharedData$new(gapminder, ~continent)
gg <- ggplot(covid_aug_by_country, aes(density_of_medical_doctors, days_from_100_cases_to_100_deaths, color = sex, frame = date)) +
  geom_point(aes(size = population_in_thousands_total, ids=country))
ggplotly(gg) %>% 
  highlight("plotly_hover")



# Write data
# ------------------------------------------------------------------------------
write_tsv(...)
ggsave(...)
