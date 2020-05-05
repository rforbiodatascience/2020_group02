# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("patchwork")
library("leaflet")

# Define functions
# ------------------------------------------------------------------------------
# source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
##Tibble for Shiny app - see shiny app

tibble_shiny <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  select(country, dead_cases_per_100000, confirmed_cases_per_100000, population_median_age_years, density_of_hospitals, life_expectancy, population_living_in_urban_areas)



# Model data
# ------------------------------------------------------------------------------
#my_data_clean_aug %>% ...

# Visualise data
# ------------------------------------------------------------------------------

#Overview of COVID-19 confirmed cases, COVID-19 realted deaths and COVID-19 tests performed for selected countries -  see shiny app

# Is there a correlation between Covid-19 confirmed cases, deaths and recoveries and % people living in urban areas?

conf_cases_vs_urban <- covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(conf_cases_vs_urban,mapping = aes(y = confirmed_cases_per_100000, x = population_living_in_urban_areas, na.rm = TRUE)) +
  geom_point(alpha = 0.5, size = 3) 
  

conf_deaths_vs_urban <- covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(conf_deaths_vs_urban, mapping = aes(y = dead_cases_per_100000 , x = population_living_in_urban_areas, na.rm = TRUE)) +
  geom_point(alpha = 0.5, size = 3 )

conf_recov_vs_urban <- covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(conf_recov_vs_urban, mapping = aes(y = recov_cases_per_100000  , x = population_living_in_urban_areas, na.rm = TRUE)) +
  geom_point(alpha = 0.5, size = 3)

# Utilizing the patchwork package for plot assembly

conf_cases_vs_urban / conf_deaths_vs_urban / conf_recov_vs_urban + 
  plot_annotation(
    title = "COVID-19 confirmed cases, COVID-19 deaths, COVID-19 recovered (cummulative April 16th) vs. urbanisation in countries", 
    subtitle = "No correlation, all variables increases in highly urbanised countries. San Marino has the highest proportion of deaths and confimed cases"
  )

#-----------------------------------------------------------------------------

# Is there a correlation between 1) Covid-19 confirmed deaths and 2) Covid-19 recoveries and age of population?

deaths_vs_median_age <- covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(conf_cases_vs_urban, mapping = aes(y = dead_cases_per_100000, x = population_median_age_years), na.rm = TRUE ) +
  geom_point(alpha = 0.5, size = 3)+
  ylim(0,60)



deaths_vs_life_exp <- covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  ggplot(conf_cases_vs_urban, mapping = aes(y = dead_cases_per_100000 , x = life_expectancy), na.rm = TRUE, ) +
  geom_point(alpha = 0.5, size = 3) + 
  ylim(0,60)


deaths_vs_median_age/deaths_vs_life_exp + 
  plot_annotation(
    title = "COVID-19 deaths (cummulative April 16th) vs. age of population in countries", 
    subtitle = "Higher proportion of COVID-19 deaths in countries with a higher proportion of people > 40 years and higher life expectancy in years"
  )

#-----------------------------------------------------------------
# Covid-19 map

# Selecting fortotal no of deaths

covid_death_map <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  select(country, lat, long, `number_of_covid-19_related_deaths`, dead_cases_per_100000) %>% 
  filter(!is.na(dead_cases_per_100000))


#Creating pop-ups to map

covid_death_map <- covid_death_map %>% 
  mutate(popup_info = paste("Country:", country, 
                            "<br/>", 
                            "Total no. of COVID- 19 related deaths:", `number_of_covid-19_related_deaths`, "<br/>", "No. COVID- 19 related deaths regulated to pop. size :", dead_cases_per_100000))
         

## Making the map, selceting map layout in http://leaflet-extras.github.io/leaflet-providers/preview/index.html 
#Deaths per population size (deaths pr. 100.000)#"Esri.WorldGrayCanvas"

covid_death_map_pop <- covid_death_map  %>% 
  leaflet() %>%
  addProviderTiles(provider = "Stamen.TerrainBackground" ) %>% 
  addCircles(covid_death_map, weight = 1, color = "red", opacity = 2,
             lng = ~long, lat = ~lat, radius = ~dead_cases_per_100000 * 5000
             , popup = ~popup_info)

covid_death_map_pop

#Total no. deaths pr. country

covid_death_map_total <- covid_death_map %>% 
  leaflet() %>%
  addProviderTiles(provider = "Stamen.TerrainBackground" ) %>% 
  addCircles(covid_death_map, weight = 1, color = "red", opacity = 2,
             lng = ~long, lat = ~lat, radius = ~`number_of_covid-19_related_deaths` * 30
             , popup = ~popup_info)
covid_death_map_total

# Write data
# ------------------------------------------------------------------------------
write_tsv(...)
ggsave(...)