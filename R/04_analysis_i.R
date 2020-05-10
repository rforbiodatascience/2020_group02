# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")
library("leaflet")
library("htmlwidgets")

# Load data ---------------------------------------------------------------
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv")


# Wrangle data ------------------------------------------------------------
#Selecting for total no. of deaths, dead_cases_per_100000, days_from_100_cases_to_100_deaths, days_from_dec1_to_100_cases 
covid_map <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date)) %>% 
  select(country, lat, long, `number_of_covid-19_related_deaths`, dead_cases_per_100000, days_from_100_cases_to_100_deaths, days_from_dec1_to_100_cases)

#NB!For overview of COVID-19 confirmed cases, COVID-19 realted deaths and COVID-19 tests performed for selected countries - see covid_app

# Visualise data - COVID-19 global maps -----------------------------------
#Creating pop-ups to maps for total no. of deaths and dead cases per 100.000
covid_map <- covid_map %>% 
  mutate(popup_death = paste("Country:", country, 
                             "<br/>", 
                             "Total no. of COVID-19 related deaths:", `number_of_covid-19_related_deaths`, 
                             "<br/>", 
                             "No. COVID-19 related deaths (per 100.000):", dead_cases_per_100000))

#Selceting map layout, "Stamen.TerrainBackground", in http://leaflet-extras.github.io/leaflet-providers/preview/index.html 

#Global map for deaths per population size (deaths pr. 100.000)
map_dead_cases_per_100000 <- covid_map %>% 
  filter(!is.na(dead_cases_per_100000)) %>% 
  leaflet() %>%
  addProviderTiles(provider = "Stamen.TerrainBackground") %>% 
  addCircles(covid_map, lng = ~long, lat = ~lat, radius = ~`dead_cases_per_100000` * 5000,
             popup = ~popup_death, weight = 3, color = "red", opacity = 2)

#Global map for total no. of deaths
map_number_of_covid_19_related_deaths <- covid_map %>% 
  filter(!is.na(`number_of_covid-19_related_deaths`)) %>% 
  leaflet() %>%
  addProviderTiles(provider = "Stamen.TerrainBackground") %>% 
  addCircles(covid_map, lng = ~long, lat = ~lat, radius = ~`number_of_covid-19_related_deaths` * 20,
             popup = ~popup_death, weight = 3, color = "red", opacity = 2) 

#Global map of deaths from 1st december to 100 confirmed cases, as range.
#Filtering NAs from variables to be plotted. Adding a new variable (covid_map_cuts) for indexing no. of days from Dec 1st to first 100 confirmed COVID in ranges (1-80; 80-120; .. days)
map_days_from_dec1_to_100_cases <- covid_map  %>% 
  filter(!is.na(days_from_dec1_to_100_cases)) %>% 
  mutate(covid_map_cuts = cut(days_from_dec1_to_100_cases, c(1, 80, 120, 150, Inf), 
                              labels = c("< 80 days", "80-120 days", "120-150 days", ">150 days")))
 
#Choosing color/palet for range intervals for mapping. 
pal_1 <- colorFactor(palette = c("red", "orange", "yellow", "green"), 
                   domain = map_days_from_dec1_to_100_cases$covid_map_cuts)

#Mapping data with colour related to range index, and adding labels + legeends.
map_days_from_dec1_to_100_cases <- map_days_from_dec1_to_100_cases %>% 
  leaflet() %>%
  addProviderTiles(provider = "Stamen.TerrainBackground") %>% 
  addCircles(covid_map, lng = ~long, lat = ~lat, color = ~pal_1(covid_map_cuts), 
             weight = 2, radius = 300000, opacity = 1, 
             label = paste(map_days_from_dec1_to_100_cases$country, 
                           "- days from December 1st to 100 COVID-19 cases:", map_days_from_dec1_to_100_cases$days_from_dec1_to_100_cases)) %>% 
  addLegend(title = "COVID-19: days from December 1st to first 100 confirmed cases ", pal = pal_1, values = map_days_from_dec1_to_100_cases$covid_map_cuts)


#Global map of days from first 100 cases to 100 deaths, as range. For work-around explanation, see above.
map_days_from_100_cases_to_100_deaths <- covid_map  %>% 
  filter(!is.na(days_from_100_cases_to_100_deaths)) %>% 
  mutate(covid_map_cuts_death = cut(days_from_100_cases_to_100_deaths, c(1, 10, 20, 30, 40, Inf), 
                                    labels = c("< 10 days", "10-20 days", "20-30 days", "30-40", ">40 days")))

pal_2 <- colorFactor(palette = c("red", "orange", "yellow", "green", "white"), 
                     domain = map_days_from_100_cases_to_100_deaths$covid_map_cuts_death)

map_days_from_100_cases_to_100_deaths  <- map_days_from_100_cases_to_100_deaths%>% 
  leaflet() %>%
  addProviderTiles(provider = "Stamen.TerrainBackground") %>% 
  addCircles(covid_map, lng = ~long, lat = ~lat, color = ~pal_2(covid_map_cuts_death), 
             weight = 2, radius = 300000, opacity = 1,
             label = paste(map_days_from_100_cases_to_100_deaths$country, 
                           "- days from first 100 confimed cases to 100 deaths:", map_days_from_100_cases_to_100_deaths$days_from_100_cases_to_100_deaths)) %>% 
  addLegend(title = "COVID-19: days from first 100 confirmed cases to 100 deaths",
            pal = pal_2, values = map_days_from_100_cases_to_100_deaths$covid_map_cuts_death)


# Write data - saving maps ------------------------------------------------
#Savewiget only possible to working directory. Using normalizePath to save to subdirectories. 

f<-"results/04_analysis_i/map_dead_cases_per_100000.html"
saveWidget(widget = map_dead_cases_per_100000, file.path(normalizePath(dirname(f)), basename(f)))

f<-"results/04_analysis_i/map_number_of_covid_19_related_deaths.html"
saveWidget(widget = map_number_of_covid_19_related_deaths, file.path(normalizePath(dirname(f)), basename(f)))

f<-"results/04_analysis_i/map_days_from_dec1_to_100_cases.html"
saveWidget(widget = map_days_from_dec1_to_100_cases, file.path(normalizePath(dirname(f)), basename(f)))

f<-"results/04_analysis_i/map_days_from_100_cases_to_100_deaths.html"
saveWidget(widget = map_days_from_100_cases_to_100_deaths, file.path(normalizePath(dirname(f)), basename(f)))
