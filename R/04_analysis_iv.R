# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
install.packages("gganimate")
install.packages("gifski")
install.packages("plotly")
library(plotly)
library(tidyverse)
library(gganimate)
library(gifski)


# Define functions
# ------------------------------------------------------------------------------
# source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv")

#one row pr country using latest date
covid_aug_by_country <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date)) 

# Wrangle data
# ------------------------------------------------------------------------------

#TO DO
# 1) Legend for plots
# 2) Make comparable gifs
# 4) Make shiny-app

#Plotting development of cases and deaths for each country

ggplot(covid_aug, aes(x=date, y=`number_of_confirmed_covid-19`, group=country)) +
  geom_line()
ggplot(covid_aug, aes(x=date, y=`number_of_covid-19_related_deaths`, group=country, color=sex)) +
  geom_line()



#plotting depending variable (x-axis) possibly affecting corona outbreak (y-axis)
ggplot(covid_aug_by_country, aes_string(x="population_aged_60_years_old_percentage", y = 'days_from_dec1_to_100_cases')) +
  geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5)) + 
  scale_size(range = c(0.5, 20), name="Population", labels = NULL) +
  scale_colour_gradientn(colours=topo.colors(5), name = "GDP per capita") +
  ylab("Days from 1st December to 100 cases") +
  xlab("population_aged_60_years_old_percentage") +
  ggtitle("Development of Corona-pandemic by country") +
  guides(alpha="none")
  


#Making list for looping all variables against selected outcomes
list_of_cov_hj <- names(covid_aug)[8:40]

#Plotting all variables against "Days from 100 cases to 100 deaths"
plot_list_hj1 <- list()
for(i in list_of_cov_hj){
  plot_name <- i
  plt <- covid_aug_by_country %>% 
    drop_na(i) %>% 
  ggplot(aes_string(x=i, y = 'days_from_100_cases_to_100_deaths')) +
    geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5)) + 
    scale_size(range = c(0.5, 20), name="Population", labels=NULL) +
    scale_colour_gradientn(colours=topo.colors(5), name="GDP per capita") +
    ylab("Days from 100 cases to 100 deaths") +
    xlab(i) +
    ggtitle("Development of Corona-pandemic by country") +
    guides(size="none", alpha="none")
  plot_list_hj1[[i]] = plt
  print(plot_list_hj1[[i]])
}

for(i in list_of_cov_hj){
  file_name = paste("results/04_analysis_iv/deaths_", i, ".png", sep="")
  png(file_name, width=8.5, height = 6.5,unit='in',res=300)
  print(plot_list_hj1[[i]])
  dev.off()
}

#Plotting all variables against "'days_from_dec1_to_100_cases'"
plot_list_hj2 <- list()
for(i in list_of_cov_hj){
  plot_name <- i
  plt <- covid_aug_by_country %>% 
    drop_na(i) %>% 
    ggplot(aes_string(x=i, y = 'days_from_dec1_to_100_cases')) +
    geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5)) + 
    scale_size(range = c(.1, 20), name="Population", labels= NULL) +
    scale_colour_gradientn(colours=topo.colors(5), name="GDP per capita") +
    ylab("Days from 1st December to 100 cases") +
    xlab(i) +
    ggtitle("Development of Corona-pandemic by country") +
    guides(size="none", alpha="none")
  plot_list_hj2[[i]] = plt
  print(plot_list_hj2[[i]])
}

for(i in list_of_cov_hj){
  file_name = paste("results/04_analysis_iv/cases_", i, ".png", sep="")
  png(file_name, width=8.5, height = 6.5,unit='in',res=300)
  print(plot_list_hj2[[i]])
  dev.off()
}



gg <- ggplot(covid_aug, aes(x = density_of_medical_doctors, y = `number_of_confirmed_covid-19`, color = sex, frame = date, size = population_in_thousands_total, ids=country)) +
  geom_point()
ggplotly(plot_test) %>% 
  highlight("plotly_hover")

 
#Making gif showing progression of corona for each country
 plot_test <- ggplot(covid_aug, aes(y = `number_of_confirmed_covid-19`, x = `number_of_covid-19_related_deaths`)) +
    geom_point(aes(color=sex, size=population_in_thousands_total, alpha=0.6)) + 
    scale_size(range = c(.1, 16), name="Population") +
    ylab("Confirmed Covid-19 cases") +
    xlab("Density of medical doctors (unit)") +
    ggtitle("Development of Corona-pandemic by country") +
    guides(size="none", alpha="none") + 
  transition_time(date) +
    labs(title = "Date: {frame_time}")
#Animate and present the files in  a gif
 animate(plot_test, duration = 10, fps = 10, width = 450, height = 450, renderer = gifski_renderer())
 # save as a GIF
 anim_save("results/04_analysis_iv/hj_plot_test.gif")
 
 

  ggplot(covid_aug_by_country, aes(y = days_from_dec1_to_100_cases, x = density_of_medical_doctors)) +
    geom_point(aes(color=sex, size=population_in_thousands_total))
  
  

covid_aug %>%
  group_by(country) %>% 
  slice(which.max(date)) %>%
  ggplot(covid_aug, mapping = aes(y = days_from_100_cases_to_100_deaths, x = density_of_medical_doctors), size = "population_in_thousands_total", color = "sex") +
  geom_point()

#covid_aug_hj <- covid_aug %>%
  mutate(date, as_date(date))



gg <- ggplot(covid_aug, aes(x = density_of_medical_doctors, y = `number_of_confirmed_covid-19`, color = sex, frame = date, size = population_in_thousands_total, ids=country)) +
  geom_point()
ggplotly(plot_test) %>% 
  highlight("plotly_hover")




# Write data
# ------------------------------------------------------------------------------
write_tsv(...)
ggsave(...)
