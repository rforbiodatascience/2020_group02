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

#Plotting development of cases and deaths for each country

#ggplot(covid_aug, aes(x=date, y=`number_of_confirmed_covid-19`, group=country)) +
  geom_line()
#ggplot(covid_aug, aes(x=date, y=`number_of_covid-19_related_deaths`, group=country, color=sex)) +
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


covid_aug_by_country <- covid_aug_by_country %>%
  mutate(quartile = ntile(value, 10)) %>%
  mutate

#Making interactive plot for selected variables (life_exp, health_expenditure, Pollution, pop in urban, pop >60 years)

p_le <- ggplot(covid_aug, aes_string(x="life_expectancy", y = 'days_from_dec1_to_100_cases')) +
  geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5)) + 
  scale_size(range = c(0.5, 20), name="Population", labels = NULL) +
  scale_colour_gradientn(colours=topo.colors(5), name = "GDP per capita (log)") +
  ylab("Days from 1st December to 100 cases") +
  xlab("Life expectancy") +
  ggtitle("Development of Corona-pandemic by country") +
  guides(alpha="none")

p_he <- ggplot(covid_aug_by_country, aes_string(x="current_health_expenditure_per_person_usd", y = 'days_from_100_cases_to_100_deaths')) +
  geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5)) + 
  scale_size(range = c(0.5, 20), name="Population", labels=NULL) +
  scale_colour_gradientn(colours=topo.colors(5), name="GDP per capita") +
  ylab("Days from 100 cases to 100 deaths") +
  xlab("Health expenditure per person USD (log)") +
  scale_x_log10() +
  ggtitle("Development of Corona-pandemic by country") +
  guides(alpha="none")



#Population above 60 years old
p_60years <- ggplot(covid_aug_by_country, aes(x = `population_aged_60_years_old_percentage`, y = `days_from_dec1_to_100_cases`,
            color = log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5, ids=country)) +
  geom_point() +
  scale_size(range = c(0.5, 20), name="Population") +
  scale_colour_gradientn(colours=topo.colors(5), name = "GDP per capita") +
  ylab("Number of confirmed Covid-19 cases") +
  xlab("population aged above 60 years old (%)") +
  ggtitle("Development of cases by country") +
  guides(alpha="none")


ggplotly(p_60years, tooltip = c("country")) %>% 
  highlight("plotly_hover")
ggplotly(p_urban, tooltip = c("country")) %>% 
  highlight("plotly_hover")
ggplotly(p_le) %>% 
  rangeslider(covid_aug$date)


covid_aug2 <- covid_aug %>%
  rename(covid_deaths = 'number_of_covid-19_related_deaths') %>%
  rename(covid_cases = 'number_of_confirmed_covid-19') %>%
  rename(date2 = date, Date)

 
#Making gif showing progression of corona for each country
gif_plot <-ggplot(covid_aug2, aes_string(x="covid_cases", y = "covid_deaths")) +
  geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5)) + 
  scale_size(range = c(0.5, 20), name="Population", labels = NULL) +
  scale_colour_gradientn(colours=topo.colors(5), name = "GDP per capita") +
  ylab("Covid deaths") +
  xlab("covid_cases") +
  ggtitle("Development of Corona-pandemic by country") +
  guides(alpha="none") + 
  transition_time(date) +
    labs(title = "Date: {frame_time}")
#Animate and present the files in  a gif
 animate(gif_plot, duration = 10, fps = 10, width = 450, height = 450, renderer = gifski_renderer())
 # save as a GIF
 anim_save("results/04_analysis_iv/hj_plot_test.gif")
 
 #Making gif showing progression of corona for each country - log-axis
 gif_plot_log <-ggplot(covid_aug2, aes_string(x="covid_cases", y = "covid_deaths")) +
   geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5)) + 
   scale_size(range = c(0.5, 20), name="Population", labels = NULL) +
   scale_colour_gradientn(colours=topo.colors(5), name = "GDP per capita") +
   scale_x_log10() +
   scale_y_log10() +
   ylab("Covid deaths") +
   xlab("covid_cases") +
   ggtitle("Development of Corona-pandemic by country") +
   guides(alpha="none") + 
   transition_time(date) +
   labs(title = "Date: {frame_time}")
 #Animate and present the files in  a gif
 animate(gif_plot_log, duration = 10, fps = 10, width = 450, height = 450, renderer = gifski_renderer())
 # save as a GIF
 anim_save("results/04_analysis_iv/hj_plot_test_log.gif")
 
 


# Write data
# ------------------------------------------------------------------------------
write_tsv(...)
ggsave(...)
