
# Clear Workspace ---------------------------------------------------------

rm(list = ls())


# Load libraries ----------------------------------------------------------

install.packages("gganimate")
install.packages("gifski")
install.packages("plotly")
install.packages("scales")
library(plotly)
library(tidyverse)
library(gganimate)
library(gifski)
library(scales)

# Load data ---------------------------------------------------------------

covid_aug <- read_tsv(file = "data/03_covid_aug.tsv")

#one row pr country using latest date
covid_aug_by_country <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date)) 


# Plots in loop - creating png files --------------------------------------

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
    xlab(str_to_sentence(str_replace(i, "_", " "))) +
    ggtitle("Development of Corona-pandemic by country") +
    guides(alpha="none")
  plot_list_hj1[[i]] = plt
  print(plot_list_hj1[[i]])
}

for(i in list_of_cov_hj){
  file_name = paste("results/04_analysis_point_plots/deaths_", i, ".png", sep="")
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
    scale_colour_gradientn(colours=topo.colors(5), name="GDP per capita (log)") +
    ylab("Days from 1st December to 100 cases") +
    xlab(str_to_sentence(str_replace(i, "_", " "))) +
    ggtitle("Development of Corona-pandemic by country") +
    guides(alpha="none")
  plot_list_hj2[[i]] = plt
  print(plot_list_hj2[[i]])
}

for(i in list_of_cov_hj){
  file_name = paste("results/04_analysis_point_plots/cases_", i, ".png", sep="")
  png(file_name, width=8.5, height = 6.5,unit='in',res=300)
  print(plot_list_hj2[[i]])
  dev.off()
}



# Interactive plots for selected variables --------------------------------

#plotting depending variable (x-axis) possibly affecting corona outbreak (y-axis)
p_resp <- ggplot(covid_aug_by_country, aes_string(x="respiratory_diseases", y = 'days_from_dec1_to_100_cases')) +
  geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5)) + 
  scale_size(range = c(0.5, 20), name="Population", labels = NULL) +
  scale_colour_gradientn(colours=topo.colors(5), name = "GDP per capita (log)") +
  ylab("Days from 1st December to 100 cases") +
  xlab("Respiratory Disases") +
  ggtitle("Development of Corona-pandemic by country") +
  guides(alpha="none")

p_le <- ggplot(covid_aug_by_country, aes_string(x="life_expectancy", y = 'days_from_dec1_to_100_cases')) +
  geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5)) + 
  scale_size(range = c(0.5, 20), name="Population", labels = NULL) +
  scale_colour_gradientn(colours=topo.colors(5), name = "GDP per capita (log)") +
  ylab("Days from 1st December to 100 cases") +
  xlab("Life expectancy (years)") +
  ggtitle("Development of Corona-pandemic by country") +
  guides(alpha="none")

p_urban <- ggplot(covid_aug_by_country, aes_string(x="population_living_in_urban_areas", y = 'days_from_dec1_to_100_cases')) +
  geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.5)) + 
  scale_size(range = c(0.5, 20), name="Population", labels=NULL) +
  scale_colour_gradientn(colours=topo.colors(5), name="GDP per capita (log)") +
  ylab("Days from 1st December to 100 cases") +
  xlab("Population living in urban areas (%)") +
  ggtitle("Development of Corona-pandemic by country") +
  guides(alpha="none")



ggplotly(p_resp, tooltip = c("country", "gdp_per_capita_us_dollars")) %>% 
  highlight("plotly_hover")
ggplotly(p_urban, tooltip = c("country")) %>% 
  highlight("plotly_hover")
ggplotly(p_urban) %>% 
  rangeslider(covid_aug$date[2020-01-22], covid_aug$date[2020-05-03])


# Gif ---------------------------------------------------------------------

#renaming output-variables for gif
covid_aug2 <- covid_aug %>%
  rename(covid_deaths = 'number_of_covid-19_related_deaths') %>%
  rename(covid_cases = 'number_of_confirmed_covid-19')
#Making gif showing progression of corona for each country - log-axis
 gif_plot_log <-ggplot(covid_aug2, aes_string(x="covid_cases", y = "covid_deaths")) +
   geom_point(aes(color=log(gdp_per_capita_us_dollars), size=population_in_thousands_total, alpha=0.7)) + 
   scale_size(range = c(0.5, 20), name="Population", label = comma) +
   scale_colour_gradientn(colours=topo.colors(5), name = "GDP per capita (log)") +
   scale_x_log10(label = comma) +
   scale_y_log10(label = comma) +
   ylab("Confirmed Covid-19 deaths (log)") +
   xlab("Confirmed Covid-19 cases (log)") +
   theme(axis.text = element_text(size = 12), axis.title = element_text(size = 18)) +
   guides(alpha="none") + 
   transition_time(date) +
   labs(title = "Date: {frame_time}")
 #Animate and present the files in  a gif
 animate(gif_plot_log, duration = 10, fps = 10, width = 900, height = 450, renderer = gifski_renderer())
 # save as a GIF
 anim_save("results/04_analysis_point_plots/cases_deaths_log.gif")
 
 


# Write data
# ------------------------------------------------------------------------------
write_tsv(...)
ggsave(...)
