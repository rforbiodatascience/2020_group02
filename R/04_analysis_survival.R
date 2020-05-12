# Clear workspace ---------------------------------------------------------

rm(list = ls())


# Load libraries ----------------------------------------------------------

library("tidyverse")
library("patchwork")
library("survivalAnalysis")
library("broom")



# Load data ---------------------------------------------------------------

covid_aug <- read_tsv(file = "data/03_covid_aug.tsv",
                      col_types = cols(thousand_deaths = col_date(),
                                       `days_from_100_cases_to_1000_deaths` = col_double()))



# Wrangle data ------------------------------------------------------------

#kaplan-meier curves for survival - example with time to 100 deaths and density of medical doctors
kaplan_meier <- covid_aug %>% 
  group_by(country) %>% 
  slice(which.max(date)) %>%  
  ungroup() %>% 
  mutate(event = if_else(!is.na(hundred_deaths), 1, 0)) %>% 
  mutate(time = if_else(event == 0, (date - hundred_cases),(days_from_100_cases_to_100_deaths))) %>% 
  filter(!is.na(time))


# Survival analyses and plots -------------------------------------------------------

#KM plots for all tertiled variables
list_colnames <- names(kaplan_meier)[59:91]

plot_list <- list()

for(i in list_colnames){
  plot_name <- i
  i_var = as.name(i)
  km_res <- kaplan_meier %>% 
#  drop_na(i) %>% 
  analyse_survival(vars(time, event), by = factor(kaplan_meier[[i_var]]))
  km_plot <- kaplan_meier_plot(km_res,
                                 break.time.by=15.25,
                                 xlab="months",
                                 ylab="Cumulative survival (<100 deaths)",
                                 legend.title = str_replace_all(str_to_sentence(str_replace_all(i, "_", " ")), "ter", ""),
                                 font.legend = c(16, "black"),
                                 font.x = c(16, "black"), 
                                 font.y = c(16, "black"),
                                 font.tickslab = c(16, "bold"),
                                 hazard.ratio=T,
                                 risk.table=TRUE,
                                 risk.table.fontsize = 5,
                                 table.layout="clean",
                                 ggtheme=ggplot2::theme_bw(10)
)
    
    plot_list[[i]] = km_plot
    print(plot_list[[i]])
  }

  for(i in list_colnames){
    file_name = paste("results/04_analysis_survival/KM_", i, ".png", sep="")
    png(file_name, width=8.5, height = 6.5,unit='in',res=300)
    print(plot_list[[i]])
    dev.off()
  }
  
