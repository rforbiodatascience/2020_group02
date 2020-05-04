# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("patchwork")
library(survivalAnalysis)

# Define functions
# ------------------------------------------------------------------------------
# source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv")

# Wrangle data
# ------------------------------------------------------------------------------


#kaplan-meier curves for survival - example with time to 100 deaths and density of medical doctors
kaplan_meier <- covid_join %>% 
  group_by(country) %>% 
  slice(which.max(date)) %>%  
  ungroup() %>% 
  mutate(event = if_else(!is.na(hundred_deaths), 1, 0)) %>% 
  mutate(time = if_else(event == 0, (date - hundred_cases),(days_from_100_cases_to_100_deaths))) %>% 
  filter(!is.na(time))

#summary and plot
#hvis man vil have kategorien "missing" med kan man bruge fct_explicit_na() i stedet for factor
kaplan_meier %>% analyse_survival(vars(time, event), by=factor(density_medical_doctors_ter)) ->
  km_result
print(km_result)

#Kaplan meier plot saved as .png
png("results/km_medical_doctors.png", width=8.5,height = 6.5,unit='in',res=300)
kaplan_meier_plot(km_result,
                  break.time.by=15.25,
                  xlab="months",
                  legend.title="Density of medical doctors",
                  hazard.ratio=T,
                  risk.table=TRUE,
                  table.layout="clean",
                  ggtheme=ggplot2::theme_bw(10))

dev.off()

# Write data
# ------------------------------------------------------------------------------
write_tsv(...)
ggsave(...)
