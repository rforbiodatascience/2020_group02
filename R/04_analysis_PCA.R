
# Clear workspace ---------------------------------------------------------

rm(list = ls())


# Load libraries ----------------------------------------------------------

library("tidyverse")
library("patchwork")
library("survivalAnalysis")
library("broom")


# Load data ---------------------------------------------------------------

covid_aug <- read_tsv(file = "data/03_covid_aug.tsv")


# PCA data analysis -------------------------------------------------------

#Filtering to avoid missing data and removing non-numeric columns
#Performing PCA based on country demographics
covid_pca <- covid_aug %>%
  filter(!is.na(concentration_fine_particles)) %>% 
  filter(!is.na(bmi_above30_prevalence_all)) %>% 
  filter(!is.na(current_health_expenditure_per_person_usd)) %>% 
  filter(!is.na(density_of_medical_doctors)) %>% 
  filter(!is.na(life_expectancy)) %>% 
  filter(!is.na(population_aged_60_years_old_percentage)) %>% 
  filter(!is.na(population_density)) %>% 
  filter(!is.na(cardiovascular_diseases)) %>% 
  group_by(country) %>% 
  slice(which.max(date)) %>%  
  ungroup() %>% 
  select_if(colSums(is.na(.)) == 0) %>% 
  select(-('country':'adult_mortality_rate'), -country, -date, -first_case, -ends_with("ter"), -confirmed_cases_per_100000, -dead_cases_per_100000) %>% 
  prcomp(center = TRUE, scale. = TRUE)
covid_pca

covid_pca %>% tidy("pcs")

#Plot of PCA significance of individual PCs
PC_sign <- covid_pca %>% tidy("pcs") %>% 
  ggplot(aes(x = PC, y = percent)) +
  geom_col() +
  theme_bw()

png("results/04_analysis_PCA/PC_significance.png", width=8.5,height = 6.5,unit='in',res=300)

PC_sign

dev.off()


#Naming of filtered dataset for augmenting
covid_filtered <- covid_aug %>%
  filter(!is.na(concentration_fine_particles)) %>% 
  filter(!is.na(bmi_above30_prevalence_all)) %>% 
  filter(!is.na(current_health_expenditure_per_person_usd)) %>% 
  filter(!is.na(density_of_medical_doctors)) %>% 
  filter(!is.na(life_expectancy)) %>% 
  filter(!is.na(population_aged_60_years_old_percentage)) %>% 
  filter(!is.na(population_density)) %>% 
  filter(!is.na(cardiovascular_diseases)) %>% 
  group_by(country) %>% 
  slice(which.max(date)) %>%
  mutate(binary_death = if_else(!is.na(hundred_deaths), 1, 0)) %>% 
  mutate(tertile_deaths = case_when(
    `number_of_covid-19_related_deaths` < 100  ~ 0,
    `number_of_covid-19_related_deaths` > 99 & `number_of_covid-19_related_deaths` < 1000 ~ 1,
    `number_of_covid-19_related_deaths` > 999 ~ 2)) %>% 
  mutate(tertile_rel_deaths = case_when(
    `dead_cases_per_100000` < 1  ~ 0,
    `dead_cases_per_100000` > 0.999 & `dead_cases_per_100000` < 10 ~ 1,
    `dead_cases_per_100000` > 9.99 ~ 2)) %>% 
  
  ungroup() %>% 
  select_if(colSums(is.na(.)) == 0) %>% 
  select(-('country':'adult_mortality_rate'), -date, -first_case, -ends_with("ter"), -confirmed_cases_per_100000, -dead_cases_per_100000)


#Augmenting PCA data with dataset (filtered)
covid_pca_aug <- covid_pca %>% augment(covid_filtered)
covid_pca_aug

#plotting PCA with coloring by tertile absolute and relative COVID-19 variables
pl_pca_1 <- covid_pca_aug %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = tertile_deaths)) +
  geom_point()

pl_pca_2 <- covid_pca_aug %>% 
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = tertile_rel_deaths)) +
  geom_point()


png("results/04_analysis_PCA/pca_deaths.png", width=8.5,height = 6.5,unit='in',res=300)

(pl_pca_1+pl_pca_2)

dev.off()

#Clustering - n=3 - cluster size 12, 2 and 150
covid_k_org <- covid_pca_aug %>%
  kmeans(centers = 3)
covid_k_org

#augmenting clustering data with PCA and dataset
covid_pca_aug_k_org <- covid_k_org %>%
  augment(covid_pca_aug) %>% 
  rename(cluster_org = .cluster)
covid_pca_aug_k_org

#Clustering based on PCA - cluster size 95, 67 and 2
covid_k_pca <- covid_pca_aug_k_org %>%
  select(.fittedPC1, .fittedPC2) %>%
  kmeans(centers = 3)
covid_k_pca

#augmenting cluster based on PCA to filtered dataset with "raw" cluster and PCA analyses
covid_pca_aug_k_org_pca <- covid_k_pca %>%
  augment(covid_pca_aug_k_org) %>% 
  rename(cluster_pca = .cluster)
covid_pca_aug_k_org_pca


# Comparative plots and .pngs ---------------------------------------------------------

#Comparative plots for absolute deaths

#Plot PCA - colour by COVID deaths
pl1_abs <- covid_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = tertile_deaths)) +
  geom_point() +
  theme(legend.position = "bottom")

#Plot PCA - colour by cluster analysis
pl2_abs <- covid_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_org)) +
  geom_point() +
  theme(legend.position = "bottom")

#Plot PCA - colour by cluster based on PCA
pl3_abs <- covid_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(legend.position = "bottom")

png("results/04_analysis_PCA/pca_cluster_deaths.png", width=8.5,height = 6.5,unit='in',res=300)

(pl1_abs + pl2_abs + pl3_abs)

dev.off()

#Comparative plots of relative deaths using variable dead_cases_per_100000

#Plot PCA - colour by relative COVID deaths
pl1_rel <- covid_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = tertile_rel_deaths)) +
  geom_point() +
  theme(legend.position = "bottom")

#Plot PCA - colour by cluster analysis
pl2_rel <- covid_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_org)) +
  geom_point() +
  theme(legend.position = "bottom")

#Plot PCA - colour by cluster based on PCA
pl3_rel <- covid_pca_aug_k_org_pca %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, colour = cluster_pca)) +
  geom_point() +
  theme(legend.position = "bottom")



png("results/04_analysis_PCA/pca_cluster_rel_deaths.png", width=8.5,height = 6.5,unit='in',res=300)

(pl1_rel + pl2_rel + pl3_rel)

dev.off()

