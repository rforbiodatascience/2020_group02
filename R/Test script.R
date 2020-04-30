list_of_dfs <- list(adult_mortality_clean, air_pollution_clean, bmi_above30_clean, COVID_test_clean, 
                    handwashing_facilities_clean, health_expenditure_clean, health_infrastructure_clean,
                    household_pollution_clean, JH_conftime_clean, JH_deadtime_clean, JH_recotime_clean,
                    life_expectancy_clean, measles_cases_clean, medical_doctors_clean, mortality_causes_clean,
                    mortality_pollution_related_clean, nurses_midwifes_clean, POP_demo_clean, sex_leader_clean,
                    smoking_clean, UN_gdp_clean, UN_pop_clean)

list_of_dfs_test <- list(adult_mortality_clean)



#Anti-join with copy-pasting
countries_diff_adult_mortality <- adult_mortality_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_air_pollution <- air_pollution_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_bmi <- bmi_above30_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_covid_test <- COVID_test_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_handwashing_facilities <- handwashing_facilities_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_health_expenditure <- health_expenditure_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_health_infrastructure <- health_infrastructure_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_household_pollution <- household_pollution_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_JH_deadtime <- JH_deadtime_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_JH_recotime <- JH_recotime_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_life_expectancy <- life_expectancy_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_measles_cases <- measles_cases_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_medical_doctors <- medical_doctors_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_mortality_causes <- mortality_causes_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_mortality_pollution <- mortality_pollution_related_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_nurses_midwifes <- nurses_midwifes_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_pop_demo <- POP_demo_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_sex_leaders <- sex_leader_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_smoking <- smoking_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_un_gdp <- UN_gdp_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

countries_diff_un_pop <- UN_pop_clean %>% 
  anti_join(JH_conftime_clean, by = 'country') %>% 
  count(country, sort = TRUE)

#Combine countries from different sources 
country_diff_dfs <- mget(ls(pattern = "countries_diff_.+"))

countries_diffs_combined <- bind_rows(country_diff_dfs, .id = "origin") %>% 
  group_by(country) %>% 
  arrange(origin) %>% 
  slice(1) %>%  
  ungroup() %>% 
  arrange(country) %>% 
  select(origin, country)


#For loop for anti-joining on datasets to identify naming differences for countries

list_of_dataframes <- replicate(22, data.frame())
for (i in seq_along(dfs)) {
  list_of_dataframes[[i]] <- anti_join(dfs[[i]], JH_conftime_clean, by = "country") %>% 
    count(country, sort = TRUE)
}

country_differences_new <- bind_rows(list_of_dataframes, .id = "origin") %>% 
  mutate(origin = recode(origin, "1" = "adult_mortality", "2" = "air_pollution", "3" = "bmi", "4" = "COVID_test", "5" = "handwashing_facilities",
                         "6" = "health_expenditure", "7" = "health_infrastructure", "8" = "household_pollution", "12" = "life_expectancy",
                         "13" = "measles_cases", "14" = "medical_doctors", "15" = "mortality_causes", "16" = "mortality_pollution", "17" = "nurses_midwifes",
                         "18" = "pop_demo", "19" = "sex_leader",  "20" = "smoking", "21" = "un_gdp", "22" = "un_pop")) %>% 
  group_by(country) %>% 
  arrange(origin) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(country) %>% 
  select(origin, country)



#Packing it up in a function
test.diff <- function(x) {
  for (i in seq_along(dfs) {
      anti_join(dfs[[i]], JH_conftime_clean, by = "country") %>% 
      count(country, sort = TRUE)
  }
  return(countries_diff_[[i]])
}

test.diff(dfs)


#Applying country function
adult_mortality_clean_aug <- adult_mortality_clean %>% 
  mutate(country_diff =(country_translate(country))) %>% 
  mutate(country = ifelse(!is.na(country_diff),country_diff,country)) %>% 
  select(-country_diff)

dfs_corr_countries <- dfs %>%
  map(~mutate(., country_diff = (country_translate(country))) %>% 
        mutate(country = if_else(!is.na(country_diff), country_diff, country)) %>% 
        select(-country_diff))

jh_combined <- dfs_corr_countries %>% 
  reduce(left_join, by = "country")

lapply(dfs, function(x) {
  mutate(x, country_diff = (country_translate(country))) %>% 
    mutate(country = if_else(!is.na(country_diff), country_diff, country)) %>% 
    select(-country_diff)
})

list_of_dfs_translate <- replicate(22, data.frame())
for (i in seq_along(dfs)) {
  list_of_dataframes[[i]] <- mutate(dfs[[i]], country_diff = (country_translate("country"))) %>% 
    mutate(country = if_else(!is.na(country_diff), country_diff, country)) %>% 
    select(-country_diff)
}

mutate(country = if_else(!is.na(country_diff), country_diff, country)) %>% 
  select(-country_diff)