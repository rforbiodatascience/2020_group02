# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("tidyverse")
library("stringr")
library("lubridate")
library("readxl") 


# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
# my_data <- read_tsv(file = "data/01_my_data.tsv")

COVID_test <- read_tsv(file = "data/01_COVID_test.tsv")

JH_conftime <- read_tsv(file = "data/01_JH_conftime.tsv")
JH_deadtime <- read_tsv(file = "data/01_JH_deadtime.tsv")
JH_recotime <- read_tsv(file = "data/01_JH_recotime.tsv")

POP_demo <- read_tsv(file = "data/01_POP_demo.tsv")
adult_mortality <- read_tsv(file = "data/01_adult_mortality_load.tsv")
life_expectancy <- read_tsv(file = "data/01_life_expectancy_load.tsv")
mortality_causes <- read_tsv(file = "data/01_mortality_causes_load.tsv")

UN_pop <- read_tsv(file = "data/01_UN_pop_raw.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
#my_data_clean <- my_data # %>% ...

##Data in our world data
#COVID-19 tests performed. Global data

COVID_test_clean <- COVID_test %>%
  rename("Country" = "Entity") %>% 
  select(Country, Date, `Cumulative total`, `Cumulative total per thousand`)

#This dont work on the dataset - MCHR001 is working on this
#COVID_test_clean$Date <- as.Date(COVID_test_clean$Date, "%d-%m-%y")


## WHO -Population demographics
# Population size, median Pop age, urban distribution

POP_demo_clean <- POP_demo %>%  
  mutate(`Population (in thousands) total numeric` = str_replace(`Population (in thousands) total`, " ", "")) %>%  
  select(-c(`Population living on &lt;$1 (PPP int. $) a day (%)`, `Population (in thousands) total`)) %>%
  filter(Year %in% c("2020", "2013", "2016")) 

## Johns Hopkins COVID data
# Confirmed COVID-19 cases, in time series. Global data _MCHR001 is working on this

#Tidy data by pivot_longer
JH_conftime_clean <- JH_conftime 

JH_conftime_clean <- JH_conftime_clean %>% 
  unite(Lat, Long,
              col = Lat_Long, sep = "/") %>% 
    mutate(Lat_Long = case_when(
      str_detect(Lat_Long, "-35.4735/149.0124") ~ "-33.8688/151.2093", 
      str_detect(Lat_Long, "-28.0167/153.4") ~ "-33.8688/151.2093",  
      str_detect(Lat_Long, "-41.4545/145.9707") ~ "-33.8688/151.2093", 
      str_detect(Lat_Long, "-37.8136/144.9631") ~ "-33.8688/151.2093",
      str_detect(Lat_Long, "-31.9505/115.8605") ~ "-33.8688/151.2093", 
      str_detect(Lat_Long, "-12.4634/130.8456") ~ "-33.8688/151.2093",
      str_detect(Lat_Long, "-34.9285/138.6007")~ "-33.8688/151.2093",
      str_detect(Lat_Long, "53.9333/-116.5765") ~ "49.2827/-123.1207",
      str_detect(Lat_Long, "37.6489/-122.6655") ~ "49.2827/-123.1207",
      str_detect(Lat_Long, "53.7609/-98.8139") ~ "49.2827/-123.1207",
      str_detect(Lat_Long, "46.5653/-66.4619") ~ "49.2827/-123.1207",
      str_detect(Lat_Long, "53.1355/-57.6604") ~ "49.2827/-123.1207",
      str_detect(Lat_Long, "44.682/-63.7443") ~ "49.2827/-123.1207",
      str_detect(Lat_Long, "51.2538/-85.3232") ~ "49.2827/-123.1207",
      str_detect(Lat_Long, "46.5107/-63.4168") ~ "49.2827/-123.1207",
      str_detect(Lat_Long, "52.9399/-73.5491") ~ "49.2827/-123.1207",
      str_detect(Lat_Long, "52.9399/-106.4509") ~ "49.2827/-123.1207",
      str_detect(Lat_Long, "31.8257/117.2264") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "30.0572/107.874") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "26.0789/117.9874") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "23.3417/113.4244") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "23.8298/108.7881") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "26.8154/106.8748") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "19.1959/109.7453") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "39.549/116.1306") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "47.862/127.7615") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "33.882/113.614") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "22.3/114.2") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "30.9756/112.2707") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "27.6104/111.7088") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "44.0935/113.9448") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "32.9711/119.455") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "27.614/115.7221") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "43.6661/126.1923") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "41.2956/122.6085") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "22.1667/113.55") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "37.2692/106.1655") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "35.7452/95.9956") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "35.1917/108.8701") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "36.3427/118.1498") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "31.202/121.4491") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "37.5777/112.2922") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "30.6171/102.7103") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "39.3054/117.323") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "31.6927/88.0924") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "41.1129/85.2401") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "24.974/101.487") ~ "40.1824/116.4142",
      str_detect(Lat_Long, "29.1832/120.0934") ~ "40.1824/116.4142",
      TRUE ~ Lat_Long
      )
) %>% 
  select(-`Province/State`) %>% 
  separate(Lat_Long, into = c("Lat", "Long"), sep ="/", ) %>%
  pivot_longer(names_to = "date", 
               values_to = "Number of confirmed COVID-19",
               cols = -c("Country/Region", "Lat", "Long"),
             names_ptypes = list(date = character())
             )
 
  JH_conftime_clean <- JH_conftime_clean %>%
  group_by(`Country/Region`, Lat, Long, date) %>% 
  summarise(conf_COVID = sum(`Number of confirmed COVID-19`)) 
  

# Removal of states/provinces that is not the capital.

  
#UN datasets
UN_pop_clean <- UN_pop %>%
    select(X2, Year, Series, Value) %>%
  rename("Country_Region" = "X2") %>%
  filter(Year == 2019, Series == "Population density" | Series == "Sex ratio (males per 100 females)" | Series == "Population aged 60+ years old (percentage)") %>%
  pivot_wider(names_from = Series, values_from = Value)

##WHO - mortality
#Adult mortality
adult_mortality_clean <- adult_mortality  %>% 
  filter(Year == 2016) %>% 
  select(Country, `Adult mortality rate`) 

#Adult_mortality rate corresponds to the probability of dying between age 15 and 60 per 1000 individuals

#Life expectancy and healthy life expectancy  
life_expectancy_clean <- life_expectancy  %>% 
  filter(Year == 2016) %>% 
  select(Country, `Life expectancy at birth (years)`, `Healthy life expectancy (HALE) at birth (years)`)


#Cause specific mortality
mortality_causes <- read_tsv(file = "data/01_mortality_causes_load.tsv") 

mortality_causes_clean <- mortality_causes %>% 
  as_tibble(mortality_causes_clean) %>% 
  rename(Cause_1 = "...5", Cause_2 = "...6") %>% 
  mutate(Cause_1_chr = str_replace(Cause_1, "\\d+\\.", ""))

mortality_causes

# Write data
# ------------------------------------------------------------------------------
#write_tsv(x = my_data_clean, path = "data/02_my_data_clean.tsv")

write_tsv(x = adult_mortality_clean,
          path = "data/02_adult_mortality_clean.tsv")
write_tsv(x = life_expectancy_clean,
          path = "data/02_life_expectancy_clean.tsv")
