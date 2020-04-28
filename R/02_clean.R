# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
#Check om dette kan slås sammen til færre pakker
library("tidyverse")
library("stringr")
library("lubridate")
library("readxl") 
library("readr")
library("tidyr")

# Define functions
# ------------------------------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
#Tilføj til readme fil at vi har besluttet at anvende nyeste datapunkt for hvert variabel
#Check "summarise_each(funs(first(.[!is.na(.)])))" brugt i population demographics
#Check muligheden for at supplere data med ældre non-missing values, hvis nyeste værdi er missing (Smoking)
#Funktion til latitude og longitude data for lande med flere regioner
#Load samtlige datasæt i toppen

COVID_test <- read_tsv(file = "data/01_COVID_test.tsv", 
                       col_types = cols(Date = col_date(format="%d-%m-%Y")))

JH_conftime <- read_tsv(file = "data/01_JH_conftime.tsv")
JH_deadtime <- read_tsv(file = "data/01_JH_deadtime.tsv")
JH_recotime <- read_tsv(file = "data/01_JH_recotime.tsv")

POP_demo <- read_tsv(file = "data/01_POP_demo.tsv")

adult_mortality <- read_tsv(file = "data/01_adult_mortality_load.tsv")
life_expectancy <- read_tsv(file = "data/01_life_expectancy_load.tsv")
mortality_causes <- read_tsv(file = "data/01_mortality_causes_load.tsv")

air_pollution <- read_tsv(file = "data/01_air_pollution_load.tsv")
handwashing_facilities <- read_tsv(file = "data/01_handwashing_facilities_load.tsv")
household_pollution <- read_tsv(file = "data/01_household_pollution_load.tsv")
measles_cases <- read_tsv(file = "data/01_measles_cases_load.tsv")
mortality_pollution_related <- read_tsv(file = "data/01_mortality_pollution_related_load.tsv")

health_expenditure <- read_tsv(file = "data/01_health_expenditure_load.tsv")
health_infrastructure <- read_tsv(file = "data/01_health_infrastructure_load.tsv")
medical_doctors <- read_tsv(file = "data/01_medical_doctors_load.tsv")
nurses_midwifes <- read_tsv(file = "data/01_nurses_midwifes_load.tsv")

smoking <- read_tsv(file = "data/01_smoking_load.tsv")

UN_pop <- read_tsv(file = "data/01_UN_pop_raw.tsv")
UN_gdp <- read_tsv(file = "data/01_UN_gdp_raw.tsv")

sex_leader <- read_tsv(file = "data/01_sex_leader_raw.tsv")

BMI_above30 <- read_tsv(file = "data/01_BMI_above30_agestand_raw.tsv")

# Wrangle data
# ------------------------------------------------------------------------------
##Data in our world data
#COVID-19 tests performed (cummulative data over time). Global data.

COVID_test_clean <- COVID_test %>%
  separate(Entity, into = c("Country", "waste"), sep ="-", ) %>% 
  select(Country, Date, `Cumulative total`, `Cumulative total per thousand`)

-------------------------------------------------------------------------------
## WHO -Population demographics
# Population size, median Pop age, urban distribution, % pop > 60 years and < 15 years. Dataset composed 2013, 2016 and 2020 - and collapsed by omitting the variable "Year" and NA. 
#uses str_replace_all (and not only str_replace) because of more than one ws in population of China and India.
POP_demo_clean <- POP_demo %>% 
  mutate(`Population (in thousands) total` = str_replace_all(`Population (in thousands) total`, " ", "")) %>%
  select(-c(`Population living on &lt;$1 (PPP int. $) a day (%)`)) %>%
  filter(Year %in% c("2020", "2013", "2016")) %>% 
  select(-Year) %>% 
  group_by(Country) %>%
  summarise_each(funs(first(.[!is.na(.)])))
  
# summarise_each(funs(first(.[!is.na(.)]))) er fundet via stackoverflow (https://stackoverflow.com/questions/28509462/how-to-collapse-many-records-into-one-while-removing-na-values) - virker men jeg forstår den ikke. Følg op på det! MCHR001
  -------------------------------------------------------------------------------
## Johns Hopkins COVID data
  
# Confirmed COVID-19 cases, in time series. 
# Unifying latitude and longitude for transformation to state of capital, and tidying data by pivot_long
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
      TRUE ~ Lat_Long)) %>% 
  select(-`Province/State`) %>% 
  separate(Lat_Long, into = c("Lat", "Long"), sep ="/", ) %>%
  pivot_longer(names_to = "date", 
               values_to = "Number of confirmed COVID-19",
               cols = -c("Country/Region", "Lat", "Long"),
             names_ptypes = (date = date())
             )
 
  JH_conftime_clean <- JH_conftime_clean %>%
  group_by(`Country/Region`, Lat, Long, date) %>% 
  summarise(conf_COVID = sum(`Number of confirmed COVID-19`))  

  
    
# COVID-19 deaths, in time series. 
# Unifying latitude and longitude for transformation to state of capital, and tidying data by pivot_long  
  JH_deadtime_clean <- JH_deadtime 
  
  JH_deadtime_clean <- JH_deadtime_clean %>% 
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
      TRUE ~ Lat_Long)) %>% 
    select(-`Province/State`) %>% 
    separate(Lat_Long, into = c("Lat", "Long"), sep ="/", ) %>%
    pivot_longer(names_to = "date", 
                 values_to = "Number of COVID-19 related deaths",
                 cols = -c("Country/Region", "Lat", "Long"),
                 names_ptypes = (date = date())
    )
  
  JH_deadtime_clean <- JH_deadtime_clean %>%
    group_by(`Country/Region`, Lat, Long, date) %>% 
    summarise(deaths_COVID = sum(`Number of COVID-19 related deaths`))
  

# COVID-19 recovery, in time series. 
# Unifying latitude and longitude for transformation to state of capital, and tidying data by pivot_long    
  
  JH_recotime_clean <- JH_recotime 
  
  JH_recotime_clean <- JH_recotime_clean %>% 
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
      TRUE ~ Lat_Long)) %>% 
    select(-`Province/State`) %>% 
    separate(Lat_Long, into = c("Lat", "Long"), sep ="/", ) %>%
    pivot_longer(names_to = "date", 
                 values_to = "Recovered from COVID-19 (no.)",
                 cols = -c("Country/Region", "Lat", "Long"),
                 names_ptypes = (date = date()))
  
  JH_recotime_clean <- JH_recotime_clean %>%
    group_by(`Country/Region`, Lat, Long, date) %>% 
    summarise(recov_COVID = sum(`Recovered from COVID-19 (no.)`)) 
  
------------------------------------------------------------------------------------------------
  
#UN datasets
UN_pop_clean <- UN_pop %>%
  select(X2, Year, Series, Value) %>%
  rename("country" = "X2") %>%
  filter(Year == 2019, Series == "Population density" | Series == "Sex ratio (males per 100 females)" | Series == "Population aged 60+ years old (percentage)") %>%
  pivot_wider(names_from = Series, values_from = Value) %>%
  select(Country_Region, 'Population density', 'Sex ratio (males per 100 females)', 'Population aged 60+ years old (percentage)')


UN_gdp_clean <- UN_gdp %>%
  separate("[T13.]_Region/Country/Area", into = c("nr", "country", "Year", "Series", "Value", "footnotes", "source"), sep = ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")
  UN_gdp_clean <-  as.data.frame(sapply(UN_gdp_clean, function(x) gsub("\"", "", x))) %>%
  filter(Year == 2017, Series == "GDP in current prices (millions of US dollars)" | Series == "GDP per capita (US dollars)") %>%
  pivot_wider(names_from = Series, values_from = Value) %>%
  select(country, 'GDP in current prices (millions of US dollars)', 'GDP per capita (US dollars)')



#Gender leader
sex_leader_clean <- sex %>% 
  filter(year == 2020 & month == 4) %>%
  mutate(sex = case_when(`male` == 1 ~ "male",
                         `male` == 0 ~ "female")) %>%
  select(country, sex)


##WHO - mortality
#--------------------------------------------------------------------------------------------
#Adult mortality
adult_mortality_clean <- adult_mortality %>% 
  filter(Year == 2016) %>% 
  select(Country, "Adult mortality rate (probability of dying between 15 and 60 years per 1000 population)_Both sexes") %>% 
  rename(country = Country,
         adult_mortality_rate = "Adult mortality rate (probability of dying between 15 and 60 years per 1000 population)_Both sexes")

#Adult_mortality rate corresponds to the probability of dying between age 15 and 60 per 1000 individuals in 2016

#Life expectancy and healthy life expectancy  
life_expectancy_clean <- life_expectancy %>% 
  filter(Year == 2016) %>% 
  select(Country, "Life expectancy at birth (years)_Both sexes", "Healthy life expectancy (HALE) at birth (years)_Both sexes") %>% 
  rename(country = Country,
         life_expectancy = "Life expectancy at birth (years)_Both sexes",
         healthy_life_expectancy = "Healthy life expectancy (HALE) at birth (years)_Both sexes")

#Life expectancy and healthy life expectancy corresponds to at birth estimates from the year 2016


#CAUSE-SPECIFIC MORTALITY

#read dataset to object
mortality_causes <- read_tsv(file = "data/01_mortality_causes_load.tsv") 

#Renaming variables, removing gender-specifc rows and unnecessary variables 
  mortality_causes_clean <- mortality_causes %>% 
  as_tibble(mortality_causes_clean) %>% 
  rename(Cause_1 = "...5", Cause_2 = "...6") %>% 
  filter(Sex=="Persons") %>% 
  select(-'Sex', -'GHE code', -'Member State
(See Notes for explanation of colour codes)', -'GHE cause', -'...3') %>% 

#Uniting primary and secondary causes of disease with "_" 
  unite("Cause_clean", Cause_1:Cause_2, sep = "_", remove = TRUE, na.rm = T) %>% 
  select(Cause_clean, everything()) %>% 

#Removing excess digits/letters, removing blank rows from subtypes of diseases, assigning row ID no
  mutate(Cause_clean = str_replace(Cause_clean, "^\\w+\\.", "")) %>% 
  mutate(Cause_clean = str_replace(Cause_clean, "^_", "")) %>% 
  mutate_all(na_if,"") %>% 
  filter(!is.na(Cause_clean)) %>% 
  rowid_to_column("ID") %>% 

#Transposing table using pivot
  pivot_longer(
    cols = -c("ID", "Cause_clean"), 
    names_to = "Country", 
    values_to = "Result") %>% 
  select(-"ID") %>% 
  pivot_wider(
    names_from = Cause_clean, 
    values_from = Result) %>% 

#Turning character variables into numeric
  mutate_all(na_if(.,"^\\.$"))
  mutate_all(~str_replace_all(., "^\\.$", "0")) %>% 
  mutate_all(type.convert, as.is=TRUE)

#check successful cleaning 
mortality_causes_clean

#Test of above regular expressions as strings
writeLines("^\\w+\\.")
writeLines("^\\.$")



#BMI
BMI_above30_clean <- BMI_above30  %>% 
  separate(BMI_above30_all, into = c("BMI_above30_prevalence_all", "ref_int_all"), sep = " ") %>%
  select(Country, "BMI_above30_prevalence_all") %>%
  mutate(BMI_above30_prevalence_all = as.numeric(BMI_above30_prevalence_all))


##WHO - public health and environment
#-------------------------------------------------------------------------------------------------------
#Air pollution
air_pollution_clean <- air_pollution %>% 
  separate("Concentrations of fine particulate matter (PM2.5)_2016_Total",
           into = c("concentration_fine_particles", "ref_interval"), sep = " ") %>%
  select(Country, concentration_fine_particles) %>% 
  rename(country = Country) %>% 
  mutate(concentration_fine_particles = as.numeric(concentration_fine_particles))

#Handwashing facilities
handwashing_facilities_clean <- handwashing_facilities %>%
  select(Country, "2017_Population with basic handwashing facilities at home (%)_Total") %>% 
  rename(country = Country,
         proportion_basic_handwashing_facilities = "2017_Population with basic handwashing facilities at home (%)_Total")

#Household pollution - clean fuel technologies
household_pollution_clean <- household_pollution %>% 
  select(Country, "Proportion of population with primary reliance on clean fuels and technologies (%)_2017") %>% 
  rename(country = Country,
         proportion_using_clean_fuels = "Proportion of population with primary reliance on clean fuels and technologies (%)_2017") %>% 
  mutate(proportion_using_clean_fuels = recode(proportion_using_clean_fuels, "&gt;95"="100", "&lt;5" = "1")) %>% 
  mutate(proportion_using_clean_fuels = as.numeric(proportion_using_clean_fuels))

#Measles reported cases
measles_cases_clean <- measles_cases %>% 
  select(Country, "Measles - number of reported cases_2018") %>% 
  rename(country = Country,
         measles_reported_cases = "Measles - number of reported cases_2018") %>% 
  mutate(measles_reported_cases = as.numeric(measles_reported_cases))

#Mortality from environmental pollution
mortality_pollution_related_clean <- mortality_pollution_related %>%
  separate("Ambient and household air pollution attributable death rate (per 100 000 population)_2016_Both sexes", 
           into = c("pollution_attributable_death_rate", "ref_interval"), sep = "\\[") %>%
  separate("Ambient and household air pollution attributable death rate (per 100 000 population, age-standardized)_2016_Both sexes", 
           into = c("pollution_attributable_death_rate_std", "ref_interval_std"), sep = "\\[") %>% 
  select(Country, pollution_attributable_death_rate, pollution_attributable_death_rate_std) %>% 
  rename(country = Country) %>% 
  mutate(pollution_attributable_death_rate = as.numeric(pollution_attributable_death_rate)) %>% 
  mutate(pollution_attributable_death_rate_std = as.numeric(pollution_attributable_death_rate_std))


##WHO - Health workforce and system
#-------------------------------------------------------------------------------------------------------
#Current health expenditure
health_expenditure_clean <- health_expenditure %>% 
  select(Country, "2017_Current health expenditure (CHE) per capita in US$") %>% 
  rename(country = Country,
         current_health_expenditure_per_person_USD = "2017_Current health expenditure (CHE) per capita in US$")

#Definition: Per capita current expenditures on health expressed in respective currency - US dolar. 
#Rationale: This indicator calculates the average expenditure on health per person. It contributes to understand the health expenditure relative to the population size facilitating international comparison.

#Health infrastructure
health_infrastructure_clean <- health_infrastructure %>% 
  filter(Year == "2013") %>% 
  select(Country, "Total density per 100 000 population: Hospitals") %>% 
  rename(country = Country,
         density_of_hospitals = "Total density per 100 000 population: Hospitals")

#Definition: Number of hospitals, including the following hospital categories: rural and district, provincial (second level referral), regional/specialized/teaching and research hospitals (tertiary care), from the public and private sectors, per 100,000 population.

#Medical doctors
medical_doctors_clean <- medical_doctors %>% 
  group_by(Country) %>% 
  arrange(desc(Year)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(Country, "Medical doctors (per 10 000 population)") %>% 
  rename(country = Country,
         density_of_medical_doctors = "Medical doctors (per 10 000 population)")

#Definition: Medical doctors per 10000 inhabitants. Includes generalists , specialist medical practitioners and medical doctors not further defined, in the given national and/or subnational area.
  
#Nurses and midwifes
nurses_midwifes_clean <- nurses_midwifes %>%
  group_by(Country) %>% 
  arrange(desc(Year)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(Country, "Nursing and midwifery personnel (per 10 000 population)") %>% 
  rename(country = Country,
         density_of_nurses_midwifes = "Nursing and midwifery personnel (per 10 000 population)")

#Definition: Nurses and midwifes per 10000 inhabitants. 


##WHO - Smoking
#-------------------------------------------------------------------------------
#Read dataset to object
smoking_clean <- smoking %>% 
  
#Separate into variables of prevalence and confidence interval and remove unnecessary variables and rename
  separate("Prevalence of smoking any tobacco product among persons aged &gt;= 15 years_Male", 
           into = c("prevalence_smoking_males", "ref_interval_males"), sep = " ") %>% 
  separate("Prevalence of smoking any tobacco product among persons aged &gt;= 15 years_Female", 
           into = c("prevalence_smoking_females", "ref_interval_females"), sep = " ")  %>% 
  mutate(prevalence_smoking_males = as.numeric(prevalence_smoking_males)) %>% 
  mutate(prevalence_smoking_females = as.numeric(prevalence_smoking_females)) %>% 
  select(Country, Year, prevalence_smoking_males, prevalence_smoking_females) %>% 
  rename(country = Country) %>% 

#Remove predicted future prevalence of smoking
  filter(Year != "2025") %>% 
  
#Limiting dataset to most recent observation
  group_by(country) %>% 
  arrange(desc(Year)) %>% 
  slice(1) %>% 
  ungroup %>% 

#Combining smoking prevalence to overall measure for males and females combined
  mutate(prevalence_smoking = (prevalence_smoking_females+prevalence_smoking_males)/2) %>%

#Removing unnecessary variables
  select(country, prevalence_smoking)  

#Definition: Percentage of population above age 15 years smoking any tobacco product.




# Write data
# ------------------------------------------------------------------------------
#write_tsv(x = my_data_clean, path = "data/02_my_data_clean.tsv")

write_tsv(x = COVID_test_clean, 
          path = "data/02_COVID_test_clean.tsv" )
write_tsv(x = POP_demo_clean, 
          path = "data/02_POP_demo_clean.tsv" )
write_tsv(x = JH_conftime_clean, 
          path = "data/02_JH_conftime_clean.tsv" )
write_tsv(x = JH_deadtime_clean, 
          path = "data/02_JH_deadtime_clean.tsv" )
write_tsv(x = JH_recotime_clean, 
          path = "data/02_JH_recotime_clean.tsv" )
write_tsv(x = adult_mortality_clean,
          path = "data/02_adult_mortality_clean.tsv")
write_tsv(x = life_expectancy_clean,
          path = "data/02_life_expectancy_clean.tsv")
write_tsv(x = air_pollution_clean,
          path = "data/02_air_pollution_clean.tsv")
write_tsv(x = handwashing_facilities_clean,
          path = "data/02_handwashing_facilities_clean.tsv")
write_tsv(x = household_pollution_clean,
          path = "data/02_household_pollution_clean.tsv")
write_tsv(x = measles_cases_clean,
          path = "data/02_measles_cases_clean.tsv")
write_tsv(x = mortality_pollution_related_clean,
          path = "data/02_mortality_pollution_related_clean.tsv")
write_tsv(x = health_expenditure_clean,
          path = "data/02_health_expenditure_clean.tsv")
write_tsv(x = health_infrastructure_clean,
          path = "data/02_health_infrastructure_clean.tsv")
write_tsv(x = medical_doctors_clean,
          path = "data/02_medical_doctors_clean.tsv")
write_tsv(x = nurses_midwifes_clean,
          path = "data/02_nurses_midwifes_clean.tsv")
write_tsv(x = smoking_clean,
          path = "data/02_smoking_clean.tsv")
write_tsv(x = UN_pop_clean,
          path = "data/02_UN_pop_clean.tsv")
write_tsv(x = UN_gdp_clean,
          path = "data/02_UN_gdp_clean.tsv")
write_tsv(x = sex_leader_clean,
          path = "data/02_sex_leader_clean.tsv")
write_tsv(x = BMI_above30_clean,
          path = "data/02_BMI_above30_clean.tsv")
write_tsv(x = mortality_causes_clean,
          path = "data/02_mortality_causes_clean.tsv")
