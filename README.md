Influence of demographic characteristics on COVID-19 kinetics
================
Mette Christensen, Signe Modvig Stausbøll, Hans Jakob Hartling, Mette
Christoffersen

## Description

This is a project on the influence of demographic characteristics on
COVID-19 kinetics, which was performed as a part of “R for Bio Data
Science” at DTU in the spring of 2020. The project includes an
exploratory data analyses, survival analysis, PCA clustering, and a
Shiny app developed on the data.

## Data

The data for this project includes COVID-19 data from Johns Hopkins
University
(<https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data>)
combined with publicly available data on population demographics, public
health measures, health system capacity, COVID-19 tests, and
cause-specific mortality for countries around the world. These
additional data has been retrieved from the World Health Organization
(<https://www.who.int/data/gho>), United Nations (<http://data.un.org/>)
and Our World in Data (<https://ourworldindata.org/>). Data has been
updated with follow-up until May 4th 2020.

Original data is available in this repository in /data/\_raw

## List of variables

**number\_of\_confirmed\_covid-19**: Number of confirmed COVID-19 cases.

**number\_of\_covid-19\_related\_deaths**: Number of confirmed COVID-19
related deaths.

**recovered\_from\_covid-19\_no.**: Number of recovered cases of
COVID-19.

**adult\_mortality\_rate**: The probability of dying between the ages of
15 and 60 years (per 1 000 population) per year among a hypothetical
cohort of 100 000 people that would experience the age-specific
mortality rate of the reporting year.

**bmi\_above30\_prevalence\_all**: Percentage of defined population with
a body mass index (BMI) of 30 kg/m2 or higher.

**cardiovascular\_diseases**: Mortality from cardiovascular disease in
percent.

**concentration\_fine\_particles**: The mean annual concentration of
fine suspended particles of less than 2.5 microns in diameters is a
common measure of air pollution. The mean is a population-weighted
average for urban population in a country.

**confirmed\_cases\_per\_100000**: Number of confirmed COVID-19 cases
per 100000 inhabitants.

**country**: Country

**cumulative\_covid\_test**: Cumulative number of COVID-19 tests.

**current\_health\_expenditure\_per\_person\_usd**: Level of general
government expenditure on health (GGHE) expressed as a percentage of
total government expenditure.

**date**: Date

**date\_28\_days\_after\_100\_cases**: Date 28 days after reaching 100
cases.

**days\_from\_100\_cases\_to\_100\_deaths**: Number of days from
reaching 100 cases to reaching 100 deaths.

**days\_from\_100\_cases\_to\_1000\_deaths**: Number of days from
reaching 100 cases to reaching 1000 deaths.

**days\_from\_dec1\_to\_100\_cases**: Number of days from December 1st
2019 to reaching 100 cases.

**days\_to\_hundred\_cases**: Number of days until reaching 100 cases.

**days\_to\_thousand\_cases**: Number of days until reaching 1000 cases.

**dead\_cases\_per\_100000**: Number of COVID-19 related deaths per
100000 inhabitants.

**deaths\_28\_days\_after\_100\_cases**: Number of COVID-19 related
deaths 28 days after reaching 100 cases.

**deaths\_28\_days\_per\_100000**: Number of COVID-19 related deaths per
100000 inhabitants.

**density\_of\_hospitals**: Number of hospitals, including the following
hospital categories: rural and district, provincial (second level
referral), regional/specialized/teaching and research hospitals
(tertiary care), from the public and private sectors, per 100,000
population.

**density\_of\_medical\_doctors**: Medical doctors per 10000
inhabitants.

**density\_of\_nurses\_midwifes**: Nurses and midwifes per 10000
inhabitants.

**first\_case**: Date for appearance of first case.

**first\_death**: Date for first COVID-19 related death.

**gdp\_in\_current\_prices\_millions\_of\_us\_dollars**: Gross domestic
product in current prices (Millions USD)

**gdp\_per\_capita\_us\_dollars**: Gross domestic product per capita in
US dollars.

**healthy\_life\_expectancy**:

**hundred\_cases**: Date for reaching 100 cases.

**hundred\_deaths**: Date for reaching 100 deaths.

**ischaemic\_heart\_disease**: Mortality from ischaemic heart disease in
percent.

**kidney\_diseases**: Mortality from kidney diseases in percent.

**lat**: Latitude for country.

**life\_expectancy**:

**long**: Longitude for country.

**malignant\_neoplasms**: Mortality from malignant neoplasms in percent.

**measles\_reported\_cases**:

**number\_of\_confirmed\_covid-19**:

**pollution\_attributable\_death\_rate**:

**pollution\_attributable\_death\_rate\_std**:

**population\_aged\_60\_years\_old\_percentage**:

**population\_density**:

**population\_in\_thousands\_total**:

**population\_living\_in\_urban\_areas**:

**population\_median\_age\_years**:

**population\_proportion\_over\_60**:

**population\_proportion\_under\_15**:

**prevalence\_smoking**: Percentage of population above age 15 years
smoking any tobacco product.

**proportion\_basic\_handwashing\_facilities**: The percentage of
population living in households that have a handwashing facility with
soap and water at home.

**recov\_cases\_per\_100000**:

**recovered\_from\_covid-19\_no.**

**respiratory\_diseases**: Mortality from respiratory diseases in
percent.

**respiratory\_infectious**: Mortality from respiratory infections in
percent.

**road\_injury**: Mortality from road injuries in percent.

**sex**:

**sex\_ratio\_males\_per\_100\_females**:

**test\_cases\_per\_100000**:

**thousand\_cases**:

**thousand\_deaths**:

## Packages
