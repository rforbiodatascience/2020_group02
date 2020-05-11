# Clear workspace
# ------------------------------------------------------------------------------
rm(list = ls())

# Load libraries
# ------------------------------------------------------------------------------
library("DiagrammeR")


# Define functions
# ------------------------------------------------------------------------------
# source(file = "R/99_project_functions.R")

# Load data
# ------------------------------------------------------------------------------
covid_aug <- read_tsv(file = "data/03_covid_aug.tsv",
                      col_types = cols(thousand_deaths = col_date(),
                                       `days_from_100_cases_to_1000_deaths` = col_double()))



# Flowchart with study design ----------------------------------------------

grViz("digraph {
  
graph[compound = true, color = crimson, penwidth = 3]
node [fontname = helvetica, fontsize = 20, shape = rectangle, style = filled, fillcolor = White, width = 6, height = 0.75,  penwidth = 2.0]
edge [color = black, arrowhead = none, arrowtail = none]

data1 [label = 'Johns Hopkins data', shape = folder, fillcolor = Lightblue]
data2 [label = 'WHO', shape = folder, fillcolor = Lightblue]
data3 [label = 'UN', shape = folder, fillcolor = Lightblue]
data4 [label = 'Our world in data', shape = folder, fillcolor = Lightblue]

cleaning [label =  'Combined database', width = 8]

exp1 [label = 'Life expectancy']
exp2 [label = 'Pollution attributable death rate']
exp3 [label = 'Health expenditure per person']
exp4 [label = 'Proportion of population living in urban areas']
exp5 [label = 'Proportion of population above 60 years']
exp6 [label = 'Mortality from respiratory diseases']
outcome1 [label = 'Days from December 1st 2019 to 100 cases \n as a measure of spread of COVID-19', fillcolor = white]
outcome2 [label = 'Days from 100 cases to 100 deaths \n as a measure of health system readiness/capacity', fillcolor = white]


subgraph cluster1 {
  label = 'Selected exposure variables'; fontname = helvetica; nodesep = 0.75; ranksep = 0.75; width = 3
  'exp1' -> 'exp2' -> 'exp3' -> 'exp4' -> 'exp5' -> 'exp6'
}

subgraph cluster2 {
  label = 'Selected outcome variables'; fontname = helvetica; nodesep = 0.75; ranksep = 0.75; width = 3
  'outcome1' -> 'outcome2'
}


{data1 data2 data3 data4} -> cleaning;
cleaning -> 'outcome1' [lhead = cluster2]
cleaning -> 'exp1' [lhead = cluster1]
}")



# Flowchart with variables ------------------------------------------------

grViz("digraph {
  
graph [compound = true, nodesep = 0.75, ranksep = .5, color = crimson, penwidth = 3]
node [fontname = helvetica, fontsize = 16, fontcolor = black, shape = rectangle, fixedsize = TRUE, height = 1, penwidth = 2.0, color = black]
edge [color = black, arrowhead = none, arrowtail = none]

subgraph cluster1 {
    node [width = 3, height = 0.75, ranksep = 0.5]
    'Country' -> 'COVID-19 cases' -> 'COVID-19 deaths' -> 'COVID-19 recovered cases'
}

subgraph cluster2 {
    node [fixedsize = TRUE, width = 3, height = 0.75, ranksep = 0.5]
    'GDP' -> 'GDP per capita' 
}

subgraph cluster3 {
    node [fixedsize = TRUE, width = 3, height = 0.75, ranksep = 0.5]
    'COVID-19 tests' 
}

subgraph cluster4 {
    label = 'Population demographics'; fontname = helvetica
    node [fixedsize = TRUE, width = 3, height = 0.75, ranksep = 0.5]
    'Population size' -> 'Population density' -> 'Proportion of population \n above 60 years' -> 'Proportion of population \n living in urban areas'
}

subgraph cluster5 {
    label = 'Health system'; fontname = helvetica
    node [fixedsize = true, width = 3]
    'Density of hospitals' -> 'Density of medical doctors' -> 'Density of nurses/midwifes' -> 'Health expenditure per person'
}

subgraph cluster6 {
    label = 'Public health and environment'; fontname = helvetica
    node [fixedsize = true, width = 3]
    'Body mass index' -> 'Air pollution' -> 'Pollution attributable death rate' -> 'Smoking'
}

subgraph cluster7 {
    label = 'Lifespan and mortality'; fontname = helvetica; size = 0.75
    node [fixedsize = true, width = 3]
    'Adult mortality rate' -> 'Healthy life span' -> 'Cause specific mortality'  
}
  
  JH [label = 'Johns Hopkins \n COVID-19 data', width = 3, fontsize = 20]
  UN [label = 'UN datasets', width = 3, fontsize = 20]
  our_world_in_data [label = 'Our World In Data', width = 3, fontsize = 20]
  WHO [label = 'WHO datasets', width = 3, fontsize = 20]
  
  JH -> 'Country' [lhead = cluster1]
  UN -> GDP [lhead = cluster2]
  our_world_in_data -> 'COVID-19 tests' [lhead = cluster3]
  WHO -> 'Population size' [lhead = cluster4]
  WHO -> 'Density of hospitals' [lhead = cluster5]
  WHO -> 'Body mass index' [lhead = cluster6]
  WHO -> 'Adult mortality rate' [lhead = cluster7]

}")







