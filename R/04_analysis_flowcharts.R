# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------

library("DiagrammeR")
library("DiagrammeRsvg")
library("rsvg")
library("tidyverse")


# Define functions --------------------------------------------------------

source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

covid_aug <- read_tsv(file = "data/03_covid_aug.tsv",
                      col_types = cols(thousand_deaths = col_date(),
                                       `days_from_100_cases_to_1000_deaths` = col_double()))


# Flowchart with study design ----------------------------------------------

flowchart_design <- grViz("digraph {
  
graph[compound = true, color = crimson, penwidth = 3]
node [fontname = helvetica, fontsize = 30, shape = rectangle, style = filled, fillcolor = White, width = 10, height = 1.2,  penwidth = 2.0]
edge [color = black, arrowhead = none, arrowtail = none]

data1 [label = 'Johns Hopkins data', shape = folder, fillcolor = Lightblue, fontsize = 40]
data2 [label = 'WHO', shape = folder, fillcolor = Lightblue, fontsize = 40]
data3 [label = 'UN', shape = folder, fillcolor = Lightblue, fontsize = 40]
data4 [label = 'Our world in data', shape = folder, fillcolor = Lightblue, fontsize = 40]

cleaning [label =  'Combined database', width = 8]

exp1 [label = 'Life expectancy']
exp2 [label = 'Pollution attributable death rate']
exp3 [label = 'Health expenditure per person']
exp4 [label = 'Proportion of population living in urban areas']
exp5 [label = 'Proportion of population above 60 years']
exp6 [label = 'Mortality from respiratory diseases']
outcome1 [label = 'Days from December 1st 2019 to 100 cases \n as a measure of worldwide spread of COVID-19', fillcolor = white, width = 12]
outcome2 [label = 'Days from 100 cases to 100 deaths \n as a measure of nationwide spread and dealing with COVID-19', fillcolor = white, width = 12]


subgraph cluster1 {
  label = 'Selected exposure variables'; fontname = helvetica; fontsize = 25; nodesep = 0.75; ranksep = 0.75; height = 2 width = 20
  'exp1' -> 'exp2' -> 'exp3' -> 'exp4' -> 'exp5' -> 'exp6'
}

subgraph cluster2 {
  label = 'Selected outcome variables'; fontname = helvetica; fontsize = 25; nodesep = 0.75; ranksep = 0.75; height = 2; width = 10
  'outcome1' -> 'outcome2'
}


{data1 data2 data3 data4} -> cleaning;
cleaning -> 'outcome1' [lhead = cluster2]
cleaning -> 'exp1' [lhead = cluster1]
}")

export_svg(flowchart_design) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("results/04_analysis_flowcharts/flowchart_design.png")

 
# Flowchart - analyses setup----------------------------------------------------

flowchart_analyses <- grViz("digraph {
  
graph[compound = true, color = crimson, penwidth = 3]
node [fontname = helvetica, fontsize = 20, shape = rectangle, style = filled, fillcolor = White, width = 4, height = 0.75,  penwidth = 2.0]
edge [color = black, arrowhead = none, arrowtail = none]

cleaning [label = 'Combined database']
analysis [label = 'Data analyses']
analysis1 [label = 'Simple associations']
analysis2 [label = 'Stratified associations']
analysis3 [label = 'Survival analyses']
analysis4 [label = 'Cluster/PCA analyses']
analysis5 [label = 'Shiny app/maps']
conclusion [label = 'Conclusions']
presentation [label = 'Presentation']


cleaning -> 'analysis'
analysis -> 'analysis1'
analysis -> 'analysis2'
analysis -> 'analysis3'
analysis -> 'analysis4'
analysis -> 'analysis5'
analysis1 -> 'conclusion'
analysis2 -> 'conclusion'
analysis3 -> 'conclusion'
analysis4 -> 'conclusion'
analysis5 -> 'conclusion'
conclusion -> 'presentation'
}")


export_svg(flowchart_analyses) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("results/04_analysis_flowcharts/flowchart_analyses.png")


# Flowchart with variables ------------------------------------------------

variables <- grViz("digraph {
  
graph [compound = true, nodesep = 0.75, ranksep = .5, color = crimson, penwidth = 3]
node [fontname = helvetica, fontsize = 30, fontcolor = black, shape = rectangle, fixedsize = TRUE, width = 2; height = 1.2, penwidth = 2.0, color = black]
edge [color = black, arrowhead = none, arrowtail = none]

subgraph cluster1 {
    node [width = 6, ranksep = 0.5]
    'Country' -> 'COVID-19 cases' -> 'COVID-19 deaths' -> 'COVID-19 recovered cases'
}

subgraph cluster2 {
    node [fixedsize = TRUE, width = 6, ranksep = 0.5]
    'GDP' -> 'GDP per capita' 
}

subgraph cluster3 {
    node [fixedsize = TRUE, width = 6, ranksep = 0.5]
    'COVID-19 tests' 
}

subgraph cluster4 {
    label = 'Population demographics'; fontname = helvetica; fontsize = 25
    node [fixedsize = TRUE, width = 6, ranksep = 0.5]
    'Population size' -> 'Population density' -> 'Proportion of population \n above 60 years' -> 'Proportion of population \n living in urban areas'
}

subgraph cluster5 {
    label = 'Health system'; fontname = helvetica; fontsize = 25
    node [fixedsize = true, width = 6]
    'Density of hospitals' -> 'Density of medical doctors' -> 'Density of nurses/midwifes' -> 'Health expenditure per person'
}

subgraph cluster6 {
    label = 'Public health and environment'; fontname = helvetica; fontsize = 25
    node [fixedsize = true, width = 6]
    'Body mass index' -> 'Air pollution' -> 'Pollution attributable death rate' -> 'Smoking'
}

subgraph cluster7 {
    label = 'Lifespan and mortality'; fontname = helvetica; size = 0.75; fontsize = 25
    node [fixedsize = true, width = 6]
    'Adult mortality rate' -> 'Healthy life span' -> 'Cause specific mortality'  
}
  
  JH [label = 'Johns Hopkins', width = 6, height = 0.75]
  UN [label = 'UN datasets', width = 6, height = 0.75]
  our_world_in_data [label = 'Our World In Data', width = 6, height = 0.75]
  WHO [label = 'WHO datasets', width = 6, height = 0.75]
  
  JH -> 'Country' [lhead = cluster1]
  UN -> GDP [lhead = cluster2]
  our_world_in_data -> 'COVID-19 tests' [lhead = cluster3]
  WHO -> 'Population size' [lhead = cluster4]
  WHO -> 'Density of hospitals' [lhead = cluster5]
  WHO -> 'Body mass index' [lhead = cluster6]
  WHO -> 'Adult mortality rate' [lhead = cluster7]

}")


export_svg(variables) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("results/04_analysis_flowcharts/variables_overview.png")


variables1 <- grViz("digraph {
  
graph [compound = true, nodesep = 0.75, ranksep = .5, color = crimson, penwidth = 3]
node [fontname = helvetica, fontsize = 30, fontcolor = black, shape = rectangle, fixedsize = TRUE, width = 2; height = 1.2, penwidth = 2.0, color = black]
edge [color = black, arrowhead = none, arrowtail = none]

subgraph cluster1 {
    node [width = 6, ranksep = 0.5]
    'Country' -> 'COVID-19 cases' -> 'COVID-19 deaths' -> 'COVID-19 recovered cases'
}

subgraph cluster2 {
    node [fixedsize = TRUE, width = 6, ranksep = 0.5]
    'GDP' -> 'GDP per capita' 
}

subgraph cluster3 {
    node [fixedsize = TRUE, width = 6, ranksep = 0.5]
    'COVID-19 tests' 
}

  
  JH [label = 'Johns Hopkins', width = 6, height = 0.75]
  UN [label = 'UN datasets', width = 6, height = 0.75]
  our_world_in_data [label = 'Our World In Data', width = 6, height = 0.75]
  
  JH -> 'Country' [lhead = cluster1]
  UN -> GDP [lhead = cluster2]
  our_world_in_data -> 'COVID-19 tests' [lhead = cluster3]

}")


export_svg(variables1) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("results/04_analysis_flowcharts/variables_overview1.png")


variables2 <- grViz("digraph {
  
graph [compound = true, nodesep = 0.75, ranksep = .5, color = crimson, penwidth = 3]
node [fontname = helvetica, fontsize = 30, fontcolor = black, shape = rectangle, fixedsize = TRUE, width = 2; height = 1.2, penwidth = 2.0, color = black]
edge [color = black, arrowhead = none, arrowtail = none]


subgraph cluster1 {
    label = 'Population demographics'; fontname = helvetica; fontsize = 25
    node [fixedsize = TRUE, width = 6, ranksep = 0.5]
    'Population size' -> 'Population density' -> 'Proportion of population \n above 60 years' -> 'Proportion of population \n living in urban areas'
}

subgraph cluster2 {
    label = 'Health system'; fontname = helvetica; fontsize = 25
    node [fixedsize = true, width = 6]
    'Density of hospitals' -> 'Density of medical doctors' -> 'Density of nurses/midwifes' -> 'Health expenditure per person'
}

subgraph cluster3 {
    label = 'Public health and environment'; fontname = helvetica; fontsize = 25
    node [fixedsize = true, width = 6]
    'Body mass index' -> 'Air pollution' -> 'Pollution attributable death rate' -> 'Smoking'
}

subgraph cluster4 {
    label = 'Lifespan and mortality'; fontname = helvetica; size = 0.75; fontsize = 25
    node [fixedsize = true, width = 6]
    'Adult mortality rate' -> 'Healthy life span' -> 'Cause specific mortality'  
}
  
  
  WHO [label = 'WHO datasets', width = 6, height = 0.75]
  
  WHO -> 'Population size' [lhead = cluster1]
  WHO -> 'Density of hospitals' [lhead = cluster2]
  WHO -> 'Body mass index' [lhead = cluster3]
  WHO -> 'Adult mortality rate' [lhead = cluster4]

}")


export_svg(variables2) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("results/04_analysis_flowcharts/variables_overview2.png")