#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

## Define UI for app that draws a timeseries for Covid-19 deaths, Covid-19 confirmed cases 
#and Covid-19 tests performed until April 16th. 

ui <- fluidPage(
    titlePanel("COVID-19 data until April 16th"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "country", "Select a country", choices = tibble_shiny$country, selected = "Denmark" ),
        ),
      
        
        mainPanel(
            tableOutput("covid_data"), 
            plotOutput("plot1"),
            plotOutput("plot2")) 


    )
)
server <- function(input, output, session){
    output$covid_data <- renderTable({
         country_filter <- subset(tibble_shiny, tibble_shiny$country == input$country)
           
        
    })
    
    output$plot1 <-renderPlot({
        filter_plot <- subset(covid_aug, covid_aug$country == input$country) 
        ggplot(filter_plot, mapping = aes (x = date, y = confirmed_cases_per_100000, na.rm = TRUE, selected = "Denmark")) +
            geom_area(color = "red", fill = "red") +
            labs(x = "",
                 y = "Cases and tests per 100.000",
                 title = "Confirmed COVID-19 cases") + 
          geom_area(filter_plot, mapping = aes (x = date, y = test_cases_per_100000), na.rm = TRUE,  color = "green", fill = "green", alpha = 0.3) +
          theme_minimal(base_size = 14)
        
        
    })
    
    output$plot2 <-renderPlot({
            filter_plot <- subset(covid_aug, covid_aug$country == input$country, na.rm = TRUE) 
            ggplot(filter_plot, mapping = aes (x = date, y = dead_cases_per_100000 , na.rm = TRUE, selected = "Denmark")) +
                geom_area(color = "blue", fill = "blue") +
                labs(x = "",
                     y = "Deaths per 100.000",
                     title = "COVID-19 related deaths") +
              theme_minimal(base_size = 14)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
