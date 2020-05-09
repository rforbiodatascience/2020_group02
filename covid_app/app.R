#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#---------------------------------------------------------

library(shiny)
library(tidyverse)

##Loading files 

covid_aug_app <- read_tsv(file = "03_covid_aug.tsv")
df_shiny_app <- read_tsv(file = "03_df_shiny_aug.tsv")




## Define UI for app that draws a timeseries for Covid-19 deaths, Covid-19 confirmed cases 
#and Covid-19 tests performed until April 16th. 

ui <- fluidPage(
    titlePanel("COVID-19 data until May 4th"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "country", "Select a country", choices = df_shiny_app$country, selected = "Denmark" )
        ),
      
        
        mainPanel(
            tableOutput("covid_data"), 
            plotOutput("plot1"),
            plotOutput("plot2")) 


    )
)
server <- function(input, output, session){
    output$covid_data <- renderTable({
         country_filter <- subset(df_shiny_app, df_shiny_app$country == input$country) 
        
      }, digits = 1, align = "c")
    
    output$plot1 <-renderPlot({
        filter_plot <- subset(covid_aug_app, covid_aug_app$country == input$country) 
        colors <- c("confirmed_cases_per_100000" = "red",  "test_cases_per_100000" = "green")
        ggplot(filter_plot, selected = "Denmark") +
            geom_area(mapping = aes (x = date, y = confirmed_cases_per_100000), fill = "red", color = "red") +
            geom_area(mapping = aes (x = date, y = test_cases_per_100000), fill = "green", color = "green", alpha = 0.3 ) +
            labs(x = "",
                 y = "Confirmed (and in some test) cases per 100.000", 
                  title = "Confirmed COVID-19 cases per 100.000 - in green test data")+
          theme_minimal(base_size = 14)
        
        
    })
    
    output$plot2 <-renderPlot({
            filter_plot <- subset(covid_aug_app, covid_aug_app$country == input$country, na.rm = TRUE) 
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
