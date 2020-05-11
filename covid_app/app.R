# Info about the app ------------------------------------------------------
#Graphics represents COVID-19 confirmed cases, COVID-19 deaths and COVID-19 test in times series
#Table represents values of sepcial interest to the porject
#Click on the "Run App" icon to run the app

# Loading libraries -------------------------------------------------------
library(shiny)
library(tidyverse)


# Loading files -----------------------------------------------------------
covid_aug_app <- read_tsv(file = "03_covid_aug.tsv")
df_shiny_app <- read_tsv(file = "03_df_shiny_aug.tsv")


# App function coding -----------------------------------------------------
#Defining user interface for app - selection of country from df_shiny_app, showing Denmark by default in sidebar panel. Output (graphics and table) is shown in main panel. 

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

#defining output, subsetting country from to different df to input country. Coding plots by ggplot and aligning text in Table  
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
                 y = "Conf. cases (and if available no. of tests) per 100.000", 
                  title = "Confirmed COVID-19 cases per 100.000 - in green COVID-19 test data (if available)")+
          theme_minimal(base_size = 14)
    })
    
    output$plot2 <-renderPlot({
            filter_plot <- subset(covid_aug_app, covid_aug_app$country == input$country, na.rm = TRUE) 
            ggplot(filter_plot, mapping = aes (x = date, y = dead_cases_per_100000 , na.rm = TRUE, selected = "Denmark")) +
                geom_area(color = "blue", fill = "blue") +
                labs(x = "",
                     y = "Deaths per 100.000",
                     title = "COVID-19 related deaths per 100.000") +
              theme_minimal(base_size = 14)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
