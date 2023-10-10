#' Shiny App for Unemployment Rate by Municipality
#'
#' This Shiny app displays a bar chart of unemployment rates by municipality
#' for different years using the ggplot library.
#' To run the Shiny app, You can run the application by clicking the 'Run App' button above.
#' Find out more about building applications with Shiny here: http://shiny.rstudio.com/
#'
#'
#' @param query_data_frame A dataframe containing the data with columns: Count, KPI, MunicipalityID, Municipality, Year, and UnemploymentRate.
#' @param filtered_df A dataframe created to plot the bar chart, and is filtered based on the year selected.
#'
#' @return a shiny web application plotting a basic bar chart of the unemployment rate in 6 municipalites. 
#' It includes a dropdown option to filter through years.
#' 
#' @export
#'
#'

library(shiny)
library(dplyr)
library(ggplot2)
library(devtools)
library(usethis)
install_github("yemimorrison/koladaAPI")

df <- getquerydata("http://api.kolada.se/v2/data/kpi/N03932", 
             entity = "municipality", 
             municipality = c("Uppsala","Goteborg", "Stockholm", "Linkoping", "Malmo", "Vasteras", "Norrkoping"),
             year = c(2010:2022))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Unemployment Rate in Sweden"),

    # Sidebar with a drop-down input for Years
    sidebarLayout(
      sidebarPanel(
        selectInput("year", "Select Year:",
                    choices = c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
                    selected = "2010")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barplot")
        )
    )
)

# Define server logic required to draw a bar chart
server <- function(input, output) {

  output$barplot <- renderPlot({
        # generate bins based on input$bins from ui.R
      selected_year <- as.numeric(input$year)
      filtered_df <- query_data_frame %>% filter(Year == selected_year)
    
        # draw the barchart with the specified number of bins
      bar_chart <-ggplot(filtered_df, aes(x = Municipality, y = UnemploymentRate))+
        geom_bar(stat = "identity", fill = "orange") + 
        labs(title = paste("Unemployment Rate in ", selected_year),
             x = "Municipality",
             y = "Unemployment Rate")+
        theme_minimal()
      
      bar_chart
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
