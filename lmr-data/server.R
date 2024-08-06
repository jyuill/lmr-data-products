#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(plotly)

# fetch data into lmr_data
dotenv::load_dot_env("../.env")
source('query.R')

# Define server logic required to draw a histogram
function(input, output, session) {
  # Filter the dataset based on the selected categories
  filtered_data <- reactive({
    lmr_data %>% filter(cat_type %in% input$cat_check)
  })
  
    output$sales_line <- renderPlotly({
        # filter for desired data
        #x <- lmr_data %>% filter(cat_type %in% input$cat_select) %>%
        #        group_by(cyr) %>% summarize(netsales = sum(netsales)) 
        x <- filtered_data() 
        x <- x %>% group_by(cyr) %>% summarize(netsales = sum(netsales))

        # draw the line chart for sales by year
        p <- x %>% filter(cyr %in% input$cyr_picker) %>% 
          ggplot(aes(x = cyr, y = netsales)) +
          geom_line() +
          scale_y_continuous(labels = scales::dollar) +
          theme_minimal()
        ggplotly(p)
    })

}


