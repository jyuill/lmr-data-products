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

# fetch data from online database into lmr_data
source('query.R')

# Define server logic required to draw a histogram
function(input, output, session) {
  # Filter the dataset based on the selected categories
  filtered_data <- reactive({
    lmr_data %>% filter(cyr %in% input$cyr_picker) %>%
      filter(qtr %in% input$qtr_check) %>%
      filter(cat_type %in% input$cat_check)
  })
    # plot for sales by year
    output$sales_line <- renderPlotly({
        # filter for desired data
        #x <- lmr_data %>% filter(cat_type %in% input$cat_select) %>%
        #        group_by(cyr) %>% summarize(netsales = sum(netsales)) 
        x <- filtered_data() 
        x <- x %>% group_by(cyr) %>% summarize(netsales = sum(netsales))

        # draw the line chart for sales by year
        #p <- x %>% filter(cyr %in% input$cyr_picker) %>% 
        p <- x %>%
          ggplot(aes(x = cyr, y = netsales)) +
          geom_col() +
          scale_y_continuous(labels = scales::dollar) +
          theme_minimal()
        ggplotly(p)
    })
    # plot for year-over-year change in sales
    output$sales_yoy <- renderPlotly({
      x <- filtered_data() %>% group_by(cyr) %>% summarize(netsales = sum(netsales)) %>%
        mutate(yoy = (netsales - lag(netsales))/lag(netsales))
      p <- x %>%
        ggplot(aes(x = cyr, y = yoy)) +
        geom_col() +
        scale_y_continuous(labels = scales::percent) +
        theme_minimal()
    })

}


