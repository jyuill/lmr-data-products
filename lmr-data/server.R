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
library(here)

# fetch data from online database into lmr_data
source('query.R')
#lmr_data <- lmr_data %>% arrange(cyr, cqtr, cat_type)
# drop incomplete calendar year at start
tbl_yq <- table(lmr_data$cyr, lmr_data$cqtr)
if(any(tbl_yq[1,] == 0)) {
  lmr_data <- lmr_data %>% filter(cyr != rownames(tbl_yq)[1])
}

# Define server logic required to draw a histogram
function(input, output, session) {
  # Filter the dataset based on the selected categories
  filtered_data <- reactive({
    lmr_data %>% filter(cyr %in% input$cyr_picker) %>%
      filter(cqtr %in% input$qtr_check) %>%
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


