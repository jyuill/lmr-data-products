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

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        x <- lmr_data %>% group_by(cyr) %>% summarize(netsales = sum(netsales)) %>% 
          select(netsales)
        x <- x[[1]]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })
    
    output$sales_line <- renderPlotly({
        
        x <- lmr_data %>% group_by(cyr) %>% summarize(netsales = sum(netsales)) 

        # draw the line chart for sales by year
        p <- x %>% filter(input$cyr %in% cyr) %>% 
          ggplot(aes(x = cyr, y = netsales)) +
          geom_line() +
          scale_y_continuous(labels = scales::dollar) +
          theme_minimal()
        ggplotly(p)
    })

}
