#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("LMR data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            # select one or more years, including multiple years
            selectizeInput("cyr", "Select a year", 
                           choices = unique(lmr_data$cyr), 
                           selected = unique(lmr_data$cyr),
                           multiple = TRUE
                           )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            plotlyOutput("sales_line")
        )
    )
)
