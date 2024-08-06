#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyWidgets)


# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("LMR data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # select one or more years, including multiple years
            # different options tried
            # selector is good but takes up space, not so intuitive/elegant
            # selectizeInput(inputId="cyr_select", "Select a year", 
            #                choices = unique(lmr_data$cyr), 
            #                selected = unique(lmr_data$cyr),
            #                multiple = TRUE
            #                ),
            # checkbox works but screen real estate
            # checkboxGroupInput(inputId = "cyr_check", "Select a year", 
            #                    choices = unique(lmr_data$cyr), 
            #                    selected = unique(lmr_data$cyr),
            #                    inline = FALSE
            #                    ),
            # picker for max flexibility/usability
            pickerInput(
              inputId = "cyr_picker",
              label = "Select Year(s):",
              choices = unique(lmr_data$cyr),
              selected = unique(lmr_data$cyr),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `selected-text-format` = "count > 3",
                `count-selected-text` = "{0} years selected",
                `live-search` = TRUE
              )
            ),
            # filter for categories
            selectizeInput(inputId="cat_select", "Select a year", 
                            choices = unique(lmr_data$cat_type), 
                            selected = unique(lmr_data$cat_type),
                            multiple = TRUE
                            ),
        ), # end sidebarPanel

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("sales_line")
        ) # end mainPanel
    ) # end sidebarLayout
) # end shinyUI
