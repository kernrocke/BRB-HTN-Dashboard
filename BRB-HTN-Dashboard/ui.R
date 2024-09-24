#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Barbados HEARTS Dashboard"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
           fileInput("file1", "Choose CSV File", accept = ".csv"),
        ),
        mainPanel(
          tableOutput("contents")
        )
        )
    )

