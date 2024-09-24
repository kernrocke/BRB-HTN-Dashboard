#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Install shinydashboard
#install.packages("shinydashboard")
#install.packages("NHSplotthedots")

# load packages
library(shinydashboard)
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(NHSRplotthedots)

# define user interface
ui <- dashboardPage(
  dashboardHeader(title = "Barbados HEARTS Dashboard", 
                  titleWidth = 1000),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "File Upload", tabName = "data", icon = icon("file-upload")),
      menuItem(text = "Overview", tabName = "overview", icon = icon("balance-scale-right")),
      menuItem(text = "Patient Coverage", tabName = "future", icon = icon("signal")),
      menuItem(text = "Gender", tabName = "history", icon = icon("user"))
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "future",
              fluidRow(
                infoBox(title = "Controlled BP", 48, icon = icon("hand-holding-usd")),
                infoBox(title = "Isolated Systolic Hyptertension", 33, icon = icon("bed")),
                infoBox(title = "Isolated Diastolic Hypertension", 3, icon = icon("percentage"))
              ),
              fluidRow(
                infoBox(title = "Total Patient Coverage", 48, icon = icon("hand-holding-usd")),
                infoBox(title = "Total Controlled BP patients", 33, icon = icon("bed")),
                infoBox(title = "Total Uncontrolled BP patients", 3, icon = icon("percentage")),
                infoBox(title = "Total Isolated systolic BP patients", 3, icon = icon("percentage")),
                infoBox(title = "Total Isolated diastolic BPd BP patients", 3, icon = icon("percentage")),
              )
              
      ),
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Controlled Hypertension Rates", width = 8,
                  plotOutput(outputId = "graph_1_overall_htn_rates")
                ),
                box(
                  title = "Polyclinic Hypertension Control Rates", width = 8,
                  plotOutput(outputId = "graph_2_polyclinic_htn_rates")
                ),
                box(
                  title = "Summary Systolic Blood Pressure", width = 4,
                  tableOutput(outputId = "table_1_sum_sysbp")
                ),
                box(
                  title = "Summary Diastolic Blood Pressure", width = 4,
                  tableOutput(outputId = "table_1_sum_diabp")
                )
              ),
              
      ),
      
      tabItem(tabName = "history",
              fluidRow(
                box(
                  title = "Revenue [Check-In]", width = 4,
                  plotOutput(outputId = "p2-revenue-checkin-hist")
                ),
                box(
                  title = "Revenue [Book]", width = 4,
                  plotOutput(outputId = "p1-revenue-book-hist")
                ),
                box(
                  title = "RevPAR", width = 4,
                  plotOutput(outputId = "p3-revpar-hist")
                )
              ),
              fluidRow(
                box(
                  title = "Revenue [Channel]", width = 4,
                  plotOutput(outputId = "p4-revenue-channel-hist")
                ),
                box(
                  title = "Occupancy [unit]", width = 4,
                  plotOutput(outputId = "p5-occupancy-hist")
                ),
                box(
                  title = "Book vs Cancel [Unit]", width = 4,
                  plotOutput(outputId = "p6-book-vs-cancel-hist")
                )
              )
      ),
      
      tabItem(tabName = "data",
              column(width = 4,
                     fluidRow(
                       inputPanel(
                         fileInput(inputId = "csv-upload", label = "File upload (csv only):",
                                   accept = ".csv")
                       )
                     )
              )
      )
    )
  )
)

# define server
server <- function(input, output) {
  
}

# run application
shinyApp(ui = ui, server = server)