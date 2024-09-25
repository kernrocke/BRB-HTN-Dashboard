#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Preparation
install.packages("devtools")
devtools::install_github("rstudio/gridlayout")

library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)


ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar area4 ",
    "table   plotly",
    "table   plotly"
  ),
  row_sizes = c(
    "40px",
    "1.62fr",
    "0.38fr",
    "1fr"
  ),
  col_sizes = c(
    "380px",
    "1.41fr"
  ),
  gap_size = "0.40000000000000013rem",
  grid_card(
    area = "sidebar",
    card_body(
      selectInput(
        inputId = "mySelectInput",
        label = "Year",
        choices = list("2023" = "2023", "2024" = "2024")
      )
    ),
    card_footer(
      selectInput(
        inputId = "mySelectInput",
        label = "Month",
        choices = list(
          "January" = "1",
          "February" = "2",
          "March" = "3",
          "April" = "4",
          "May" = "5",
          "June" = "6",
          "July" = "7",
          "August" = "8",
          "September" = "9",
          "October" = "10",
          "November" = "11",
          "December" = "12"
        )
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Barbados HEARTS Dashboard",
    alignment = "center",
    is_title = FALSE
  ),
  grid_card(
    area = "table",
    card_header("Hypertension Indicators"),
    card_body(
      DTOutput(
        outputId = "HypertensionIndicators",
        width = "100%"
      )
    )
  ),
  grid_card(
    area = "plotly",
    card_header("Patient Coverage"),
    card_body(
      DTOutput(outputId = "PatientCoverage", width = "100%")
    )
  ),
  grid_card(
    area = "area4",
    card_header(
      card(
        full_screen = TRUE,
        card_header("Hypertension Controlled Rates")
      )
    ),
    card_body(plotlyOutput(outputId = "HTNControlRates"))
  )
)


server <- function(input, output) {
  
  output$distPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    plot_ly(x = ~ faithful[, 2], type = "histogram")
  })
  
  output$bluePlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "steelblue", border = "white")
  })
  
  output$PatientCoverage <- renderDT({
    head(faithful, input$numRows)
  })
}

shinyApp(ui, server)