## HEADER -----------------------------------------------------
##  R file METADATA
##  algorithm name          brb_htn_dashboard / app.R
##  project:                BNR
##  analysts:               Kern Rocke
##  date first created      11-AUG-2025
## 	date last modified      29-SEP-2025
##  algorithm task          Create HTN Dashboard for Barbados HEARTS Programme
##  status                  Completed
##  objective               To have a dashboard for monitoring hypertensive patients
##  methods                 See dashboard for methods used for the dashboard


# Set max upload size to 30 MB
options(shiny.maxRequestSize = 30 * 1024^2)

#-------------------------------------------------------------------------------
######################
### Libraries ###
#####################
# Note: Add any new libraries to the list of libaries in libs

#List of libaries needed
libs <- c("shiny", "shinydashboard", "dplyr", "ggplot2", "plotly", "DT", "readr",
          "lubridate")

#Install missing libraries
installed_libs <- libs %in% rownames(installed.packages())
if (any(installed_libs == F)) {
  install.packages(libs[!installed_libs])
}

#Load libraries
invisible(lapply(libs, library, character.only = T))
#-------------------------------------------------------------------------------

# Define UI using shinydashboard
ui <- dashboardPage(
  skin = "blue", # Set dashboard skin for consistent styling
  dashboardHeader(
    title = div(
      img(src = "barbados_flag.png", height = "40px", style = "vertical-align: middle; margin-right: 10px;", alt = "Barbados Flag", `aria-label` = "Barbados Flag"),
      span("Barbados Hypertension Dashboard", style = "font-size: 24px; color: #000000; font-weight: bold;"),
      img(src = "moh_logo.png", height = "50px", style = "vertical-align: middle; margin-left: 10px;", alt = "Ministry of Health Logo", `aria-label` = "Ministry of Health Logo"),
      style = "display: flex; align-items: center;"
    ),
    titleWidth = "100%"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "nav",
      menuItem("Home", tabName = "home", icon = icon("home"), selected = TRUE),
      menuItem("Dashboard Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Analytics", tabName = "analytics", icon = icon("dashboard")),
      menuItem("Additional Information", tabName = "additional", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    # Custom CSS for header, sidebar, and page content styling
    tags$head(
      tags$style(HTML("
        /* Header styling */
        .skin-blue .main-header {
          background-color: #FFFFFF !important;
        }
        .skin-blue .main-header .logo {
          background-color: #FFFFFF !important;
          color: #000000 !important;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #F0F0F0 !important;
        }
        /* Sidebar styling */
        .skin-blue .main-sidebar {
          background-color: #FFFFFF !important;
        }
        .skin-blue .main-sidebar .sidebar-menu > li > a {
          color: #000000 !important;
        }
        .skin-blue .main-sidebar .sidebar-menu > li.active > a {
          background-color: #F0F0F0 !important;
          color: #000000 !important;
          border-left-color: #000000 !important;
        }
        .skin-blue .main-sidebar .sidebar-menu > li:hover > a {
          background-color: #F0F0F0 !important;
          color: #000000 !important;
        }
        .skin-blue .main-sidebar .sidebar-menu > li > a > .fa {
          color: #000000 !important;
        }
        /* Page content background */
        .skin-blue .content-wrapper {
          background-color: #FFFFFF !important;
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "home",
        div(
          style = "text-align: center; margin: 20px;",
          img(src = "brb_heart.png", style = "width: 300px; max-width: 80%; height: auto;", alt = "Barbados HEARTS Logo"),
          h1(strong("Welcome to the Barbados Hypertension Dashboard"), style = "font-size: 32px; margin-top: 20px; margin-bottom: 30px;")
        ),
         div(
          style = "margin: 20px;",
          h2(strong("Overview"), style = "font-size: 30px;"),
          p(style = "font-size: 18px;", "This dashboard is designed to provide a comprehensive and dynamic surveillance tool for monitoring hypertension within Barbados, leveraging routinely collected clinical data from polyclinics across the island. The primary objective is to offer public health officials, healthcare administrators, and clinicians a real-time view of hypertension prevalence and control, facilitating evidence-based decision-making and targeted interventions.",
            "By consolidating data from primary care facilities, this tool provides a robust picture of the hypertension landscape. The dashboard focuses on several key metrics essential for effective disease management and public health surveillance. These include hypertension control, which measures the proportion of patients with blood pressure within target ranges, and uncontrolled hypertension, which identifies the population at greatest risk for cardiovascular complications.",
            "Furthermore, the dashboard disaggregates hypertension subtypes to provide a more nuanced understanding of the condition. It tracks the prevalence of isolated systolic hypertension and isolated diastolic hypertension. This detailed analysis allows for the identification of specific at-risk populations and supports the development of more precise clinical and public health strategies. Ultimately, this dashboard serves as a critical resource for continuously assessing the national response to hypertension and guiding strategic planning for chronic disease prevention and control in Barbados.")
        ),
        div(
          style = "margin: 20px;",
          h2(strong("Definitions"), style = "font-size: 30px;"),
          tags$ol(style = "font-size: 18px;",
                  tags$li(tags$strong("Hypertension:"), "A systolic blood pressure of greater than or equal to 140 mmHg and a diastolic blood pressure of greater than or equal to 90 mmHg."),
                  tags$li(tags$strong("Controlled Hypertension:"), " Patients diagnosed with hypertension who have successfully lowered their blood pressure to a systolic and diastolic blood pressure of less than 140/90."),
                  tags$li(tags$strong("Uncontrolled Hypertension:"), " Patients diagnosed with hypertension but have been unsuccessful in lowering their blood pressure to a systolic blood pressure of less than 140 or a diastolic blood pressure of less than 90."),
                  tags$li(tags$strong("Isolated Systolic Hypertension:"), " Patients diagnosed with hypertension with a systolic blood pressure of greater than or equal to 140 and a diastolic blood pressure of less than 90."),
                  tags$li(tags$strong("Isolated Diastolic Hypertension:"), " Patients diagnosed with hypertension with a systolic blood pressure of less than 140 and a diastolic blood pressure of greater than or equal to 90.")
          )
        ),
        div(
          style = "margin: 20px;",
          tags$label("Upload CSV File", style = "font-size: 18px; font-weight: bold;", `for` = "file"),
          fileInput("file", NULL, accept = ".csv"),
          p(style = "font-size: 18px;", "Upload a CSV file in the same format as the provided data to view the dashboard overview and analytics pages.")
        )
      ),
      tabItem(
        tabName = "overview",
        div(
          style = "margin: 20px;", 
          fluidRow(
            column(6, 
                   div(
                     style = "font-size: 24px;",
                   selectInput("overview_year", "Select Year", choices = NULL))
            )
                   ),
          h3(strong("Patients Seen at Polyclinics by Month")),
          plotlyOutput("patients_by_month_plot"),
          h3(strong("Hypertension Control by Month")),
          plotlyOutput("control_by_month_plot"),
          h3(strong("Hypertension Control by Last Visited Polyclinic (All Years)")),
          plotlyOutput("polyclinic_control_all_years_plot")
        )
      ),
      tabItem(
        tabName = "analytics",
        div(
          style = "margin: 20px;",
          fluidRow(
            column(6,
                   div(style = "font-size: 24px;",
                       selectInput("month", "Select Month", choices = c("January" = "01", "February" = "02", "March" = "03",
                                                                        "April" = "04", "May" = "05", "June" = "06",
                                                                        "July" = "07", "August" = "08", "September" = "09",
                                                                        "October" = "10", "November" = "11", "December" = "12"))
                   ) 
            ),
            column(6,
                   div(style = "font-size: 24px;",
                       selectInput("year", "Select Year", choices = NULL)
                   ) 
            )
          ),
          h3(strong("Hypertension Control Summary")),
          fluidRow(
            valueBoxOutput("control_rate_box", width = 3),
            valueBoxOutput("uncontrolled_rate_box", width = 3),
            valueBoxOutput("isolated_systolic_rate_box", width = 3),
            valueBoxOutput("isolated_diastolic_rate_box", width = 3),
            valueBoxOutput("visit_count_box", width = 3)
          ),
          h3(strong("Visits by Parish")),
          plotlyOutput("parish_plot"),
          h3(strong("Gender Distribution")),
          plotlyOutput("gender_plot"),
          h3(strong("Systolic Blood Pressure Metrics")),
          DTOutput("systolic_table"),
          h3(strong("Diastolic Blood Pressure Metrics")),
          DTOutput("diastolic_table"),
          h3(strong("Hypertension Control by Parish")),
          plotlyOutput("control_parish_plot"),
          h3(strong("Hypertension Control by Last Visited Polyclinic")),
          plotlyOutput("polyclinic_control_plot"),
          h3(strong("Hypertension Metrics by Gender - Male")),
          DTOutput("male_hypertension_table"),
          h3(strong("Hypertension Metrics by Gender - Female")),
          DTOutput("female_hypertension_table"),
          h3(strong("Hypertension Control by Age Band")),
          plotlyOutput("age_control_plot")
        )
      ),
      tabItem(
        tabName = "additional",
        div(
          style = "margin: 20px;",
          h2(strong("Additional Information"), style = "font-size: 28px;"),
          h3(strong("1.1 Disclaimers"), style = "font-size: 24px;"),
          h4(strong("1.1.1 Data Overview and Visualizations"), style = "font-size: 20px;"),
          p(style = "font-size: 18px;", "The Barbados Ministry of Health and Wellness (MOHW) HEARTS Programme seeks to integrate seamlessly and progressively into already existing health delivery services to promote the adoption of global best practices in the prevention and control of cardiovascular diseases (CVD) and improve the performance of the services through better control of high blood pressure and the promotion of secondary prevention with emphasis on the primary health care. Steps are taken to ensure accuracy and reliability, all data are subject to continuous verification, validation and amendments when needed. Estimates are subject to variations in reporting strategies between polyclinics."),
          p(style = "font-size: 18px;", "Data are compiled and shared with Ministry of Health and Wellness by authorities from the polyclinics via the Health Medical Record Information Tool, MedData. Data management and review is done by the Ministry of Health and Wellness and processing of the data is done by the Barbados National Registry (BNR)."),
          p(style = "font-size: 18px;", "MOHW and BNR makes no warranties or representations regarding the contents, appearance, completeness, technical specifications, or accuracy of the dashboard. MOHW and BNR disclaims all responsibility relating to, and shall not be liable for, any use of the report, the results of such use, or the reliance thereon."),
          p(style = "font-size: 18px;", "MOHW reserves the right to make updates and changes to the report without notice and accepts no liability for any errors or omissions in this regard."),
          p(style = "font-size: 18px;", "The user of the dashboard is responsible for the interpretation and use of the analysis and outputs performed by the dashboard. The submission of content to the dashboard does not imply MOHW’s approval or endorsement of that content, or that the content is appropriate for any purpose or meets any established standard or requirement."),
          p(style = "font-size: 18px;", "Any designations employed or presentation by the user in its use of the app, including tables and maps, do not imply the expression of any opinion whatsoever on the part of the Ministry of Health and Wellness nor the Barbados National Registry concerning the legal status of any of the polyclinics or hospitals under the jurisdiction of the MOHW."),
          h4(strong("1.1.2 Copyright, Permissions, and Referencing"), style = "font-size: 20px;"),
          p(style = "font-size: 18px;", "© The Barbados Ministry of Health and Wellness 2025, All rights reserved."),
          p(style = "font-size: 18px;", "Permission from MOHW is required for the use of the Barbados Hypertension Dashboard."),
          p(style = "font-size: 18px;", "The user shall not, in connection with use of the app, state or imply that MOHW nor BNR endorses or is affiliated with the user, its use of the app, or any content, output, or analysis resulting from or related to the dashboard, or that MOHW nor BNR endorses any entity, organization, company, or product."),
          p(style = "font-size: 18px;", "The use of the MOHW or BNR emblem / logo by a user of the report in connection with its use is not permitted."),
          p(style = "font-size: 18px;", "Suggested citation: Barbados Hypertension Dashboard. Barbados: Ministry of Health and Wellness, 2025. Available online: https://bnr-cdrc.shinyapps.io/BRBHTNDashboard/ (last cited: [date])."),
          h3(strong("1.2 Acknowledgements"), style = "font-size: 24px;"),
          p(style = "font-size: 18px;", "We gratefully acknowledge the input of national public health staff involved in surveillance activities and data submission to Barbados Ministry of Health and Wellness. In addition, we acknowledge the Barbados National Registry for its support in the development and maintenance of the dashboard. Furthermore, we would like to thank all external partners who contributed additional insights and contextual information on the data."),
          h3(strong("1.3 Feedback"), style = "font-size: 24px;"),
          p(style = "font-size: 18px;", "For queries or comments on the contents of this dashboard, please contact: ",
            tags$a(href = "mailto:info@health.gov.bb", "info@health.gov.bb"), " or ",
            tags$a(href = "mailto:bnr@uwi.edu", "bnr@uwi.edu")),
          h3(strong("1.4 Collaborators"), style = "font-size: 24px;"),
          tags$ul(style = "font-size: 18px;",
            tags$li("Ministry of Health and Wellness, Barbados"),
            tags$li("Barbados National Registry"),
            tags$li("The George Alleyne Chronic Disease Research Centre"),
            tags$li("The University of the West Indies, Cave Hill Campus, Barbados")
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive value to store uploaded data
  data <- reactiveVal()
  
  # Debug tab selection
  observeEvent(input$nav, {
    message("Navlist selected: ", input$nav)
  })
  
  # Handle CSV file upload
  observeEvent(input$file, {
    req(input$file)
    tryCatch({
      df <- read_csv(input$file$datapath, na = c("", "NA", "NULL"), col_types = cols(
        patient_id = col_character(),
        encounter_id = col_character(),
        nrn = col_character(),
        visit_month = col_character(),
        times_seen = col_integer(),
        gender = col_character(),
        birth_date = col_character(),
        parish = col_character(),
        last_visited_polyclinic = col_character(),
        last_visited_date = col_character(),
        clinic_name = col_character(),
        visit_reason = col_character(),
        bp_encounter_id = col_character(),
        most_recent_bp_date = col_character(),
        most_recent_systoic = col_double(),
        most_recent_diasystoic = col_double(),
        htn_controlled = col_integer(),
        diasystoic_controlled = col_integer(),
        systolic_controlled = col_integer()
      ))
      
      # Parse mixed date formats for visit_month and birth_date
      df <- df %>%
        mutate(
          visit_month = if_else(grepl("/", visit_month),
                                dmy(visit_month, quiet = TRUE),
                                ymd(visit_month, quiet = TRUE)),
          birth_date = if_else(grepl("/", birth_date),
                               dmy(birth_date, quiet = TRUE),
                               ymd(birth_date, quiet = TRUE))
        )
      
      data(df)
      
      # Extract unique years from visit_month for both analytics and overview tabs
      years <- unique(year(df$visit_month))
      updateSelectInput(session, "year", choices = years, selected = max(years))
      updateSelectInput(session, "overview_year", choices = years, selected = max(years))
      message("CSV uploaded successfully, years available: ", paste(years, collapse = ", "))
    }, error = function(e) {
      showNotification("Error reading CSV file: Please ensure it matches the required format.", type = "error")
      message("CSV Read Error: ", conditionMessage(e))
    })
  })
  
  # Reactive data filtered by selected month and year for analytics tab
  filtered_data <- reactive({
    req(data(), input$month, input$year)
    filtered <- data() %>% 
      filter(format(visit_month, "%m/%Y") == paste0(input$month, "/", input$year))
    message("Filtered data rows for analytics: ", nrow(filtered))
    filtered
  })
  
  # Reactive data filtered by selected year for overview tab
  overview_data <- reactive({
    req(data(), input$overview_year)
    filtered <- data() %>% 
      filter(year(visit_month) == input$overview_year)
    message("Filtered data rows for overview: ", nrow(filtered))
    filtered
  })
  
  # Line graph of patients seen by month
  output$patients_by_month_plot <- renderPlotly({
    req(overview_data())
    df <- overview_data()
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available for selected year"))
    }
    
    monthly_data <- df %>%
      mutate(month = factor(format(visit_month, "%B"), 
                            levels = c("January", "February", "March", "April", "May", "June", 
                                       "July", "August", "September", "October", "November", "December"))) %>%
      group_by(month) %>%
      summarize(patient_count = n_distinct(nrn), .groups = "drop") %>%
      arrange(match(month, month.name)) %>%
      filter(!is.na(month))
    
    if (nrow(monthly_data) == 0) {
      return(plot_ly() %>% layout(title = "No valid data available for selected year"))
    }
    
    p <- ggplot(monthly_data, aes(x = month, y = patient_count, group = 1)) +
      geom_line(color = "blue", size = 1.5) +
      geom_point(color = "blue") +
      geom_text(aes(label = round(patient_count, 1), y = patient_count * 1.02), vjust = -0.5, size = 4) +
      theme_minimal() +
      labs(x = "Month", y = "Number of Unique Patients") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Line graph of hypertension control by month
  output$control_by_month_plot <- renderPlotly({
    req(overview_data())
    df <- overview_data()
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available for selected year"))
    }
    
    monthly_control <- df %>%
      filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic)) %>%
      mutate(month = factor(format(visit_month, "%B"), 
                            levels = c("January", "February", "March", "April", "May", "June", 
                                       "July", "August", "September", "October", "November", "December"))) %>%
      group_by(month) %>%
      summarize(
        control_rate = mean(most_recent_systoic < 140 & most_recent_diasystoic < 90, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      arrange(match(month, month.name)) %>%
      filter(!is.na(month))
    
    if (nrow(monthly_control) == 0) {
      return(plot_ly() %>% layout(title = "No valid blood pressure data available for selected year"))
    }
    
    p <- ggplot(monthly_control, aes(x = month, y = control_rate, group = 1)) +
      geom_line(color = "darkgreen", size = 1.5) +
      geom_point(color = "darkgreen") +
      geom_text(aes(label = round(control_rate, 1), y = control_rate * 1.01), vjust = -0.5, size = 4) +
      theme_minimal() +
      labs(x = "Month", y = "Hypertension Control Percentage (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Clustered bar chart of hypertension control by last visited polyclinic for all years
  output$polyclinic_control_all_years_plot <- renderPlotly({
    req(data())
    df <- data()
    
    if (nrow(df) == 0) {
      message("No rows in data for polyclinic control all years plot")
      return(plot_ly() %>% layout(title = "No data available"))
    }
    
    valid_bp <- df %>% 
      filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic), 
             !is.na(last_visited_polyclinic), last_visited_polyclinic != "") %>%
      mutate(year = year(visit_month)) %>%
      distinct(nrn, year, .keep_all = TRUE)
    
    message("Valid blood pressure and polyclinic rows for all years plot: ", nrow(valid_bp))
    
    if (nrow(valid_bp) == 0) {
      return(plot_ly() %>% layout(title = "No valid blood pressure or polyclinic data available"))
    }
    
    control_df <- valid_bp %>%
      group_by(last_visited_polyclinic, year) %>%
      summarize(
        total_patients = n(),
        controlled_patients = sum(most_recent_systoic < 140 & most_recent_diasystoic < 90, na.rm = TRUE),
        control_rate = if_else(total_patients > 0, round((controlled_patients / total_patients) * 100, 1), 0),
        .groups = "drop"
      ) %>%
      mutate(last_visited_polyclinic = as.character(last_visited_polyclinic))
    
    if (nrow(control_df) == 0) {
      message("No valid data for polyclinic control all years plot")
      return(plot_ly() %>% layout(title = "No valid data for hypertension control"))
    }
    
    message("Polyclinics in all years control plot: ", paste(unique(control_df$last_visited_polyclinic), collapse = ", "))
    message("Years in all years control plot: ", paste(unique(control_df$year), collapse = ", "))
    
    p <- ggplot(control_df, aes(x = last_visited_polyclinic, y = control_rate, fill = as.factor(year))) +
      geom_bar(stat = "identity", position = "dodge") +
      geom_text(aes(label = round(control_rate, 1), y = control_rate * 1.01), position = position_dodge(width = 0.9), vjust = -0.5, size = 2.5) +
      theme_minimal() +
      labs(x = "Last Visited Polyclinic", y = "Hypertension Control Percentage (%)", fill = "Year") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Hypertension control rate valueBox
  output$control_rate_box <- renderValueBox({
    req(filtered_data())
    df <- filtered_data()
    if (nrow(df) == 0) {
      valueBox("No data", "Hypertension Controlled", icon = icon("heartbeat"), color = "green")
    } else {
      valid_bp <- df %>% 
        filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic))
      if (nrow(valid_bp) == 0) {
        valueBox("No valid data", "Hypertension Controlled", icon = icon("heartbeat"), color = "green")
      } else {
        control_rate <- mean(valid_bp$most_recent_systoic < 140 & valid_bp$most_recent_diasystoic < 90, na.rm = TRUE) * 100
        valueBox(
          paste0(round(control_rate, 1), "%"),
          "Hypertension Controlled",
          icon = icon("heartbeat"),
          color = "green"
        )
      }
    }
  })
  
  # Hypertension uncontrolled rate valueBox
  output$uncontrolled_rate_box <- renderValueBox({
    req(filtered_data())
    df <- filtered_data()
    if (nrow(df) == 0) {
      valueBox("No data", "Hypertension Uncontrolled", icon = icon("exclamation-triangle"), color = "red")
    } else {
      valid_bp <- df %>% 
        filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic))
      message("Valid blood pressure rows for uncontrolled rate: ", nrow(valid_bp))
      if (nrow(valid_bp) == 0) {
        valueBox("No valid data", "Hypertension Uncontrolled", icon = icon("exclamation-triangle"), color = "red")
      } else {
        uncontrolled_rate <- mean(valid_bp$most_recent_systoic >= 140 | valid_bp$most_recent_diasystoic >= 90, na.rm = TRUE) * 100
        valueBox(
          paste0(round(uncontrolled_rate, 1), "%"),
          "Hypertension Uncontrolled",
          icon = icon("exclamation-triangle"),
          color = "red"
        )
      }
    }
  })
  
  # Isolated systolic hypertension rate valueBox
  output$isolated_systolic_rate_box <- renderValueBox({
    req(filtered_data())
    df <- filtered_data()
    if (nrow(df) == 0) {
      valueBox("No data", "Isolated Systolic Hypertension", icon = icon("chart-line"), color = "orange")
    } else {
      valid_bp <- df %>% 
        filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic))
      message("Valid blood pressure rows for isolated systolic rate: ", nrow(valid_bp))
      if (nrow(valid_bp) == 0) {
        valueBox("No valid data", "Isolated Systolic Hypertension", icon = icon("chart-line"), color = "orange")
      } else {
        isolated_systolic_rate <- mean(valid_bp$most_recent_systoic >= 140 & valid_bp$most_recent_diasystoic < 90, na.rm = TRUE) * 100
        valueBox(
          paste0(round(isolated_systolic_rate, 1), "%"),
          "Isolated Systolic Hypertension",
          icon = icon("chart-line"),
          color = "orange"
        )
      }
    }
  })
  
  # Isolated diastolic hypertension rate valueBox
  output$isolated_diastolic_rate_box <- renderValueBox({
    req(filtered_data())
    df <- filtered_data()
    if (nrow(df) == 0) {
      valueBox("No data", "Isolated Diastolic Hypertension", icon = icon("chart-line"), color = "yellow")
    } else {
      valid_bp <- df %>% 
        filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic))
      message("Valid blood pressure rows for isolated diastolic rate: ", nrow(valid_bp))
      if (nrow(valid_bp) == 0) {
        valueBox("No valid data", "Isolated Diastolic Hypertension", icon = icon("chart-line"), color = "yellow")
      } else {
        isolated_diastolic_rate <- mean(valid_bp$most_recent_systoic < 140 & valid_bp$most_recent_diasystoic >= 90, na.rm = TRUE) * 100
        valueBox(
          paste0(round(isolated_diastolic_rate, 1), "%"),
          "Isolated Diastolic Hypertension",
          icon = icon("chart-line"),
          color = "yellow"
        )
      }
    }
  })
  
  # Total visits valueBox
  output$visit_count_box <- renderValueBox({
    req(filtered_data())
    df <- filtered_data()
    valueBox(
      nrow(df),
      "Total Visits",
      icon = icon("hospital"),
      color = "blue"
    )
  })
  
  # Bar chart of visits by parish
  output$parish_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available for selected month/year"))
    }
    df <- df %>%
      group_by(parish) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    p <- ggplot(df, aes(x = reorder(parish, -count), y = count)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      geom_text(aes(label = count, y = count / 2), vjust = 0.5, size = 4, color = "white") +
      theme_minimal() +
      labs(x = "Parish", y = "Number of Visits") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Pie chart of gender distribution
  output$gender_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available for selected month/year"))
    }
    df_gender <- df %>%
      group_by(gender) %>%
      summarize(count = n()) %>%
      ungroup() %>%
      arrange(gender)
    
    plot_ly(df_gender, labels = ~gender, values = ~count, type = "pie",
            marker = list(colors = c("fa9fb5", "2c7fb8")),
            textinfo = 'label+percent',
            textfont = list(size = 20)) %>%
      layout(title = "Gender Distribution")
  })
  
  # Systolic blood pressure metrics table
  output$systolic_table <- renderDT({
    req(filtered_data())
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      message("No rows in filtered data for systolic table")
      return(datatable(data.frame(Message = "No data available for selected month/year"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    has_valid_bp <- sum(!is.na(df$most_recent_systoic) & !is.na(df$most_recent_diasystoic)) > 0
    message("Valid blood pressure data for systolic table: ", has_valid_bp)
    
    if (!has_valid_bp) {
      return(datatable(data.frame(Message = "No valid blood pressure data available"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    summary_df <- df %>%
      filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic)) %>%
      mutate(Category = case_when(
        most_recent_systoic < 140 & most_recent_diasystoic < 90 ~ "Controlled",
        most_recent_systoic >= 140 & most_recent_diasystoic >= 90 ~ "Uncontrolled",
        most_recent_systoic >= 140 & most_recent_diasystoic < 90 ~ "Isolated Systolic Hypertension",
        most_recent_systoic < 140 & most_recent_diasystoic >= 90 ~ "Isolated Diastolic Hypertension",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Category)) %>%
      group_by(Category) %>%
      summarize(
        Avg_Systolic = round(mean(most_recent_systoic, na.rm = TRUE), 1),
        Min_Systolic = round(min(most_recent_systoic, na.rm = TRUE), 1),
        Max_Systolic = round(max(most_recent_systoic, na.rm = TRUE), 1),
        .groups = "drop"
      )
    
    if (nrow(summary_df) > 0) {
      message("Systolic table category counts: ", paste(summary_df$Category, collapse = ", "))
    }
    
    if (nrow(summary_df) == 0) {
      message("No rows in systolic table after categorization")
      return(datatable(data.frame(Message = "No data meets any hypertension category criteria"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    datatable(summary_df, options = list(pageLength = 5, searching = FALSE))
  })
  
  # Diastolic blood pressure metrics table
  output$diastolic_table <- renderDT({
    req(filtered_data())
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      message("No rows in filtered data for diastolic table")
      return(datatable(data.frame(Message = "No data available for selected month/year"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    has_valid_bp <- sum(!is.na(df$most_recent_systoic) & !is.na(df$most_recent_diasystoic)) > 0
    message("Valid blood pressure data for diastolic table: ", has_valid_bp)
    
    if (!has_valid_bp) {
      return(datatable(data.frame(Message = "No valid blood pressure data available"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    summary_df <- df %>%
      filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic)) %>%
      mutate(Category = case_when(
        most_recent_systoic < 140 & most_recent_diasystoic < 90 ~ "Controlled",
        most_recent_systoic >= 140 & most_recent_diasystoic >= 90 ~ "Uncontrolled",
        most_recent_systoic >= 140 & most_recent_diasystoic < 90 ~ "Isolated Systolic Hypertension",
        most_recent_systoic < 140 & most_recent_diasystoic >= 90 ~ "Isolated Diastolic Hypertension",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Category)) %>%
      group_by(Category) %>%
      summarize(
        Avg_Diastolic = round(mean(most_recent_diasystoic, na.rm = TRUE), 1),
        Min_Diastolic = round(min(most_recent_diasystoic, na.rm = TRUE), 1),
        Max_Diastolic = round(max(most_recent_diasystoic, na.rm = TRUE), 1),
        .groups = "drop"
      )
    
    if (nrow(summary_df) > 0) {
      message("Diastolic table category counts: ", paste(summary_df$Category, collapse = ", "))
    }
    
    if (nrow(summary_df) == 0) {
      message("No rows in diastolic table after categorization")
      return(datatable(data.frame(Message = "No data meets any hypertension category criteria"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    datatable(summary_df, options = list(pageLength = 5, searching = FALSE))
  })
  
  # Bar chart of hypertension control percentage by parish
  output$control_parish_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      message("No rows in filtered data for control parish plot")
      return(plot_ly() %>% layout(title = "No data available for selected month/year"))
    }
    
    valid_bp <- df %>% 
      filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic), !is.na(parish), parish != "")
    message("Valid blood pressure and parish rows for control parish plot: ", nrow(valid_bp))
    
    if (nrow(valid_bp) == 0) {
      return(plot_ly() %>% layout(title = "No valid blood pressure or parish data available"))
    }
    
    valid_bp <- valid_bp %>%
      distinct(nrn, .keep_all = TRUE)
    message("Unique patients with valid BP and parish: ", nrow(valid_bp))
    
    control_df <- valid_bp %>%
      group_by(parish) %>%
      summarize(
        total_patients = n(),
        controlled_patients = sum(most_recent_systoic < 140 & most_recent_diasystoic < 90, na.rm = TRUE),
        control_rate = if_else(total_patients > 0, round((controlled_patients / total_patients) * 100, 1), 0),
        .groups = "drop"
      ) %>%
      mutate(parish = as.character(parish)) %>%
      arrange(desc(control_rate))
    
    if (nrow(control_df) > 0) {
      message("Parishes in control plot: ", paste(control_df$parish, collapse = ", "))
      message("Control rates: ", paste(control_df$control_rate, collapse = ", "))
    }
    
    if (nrow(control_df) == 0) {
      message("No valid parishes for control plot")
      return(plot_ly() %>% layout(title = "No valid parish data for hypertension control"))
    }
    
    p <- ggplot(control_df, aes(x = reorder(parish, -control_rate), y = control_rate)) +
      geom_bar(stat = "identity", fill = "darkgreen") +
      geom_text(aes(label = control_rate, y = control_rate / 2), vjust = 0.5, size = 4, color = "white") +
      theme_minimal() +
      labs(x = "Parish", y = "Hypertension Control Percentage (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p)
  })
  
  # Bar chart of hypertension control percentage by last visited polyclinic
  output$polyclinic_control_plot <- renderPlotly({
    req(filtered_data())
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      message("No rows in filtered data for polyclinic control plot")
      return(plot_ly() %>% layout(title = "No data available for selected month/year"))
    }
    
    valid_bp <- df %>% 
      filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic), 
             !is.na(last_visited_polyclinic), last_visited_polyclinic != "")
    message("Valid blood pressure and polyclinic rows for control polyclinic plot: ", nrow(valid_bp))
    
    if (nrow(valid_bp) == 0) {
      return(plot_ly() %>% layout(title = "No valid blood pressure or polyclinic data available"))
    }
    
    valid_bp <- valid_bp %>%
      distinct(nrn, .keep_all = TRUE)
    message("Unique patients with valid BP and polyclinic: ", nrow(valid_bp))
    
    control_df <- valid_bp %>%
      group_by(last_visited_polyclinic) %>%
      summarize(
        total_patients = n(),
        controlled_patients = sum(most_recent_systoic < 140 & most_recent_diasystoic < 90, na.rm = TRUE),
        control_rate = if_else(total_patients > 0, round((controlled_patients / total_patients) * 100, 1), 0),
        .groups = "drop"
      ) %>%
      mutate(last_visited_polyclinic = as.character(last_visited_polyclinic)) %>%
      arrange(desc(control_rate))
    
    if (nrow(control_df) > 0) {
      message("Polyclinics in control plot: ", paste(control_df$last_visited_polyclinic, collapse = ", "))
      message("Control rates: ", paste(control_df$control_rate, collapse = ", "))
    }
    
    if (nrow(control_df) == 0) {
      message("No valid polyclinics for control plot")
      return(plot_ly() %>% layout(title = "No valid polyclinic data for hypertension control"))
    }
    
    p <- ggplot(control_df, aes(x = reorder(last_visited_polyclinic, -control_rate), y = control_rate)) +
      geom_bar(stat = "identity", fill = "purple") +
      geom_text(aes(label = control_rate, y = control_rate / 2), vjust = 0.5, size = 4, color = "white") +
      theme_minimal() +
      labs(x = "Last Visited Polyclinic", y = "Hypertension Control Percentage (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p)
  })
  
  # Male hypertension metrics table
  output$male_hypertension_table <- renderDT({
    req(filtered_data())
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      message("No rows in filtered data for male hypertension table")
      return(datatable(data.frame(Message = "No data available for selected month/year"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    male_df <- df %>%
      filter(gender == "Male", !is.na(most_recent_systoic), !is.na(most_recent_diasystoic))
    message("Valid blood pressure rows for male hypertension table: ", nrow(male_df))
    
    if (nrow(male_df) == 0) {
      return(datatable(data.frame(Message = "No valid data available for Male patients"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    male_df <- male_df %>%
      distinct(nrn, .keep_all = TRUE)
    message("Unique male patients with valid BP: ", nrow(male_df))
    
    total_patients <- nrow(male_df)
    summary_df <- male_df %>%
      mutate(Category = case_when(
        most_recent_systoic < 140 & most_recent_diasystoic < 90 ~ "Controlled",
        most_recent_systoic >= 140 & most_recent_diasystoic >= 90 ~ "Uncontrolled",
        most_recent_systoic >= 140 & most_recent_diasystoic < 90 ~ "Isolated Systolic Hypertension",
        most_recent_systoic < 140 & most_recent_diasystoic >= 90 ~ "Isolated Diastolic Hypertension",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Category)) %>%
      group_by(Category) %>%
      summarize(
        Count = n(),
        Percentage = round((n() / total_patients) * 100, 1),
        .groups = "drop"
      )
    
    uncontrolled_count <- sum(male_df$most_recent_systoic >= 140 | male_df$most_recent_diasystoic >= 90, na.rm = TRUE)
    uncontrolled_percentage <- round((uncontrolled_count / total_patients) * 100, 1)
    summary_df <- summary_df %>%
      mutate(Percentage = if_else(Category == "Uncontrolled", uncontrolled_percentage, Percentage))
    
    if (nrow(summary_df) > 0) {
      message("Male hypertension table category counts: ", paste(summary_df$Category, collapse = ", "))
    }
    
    if (nrow(summary_df) == 0) {
      message("No rows in male hypertension table after categorization")
      return(datatable(data.frame(Message = "No Male patients meet any hypertension category criteria"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    datatable(summary_df, 
              options = list(pageLength = 5, searching = FALSE),
              rownames = FALSE) %>%
      formatStyle(columns = c("Category", "Count", "Percentage"),
                  backgroundColor = "#ADD8E6")
  })
  
  # Female hypertension metrics table
  output$female_hypertension_table <- renderDT({
    req(filtered_data())
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      message("No rows in filtered data for female hypertension table")
      return(datatable(data.frame(Message = "No data available for selected month/year"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    female_df <- df %>%
      filter(gender == "Female", !is.na(most_recent_systoic), !is.na(most_recent_diasystoic))
    message("Valid blood pressure rows for female hypertension table: ", nrow(female_df))
    
    if (nrow(female_df) == 0) {
      return(datatable(data.frame(Message = "No valid data available for Female patients"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    female_df <- female_df %>%
      distinct(nrn, .keep_all = TRUE)
    message("Unique female patients with valid BP: ", nrow(female_df))
    
    total_patients <- nrow(female_df)
    summary_df <- female_df %>%
      mutate(Category = case_when(
        most_recent_systoic < 140 & most_recent_diasystoic < 90 ~ "Controlled",
        most_recent_systoic >= 140 & most_recent_diasystoic >= 90 ~ "Uncontrolled",
        most_recent_systoic >= 140 & most_recent_diasystoic < 90 ~ "Isolated Systolic Hypertension",
        most_recent_systoic < 140 & most_recent_diasystoic >= 90 ~ "Isolated Diastolic Hypertension",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(Category)) %>%
      group_by(Category) %>%
      summarize(
        Count = n(),
        Percentage = round((n() / total_patients) * 100, 1),
        .groups = "drop"
      )
    
    uncontrolled_count <- sum(female_df$most_recent_systoic >= 140 | female_df$most_recent_diasystoic >= 90, na.rm = TRUE)
    uncontrolled_percentage <- round((uncontrolled_count / total_patients) * 100, 1)
    summary_df <- summary_df %>%
      mutate(Percentage = if_else(Category == "Uncontrolled", uncontrolled_percentage, Percentage))
    
    if (nrow(summary_df) > 0) {
      message("Female hypertension table category counts: ", paste(summary_df$Category, collapse = ", "))
    }
    
    if (nrow(summary_df) == 0) {
      message("No rows in female hypertension table after categorization")
      return(datatable(data.frame(Message = "No Female patients meet any hypertension category criteria"),
                       options = list(pageLength = 5, searching = FALSE)))
    }
    
    datatable(summary_df, 
              options = list(pageLength = 5, searching = FALSE),
              rownames = FALSE) %>%
      formatStyle(columns = c("Category", "Count", "Percentage"),
                  backgroundColor = "#FFC1CC")
  })
  
  # Bar chart of hypertension control percentage by age band
  output$age_control_plot <- renderPlotly({
    req(filtered_data(), input$year)
    df <- filtered_data()
    
    if (nrow(df) == 0) {
      message("No rows in filtered data for age control plot")
      return(plot_ly() %>% layout(title = "No data available for selected month/year"))
    }
    
    valid_df <- df %>%
      filter(!is.na(most_recent_systoic), !is.na(most_recent_diasystoic), !is.na(birth_date)) %>%
      mutate(
        end_date = as.Date(paste0(input$year, "-12-31")),
        age = floor(interval(birth_date, end_date) / years(1))) %>%
      filter(!is.na(age)) %>%
      distinct(nrn, .keep_all = TRUE)
    
    message("Valid patients with BP and age for age control plot: ", nrow(valid_df))
    
    if (nrow(valid_df) == 0) {
      return(plot_ly() %>% layout(title = "No valid data available for age calculation"))
    }
    
    age_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)
    age_labels <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                    "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", 
                    "80-84", "85+")
    
    control_df <- valid_df %>%
      mutate(age_band = cut(age, breaks = age_breaks, labels = age_labels, right = FALSE, include.lowest = TRUE)) %>%
      group_by(age_band) %>%
      summarize(
        control_rate = mean(most_recent_systoic < 140 & most_recent_diasystoic < 90, na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      arrange(age_band)
    
    if (nrow(control_df) > 0) {
      message("Age bands in control plot: ", paste(control_df$age_band, collapse = ", "))
    }
    
    if (nrow(control_df) == 0) {
      message("No valid age bands for control plot")
      return(plot_ly() %>% layout(title = "No valid data for age bands"))
    }
    
    p <- ggplot(control_df, aes(x = age_band, y = control_rate)) +
      geom_bar(stat = "identity", fill = "darkred") +
      geom_text(aes(label = round(control_rate, 1), y = control_rate / 2), vjust = 0.5, size = 4, color = "white") +
      theme_minimal() +
      labs(x = "Age Band", y = "Hypertension Control Percentage (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p)
  })
}

# Run the application
shinyApp(ui = ui, server = server)