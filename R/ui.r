library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(DT)
library(plotly)

ui <- shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = "General Forecasting Tool",
    dropdownMenu(type = "notifications", badgeStatus = "warning", icon = icon("bell"),
      notificationItem(text = "System Status: Online", icon = icon("check")),
      notificationItem(text = "Memory Usage: Normal", icon = icon("memory")),
      notificationItem(text = "CPU Load: Normal", icon = icon("microchip"))
    )
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Data & Analysis", tabName = "data_analysis", icon = icon("table")),
      menuItem("Regression", tabName = "regression_output", icon = icon("chart-line")),
      menuItem("Combined Forecast", tabName = "combined_forecast", icon = icon("line-chart")),
      menuItem("Aggregated Outputs", tabName = "aggregated_outputs", icon = icon("chart-bar")),
      menuItem("Best Model", tabName = "best_model", icon = icon("trophy"))
    )
  ),
  body = dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap")
    ),
    tabItems(
      # Data & Analysis Tab
      tabItem(tabName = "data_analysis",
              fluidRow(
                shinydashboardPlus::box(
                  title = "Data Table (First 20 Rows)",
                  width = 12, collapsible = TRUE, icon = icon("table"),
                  status = "navy", solidHeader = TRUE,
                  div(class = "box-content",
                      div(class = "loading-overlay", style = "display: none;",
                          div(class = "loading-spinner", icon("spinner", class = "fa-spin"), "Loading data...")),
                      div(class = "empty-state", id = "dataTableEmpty",
                          icon("database"),
                          h4("No Data Available"),
                          p("Upload your data to begin analysis"),
                          actionButton("uploadData", "Upload Data", class = "btn-primary")),
                      DT::dataTableOutput("previewTableMain")
                  )
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Basic Statistics (Mean & SD)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  status = "navy",
                  div(class = "box-content",
                      div(class = "loading-overlay", style = "display: none;",
                          div(class = "loading-spinner", icon("spinner", class = "fa-spin"), "Calculating statistics...")),
                      div(class = "empty-state", id = "statsEmpty",
                          icon("calculator"),
                          h4("No Statistics Available"),
                          p("Statistics will appear here once data is loaded")),
                      plotlyOutput("statsPlot", height = "300px")
                  )
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Trends for Numeric Columns",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  status = "navy",
                  div(class = "box-content",
                      div(class = "loading-overlay", style = "display: none;",
                          div(class = "loading-spinner", icon("spinner", class = "fa-spin"), "Analyzing trends...")),
                      div(class = "empty-state", id = "trendsEmpty",
                          icon("chart-line"),
                          h4("No Trend Analysis Available"),
                          p("Trend analysis will appear here once data is loaded")),
                      plotlyOutput("trendPlot", height = "500px")
                  )
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Correlation Heatmap",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  status = "navy",
                  div(class = "box-content",
                      div(class = "loading-overlay", style = "display: none;",
                          div(class = "loading-spinner", icon("spinner", class = "fa-spin"), "Calculating correlations...")),
                      div(class = "empty-state", id = "correlationEmpty",
                          icon("project-diagram"),
                          h4("No Correlation Analysis Available"),
                          p("Correlation heatmap will appear here once data is loaded")),
                      plotlyOutput("corPlot", height = "300px")
                  )
                )
              )
      ),
      # Regression Tab
      tabItem(tabName = "regression_output",
              fluidRow(
                shinydashboardPlus::box(
                  title = "Regression Model Summaries",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  status = "navy",
                  uiOutput("regressionSummary")
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Regression Charts (Actual vs Predicted)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  status = "navy",
                  uiOutput("regressionPlot")
                )
              )
      ),
      # Combined Forecast Tab
      tabItem(tabName = "combined_forecast",
              fluidRow(
                shinydashboardPlus::box(
                  title = "Combined Forecast (Plotly)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  status = "navy",
                  plotlyOutput("combinedForecastPlot", height = "400px")
                )
              )
      ),
      # Aggregated Outputs Tab
      tabItem(tabName = "aggregated_outputs",
              fluidRow(
                shinydashboardPlus::box(
                  title = "Overall Forecasts (Test Set)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  status = "navy",
                  plotlyOutput("forecastPlot", height = "300px")
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Overall Residuals (Test Set)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  status = "navy",
                  plotlyOutput("residualPlot", height = "300px"),
                  DT::dataTableOutput("errorMetrics")
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Algorithm Comparison (RMSE)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  status = "navy",
                  plotlyOutput("comparisonPlot", height = "300px")
                )
              )
      ),
      # Best Model Tab
      tabItem(tabName = "best_model",
              fluidRow(
                shinydashboardPlus::box(
                  title = "Best Performing Model",
                  width = 12, solidHeader = TRUE,
                  status = "navy",
                  uiOutput("bestAlgorithm")
                )
              )
      )
    )
  ),
  controlbar = dashboardControlbar(
    id = "controlbar",
    disable = FALSE,
    width = 500,
    collapsed = FALSE,
    overlay = TRUE,
    skin = "light",
    controlbarMenu(
      id = "controlbarMenu",
      # System Status
      controlbarItem(
        "status",
        div(class = "metric-card",
            h4("System Status"),
            div(class = "metric-value", textOutput("cpuUsage")),
            div(class = "metric-label", "CPU Usage")),
        div(class = "metric-card",
            h4("Memory"),
            div(class = "metric-value", textOutput("memoryUsage")),
            div(class = "metric-label", "Memory Usage")),
        div(class = "metric-card",
            h4("Active Models"),
            div(class = "metric-value", textOutput("activeModels")),
            div(class = "metric-label", "Running Models"))
      )
    )
  )
)

shinyUI(ui)
