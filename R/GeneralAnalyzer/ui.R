library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(DT)
library(plotly)

ui <- shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(title = "General Forecasting Tool"),
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
    tabItems(
      # Data & Analysis Tab
      tabItem(tabName = "data_analysis",
              fluidRow(
                box(
                  title = "Data Table (First 20 Rows)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  DT::dataTableOutput("previewTableMain")
                )
              ),
              fluidRow(
                box(
                  title = "Basic Statistics (Mean & SD)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  plotlyOutput("statsPlot", height = "300px")
                )
              ),
              fluidRow(
                box(
                  title = "Trends for Numeric Columns",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  plotlyOutput("trendPlot", height = "500px")
                )
              ),
              fluidRow(
                box(
                  title = "Correlation Heatmap",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  plotlyOutput("corPlot", height = "300px")
                )
              )
      ),
      # Regression Tab
      tabItem(tabName = "regression_output",
              fluidRow(
                box(
                  title = "Regression Model Summaries",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  uiOutput("regressionSummary")
                )
              ),
              fluidRow(
                box(
                  title = "Regression Charts (Actual vs Predicted)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  uiOutput("regressionPlot")
                )
              )
      ),
      # Combined Forecast Tab
      tabItem(tabName = "combined_forecast",
              fluidRow(
                box(
                  title = "Combined Forecast (Plotly)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  plotlyOutput("combinedForecastPlot", height = "400px")
                )
              )
      ),
      # Aggregated Outputs Tab
      tabItem(tabName = "aggregated_outputs",
              fluidRow(
                box(
                  title = "Overall Forecasts (Test Set)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  plotlyOutput("forecastPlot", height = "300px")
                )
              ),
              fluidRow(
                box(
                  title = "Overall Residuals (Test Set)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  plotlyOutput("residualPlot", height = "300px"),
                  DT::dataTableOutput("errorMetrics")
                )
              ),
              fluidRow(
                box(
                  title = "Algorithm Comparison (RMSE)",
                  width = 12, collapsible = TRUE, solidHeader = TRUE,
                  plotlyOutput("comparisonPlot", height = "300px")
                )
              )
      ),
      # Best Model Tab
      tabItem(tabName = "best_model",
              fluidRow(
                box(
                  title = "Best Performing Model",
                  width = 12, status = "success", solidHeader = TRUE,
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
    .list = list(
      controlbarMenu(
        id = "controlbarMenu",
        # Data & Forecast Inputs
        controlbarItem(
          title = "Data & Forecast",
          fileInput("file", "Upload CSV/Excel File", accept = c(".csv", ".xls", ".xlsx")),
          selectInput("time_col", "Select Time Column:", choices = NULL),
          selectInput("target_col", "Select Target Column:", choices = NULL),
          selectizeInput("multivar_cols", "Additional Columns (optional):", choices = NULL, multiple = TRUE),
          sliderInput("split_ratio", "Training Ratio:", min = 0.1, max = 0.9, value = 0.8, step = 0.1),
          numericInput("future_periods", "Future Periods:", value = 10, min = 1),
          selectizeInput("algorithms", "Select Algorithm(s):",
                         choices = list(
                           Forecasting = c("ARIMA", "ETS", "NNETAR", "VAR", "SVAR", "Random Forest"),
                           Regression  = c("Linear Regression", "Ridge Regression", "Lasso Regression",
                                           "Elastic Net", "Random Forest Regression", "Support Vector Regression", "Decision Tree Regression")
                         ),
                         multiple = TRUE),
          checkboxInput("runRegression", "Run Regression Analysis", FALSE),
          conditionalPanel(
            condition = "input.runRegression == true",
            selectizeInput("regressors", "Select Predictor Columns:", choices = NULL, multiple = TRUE)
          ),
          actionButton("runAnalysis", "Run Analysis", 
                       class = "btn", 
                       style = "background-color: lightgreen; font-size: 18px; padding: 10px 20px; margin-top: 15px;")
        ),
        # Chart Settings
        controlbarItem(
          title = "Chart Settings",
          tabsetPanel(
            tabPanel("Theme & Palette",
                     selectInput("chart_to_customize", "Customize Chart:",
                                 choices = c("All Charts", "Combined Forecast", "Forecast Plot", "Residual Plot",
                                             "Trend Plot", "Correlation Heatmap", "Basic Statistics"),
                                 selected = "All Charts"),
                     selectInput("ggplot_theme", "Select ggplot Theme:",
                                 choices = c("Minimal", "Classic", "BW", "Light", "Void", "Gray", "Economist",
                                             "Fivethirtyeight", "Stata", "Solarized"),
                                 selected = "Minimal"),
                     selectInput("series_palette", "Series Colour Palette:",
                                 choices = c("Default", "Set 2", "Dark2", "Pastel1"), selected = "Default"),
                     selectInput("cor_color_scheme", "Correlation Color Scheme:",
                                 choices = c("Blue-Red", "Purple-Green", "Orange-Blue"), selected = "Blue-Red")
            ),
            tabPanel("Text Options",
                     checkboxInput("change_labels", "Change axis labels", FALSE),
                     conditionalPanel(
                       condition = "input.change_labels == true",
                       textInput("custom_title", "Custom Plot Title:", value = "Combined Forecast"),
                       textInput("custom_xlab", "Custom X-axis Label:", value = "Time"),
                       textInput("custom_ylab", "Custom Y-axis Label:", value = "Target Value")
                     ),
                     checkboxInput("adjust_font", "Adjust Font Sizes", FALSE),
                     conditionalPanel(
                       condition = "input.adjust_font == true",
                       numericInput("axis_text_size", "Axis Text Size:", value = 10, min = 6, max = 20),
                       numericInput("title_text_size", "Title Text Size:", value = 14, min = 10, max = 30)
                     ),
                     checkboxInput("rotate_x", "Rotate X-axis Labels", FALSE)
            ),
            tabPanel("Legend & Size",
                     radioButtons("legend_option", "Legend Options:",
                                  choices = c("Keep as is", "Remove", "Custom"),
                                  selected = "Keep as is"),
                     conditionalPanel(
                       condition = "input.legend_option == 'Custom'",
                       textInput("legend_title", "Legend Title:", value = "Model"),
                       selectInput("legend_pos", "Legend Position:",
                                   choices = c("right", "left", "top", "bottom"), selected = "right")
                     ),
                     checkboxInput("adjust_size", "Adjust Plot Size", FALSE),
                     conditionalPanel(
                       condition = "input.adjust_size == true",
                       numericInput("plot_width", "Plot Width (px):", value = 800, min = 400),
                       numericInput("plot_height", "Plot Height (px):", value = 600, min = 300)
                     )
            )
          )
        ),
        # Appearance Settings
        controlbarItem(
          title = "Appearance",
          skinSelector()
        )
      )
    )
  ),
  title = "General Forecasting Tool",
  skin = "black-light"
)

shinyUI(ui)
