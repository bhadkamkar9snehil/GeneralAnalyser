library(shiny)
library(shinydashboardPlus)
library(shinydashboard)
library(DT)
library(plotly)

ui <- shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = "General Forecasting Tool"
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Data & Analysis",
        tabName = "data_analysis", icon = icon("table")),
      menuItem("Regression",
        tabName = "regression_output", icon = icon("chart-line")),
      menuItem("Combined Forecast",
        tabName = "combined_forecast", icon = icon("line-chart")),
      menuItem("Aggregated Outputs",
        tabName = "aggregated_outputs", icon = icon("chart-bar")),
      menuItem("Best Model",
        tabName = "best_model", icon = icon("trophy"))
    )
  ),
  body = dashboardBody(

    tabItems(
      # Data & Analysis Tab
      tabItem(tabName = "data_analysis",
              fluidRow(
                shinydashboardPlus::box(
                  id = "dataPreviewBox",
                  width = 12,
                  title = "Preview Data",
                  status = "navy",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DT::dataTableOutput("previewTableMain")
                  )
                ),
              fluidRow(
                  shinydashboardPlus::box(
                    width = 12,
                    title = "Statistics Dashboard",
                    status = "navy",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    dropdownMenu = boxDropdown(
                      boxDropdownItem("Show Distribution", id = "showDist"),
                      boxDropdownItem("Show Outliers", id = "showOutliers"),
                      dropdownDivider(),
                      boxDropdownItem("Export Stats", icon = icon("download"))
                    ),
                    plotlyOutput("statsPlot")
                  )

              ),
              fluidRow(
                shinydashboardPlus::box(
                  width = 12,
                  title = "Time Series Analysis",
                  status = "navy",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  sidebar = boxSidebar(
                    id = "timeSeriesSidebar",
                    #background = "green",
                    selectInput("tsMetric", "Select Metric", 
                              choices = c("Trend", "Seasonality", "Residuals")),
                    sliderInput("smoothingWindow", "Smoothing Window",
                              min = 1, max = 20, value = 5)
                  ),
                  plotlyOutput("trendPlot")
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  width = 12,
                  title = "Feature Relationships",
                  status = "navy",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  dropdownMenu = boxDropdown(
                    boxDropdownItem("Correlation Matrix", id = "showCorr"),
                    boxDropdownItem("Scatter Plot Matrix", id = "showScatter"),
                    boxDropdownItem("Feature Importance", id = "showImportance")
                  ),
                  plotlyOutput("corPlot")
                )
              )),
      # Regression Tab
      tabItem(tabName = "regression_output",
              fluidRow(
                column(width = 6,
                  flipBox(
                    id = "modelSummaryBox",
                    width = 12,
                    front = div(
                      h3("Model Summary"),
                      uiOutput("regressionSummary")
                    ),
                    back = div(
                      h3("Model Diagnostics"),
                      plotlyOutput("diagnosticsPlot")
                    )
                  )
                ),
                column(width = 6,
                  shinydashboardPlus::box(
                    width = 12,
                    title = "Model Performance",
                    status = "primary",
                    solidHeader = TRUE,
                    dropdownMenu = boxDropdown(
                      boxDropdownItem("Actual vs Predicted", id = "showActualPred"),
                      boxDropdownItem("Residual Plot", id = "showResiduals"),
                      boxDropdownItem("Q-Q Plot", id = "showQQ")
                    ),
                    sidebar = boxSidebar(
                      id = "modelSidebar",
                      width = 25,
                      sliderInput("confidence", "Confidence Level", 
                                min = 0.8, max = 0.99, value = 0.95),
                      checkboxInput("showInterval", "Show Prediction Interval", TRUE)
                    ),
                    uiOutput("regressionPlot")
                  )
                )
              ),
              fluidRow(
                shinydashboardPlus::socialBox(
                  width = 12,
                  title = userBlock(
                    image = "https://raw.githubusercontent.com/rstudio/hex-stickers/master/PNG/tidymodels.png",
                    title = "Model Metrics",
                    subtitle = "Performance Analysis"
                  ),
                  status = "info",

                  shinydashboardPlus::boxProfile(
                    title = "Model Overview",
                    subtitle = "Key Metrics",
                    type = 2,
                    src = "https://raw.githubusercontent.com/rstudio/hex-stickers/master/PNG/tidyverse.png"
                  ),
                  div(
                    class = "text-center",
                    tabsetPanel(
                      tabPanel("Metrics", DTOutput("metricsTable")),
                      tabPanel("Feature Importance", plotlyOutput("featureImportance")),
                      tabPanel("Model Comparison", plotlyOutput("modelComparison"))
                    )
                  )
                )
              )),
      # Combined Forecast Tab
      tabItem(tabName = "combined_forecast",
              fluidRow(
                shinydashboardPlus::box(
                  width = 12,
                  title = "Combined Forecast Analysis",
                  status = "navy",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  sidebar = boxSidebar(
                    id = "forecastSidebar",
                    startOpen = TRUE,
                    #background = "light-blue",
                    width = 25,
                    selectInput("forecastView", "View Type",
                              choices = c("Combined", "Individual", "Comparison")),
                    checkboxInput("showConfidence", "Show Confidence Intervals", TRUE),
                    sliderInput("horizonSlider", "Forecast Horizon",
                              min = 1, max = 50, value = 10)
                  ),
                  dropdownMenu = boxDropdown(
                    boxDropdownItem("Download Forecast", id = "downloadForecast"),
                    boxDropdownItem("Export Plot", id = "exportPlot"),
                    dropdownDivider(),
                    boxDropdownItem("Model Details", id = "modelDetails")
                  ),
                  plotlyOutput("combinedForecastPlot", height = "500px")
                )
              )),
      
      # Aggregated Outputs Tab
      tabItem(tabName = "aggregated_outputs",
              fluidRow(

                  flipBox(
                    id = "forecastBox",
                    width = 12,
                    front = div(
                      h3("Forecast Results"),
                      plotlyOutput("forecastPlot")
                    ),
                    back = div(
                      h3("Forecast Metrics"),
                      DTOutput("forecastMetrics")
                    )
                  )
              )
              ,
              fluidRow(
                  flipBox(
                    id = "residualsBox",
                    width = 12,
                    front = div(
                      h3("Residual Analysis"),
                      plotlyOutput("residualPlot")
                    ),
                    back = div(
                      h3("Error Distribution"),
                      plotlyOutput("errorDist")
                    )
                  )
                ),

              fluidRow(
                shinydashboardPlus::box(
                  width = 12,
                  title = "Algorithm Comparison",
                  subtitle = "Performance Metrics",
                  status = "primary",
                  plotlyOutput("comparisonPlot")

                )
              )),
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
