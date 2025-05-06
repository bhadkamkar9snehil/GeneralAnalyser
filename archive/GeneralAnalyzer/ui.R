library(shiny)
library(DT)
library(plotly)
library(shinydashboardPlus)

ui <- shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = "General Analytics Platform"
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Data Preview", tabName = "data_preview", icon = icon("table")),
      menuItem("Analysis", icon = icon("chart-bar"),
               menuSubItem("Time Series", tabName = "timeseries", icon = icon("line-chart")),
               menuSubItem("Regression", tabName = "regression", icon = icon("bar-chart-o")),
               menuSubItem("Classification", tabName = "classification", icon = icon("sitemap")),
               menuSubItem("Clustering", tabName = "clustering", icon = icon("object-group")),
               menuSubItem("Anomaly Detection", tabName = "anomaly", icon = icon("exclamation-triangle"))
      ),
      menuItem("Model Management", icon = icon("cogs"),
               menuSubItem("Training", tabName = "training", icon = icon("play-circle")),
               menuSubItem("Deployment", tabName = "deployment", icon = icon("rocket")),
               menuSubItem("Monitoring", tabName = "monitoring", icon = icon("eye")),
               menuSubItem("Explainability", tabName = "explain", icon = icon("info-circle"))
      )
    )
  ),
  body = shinydashboardPlus::dashboardBody(
    tabItems(
      # Data Preview Tab
      tabItem(tabName = "data_preview",
        fluidRow(
          box(width = 12,
            title = "Data Input",
            fileInput("file", "Choose CSV or Excel file", accept = c(".csv", ".xls", ".xlsx")),
            DT::dataTableOutput("previewTableMain")
          )
        )
      ),
      
      # Time Series Tab
      tabItem(tabName = "timeseries",
        fluidRow(
          box(width = 3,
            selectInput("time_col", "Time Column:", choices = NULL),
            selectInput("target_col", "Target Column:", choices = NULL),
            selectizeInput("multivar_cols", "Additional Variables:", choices = NULL, multiple = TRUE),
            sliderInput("split_ratio", "Training Ratio:", min = 0.1, max = 0.9, value = 0.8, step = 0.1),
            numericInput("future_periods", "Forecast Horizon:", value = 10, min = 1),
            selectizeInput("algorithms", "Select Algorithm(s):", 
                         choices = c("ARIMA", "ETS", "NNETAR", "VAR", "SVAR", "Random Forest"),
                         multiple = TRUE),
            actionButton("runAnalysis", "Run Analysis", class = "btn-primary")
          ),
          box(width = 9,
            plotlyOutput("combinedForecast", height = "400px"),
            plotlyOutput("residualPlot", height = "300px"),
            DT::dataTableOutput("errorMetrics")
          )
        )
      ),
      
      # Regression Tab
      tabItem(tabName = "regression",
        fluidRow(
          box(width = 3,
            selectInput("reg_target", "Target Variable:", choices = NULL),
            selectizeInput("reg_features", "Feature Variables:", choices = NULL, multiple = TRUE),
            selectInput("reg_type", "Regression Type:",
                       choices = c("Linear", "Polynomial", "Logistic", "Ridge", "Lasso")),
            actionButton("runRegression", "Run Regression", class = "btn-primary")
          ),
          box(width = 9,
            plotlyOutput("regressionPlot", height = "400px"),
            verbatimTextOutput("regressionSummary")
          )
        )
      ),
      
      # Classification Tab
      tabItem(tabName = "classification",
        fluidRow(
          box(width = 3,
            selectInput("class_target", "Target Class:", choices = NULL),
            selectizeInput("class_features", "Features:", choices = NULL, multiple = TRUE),
            selectInput("class_method", "Method:",
                       choices = c("Random Forest", "SVM", "Decision Tree")),
            actionButton("runClassification", "Run Classification", class = "btn-primary")
          ),
          box(width = 9,
            plotlyOutput("classificationPlot", height = "400px"),
            verbatimTextOutput("classificationMetrics")
          )
        )
      ),
      
      # Clustering Tab
      tabItem(tabName = "clustering",
        fluidRow(
          box(width = 3,
            selectizeInput("cluster_features", "Features:", choices = NULL, multiple = TRUE),
            selectInput("cluster_method", "Method:",
                       choices = c("K-Means", "Hierarchical", "DBSCAN")),
            conditionalPanel(
              condition = "input.cluster_method != 'DBSCAN'",
              numericInput("n_clusters", "Number of Clusters:", value = 3, min = 2)
            ),
            actionButton("runClustering", "Run Clustering", class = "btn-primary")
          ),
          box(width = 9,
            plotlyOutput("clusteringPlot", height = "400px"),
            verbatimTextOutput("clusteringSummary")
          )
        )
      ),
      
      # Anomaly Detection Tab
      tabItem(tabName = "anomaly",
        fluidRow(
          box(width = 3,
            selectInput("anom_target", "Target Variable:", choices = NULL),
            selectInput("anom_method", "Detection Method:",
                       choices = c("Statistical (3-sigma)", "IQR", "Isolation Forest")),
            actionButton("runAnomalyDetection", "Detect Anomalies", class = "btn-primary")
          ),
          box(width = 9,
            plotlyOutput("anomalyPlot", height = "400px"),
            DT::dataTableOutput("anomalyTable")
          )
        )
      ),
      
      # Model Management Tabs
      tabItem(tabName = "training",
        fluidRow(
          box(width = 12,
            title = "Model Training",
            verbatimTextOutput("trainingStatus"),
            DT::dataTableOutput("modelsList")
          )
        )
      ),
      
      tabItem(tabName = "deployment",
        fluidRow(
          box(width = 12,
            title = "Model Deployment",
            DT::dataTableOutput("deploymentTable"),
            actionButton("deployModel", "Deploy Selected Model")
          )
        )
      ),
      
      tabItem(tabName = "monitoring",
        fluidRow(
          valueBoxOutput("modelPerformance", width = 3),
          valueBoxOutput("modelHealth", width = 3),
          valueBoxOutput("modelAccuracy", width = 3),
          valueBoxOutput("modelLatency", width = 3)
        ),
        fluidRow(
          box(width = 12,
            plotlyOutput("performanceMetrics", height = "400px")
          )
        )
      ),
      
      tabItem(tabName = "explain",
        fluidRow(
          box(width = 3,
            selectInput("explainModel", "Select Model:", choices = NULL),
            actionButton("generateExplanation", "Generate Explanation")
          ),
          box(width = 9,
            plotlyOutput("featureImportance", height = "400px"),
            verbatimTextOutput("modelExplanation")
          )
        )
      )
    )
  ),
  controlbar = shinydashboardPlus::dashboardControlbar(
    id = "controlbar",
    skin = "light",
    title = "Settings & Options",
    shinydashboardPlus::controlbarMenu(
      id = "controlbarMenu",
      shinydashboardPlus::controlbarItem(
        title = "Appearance",
        shinydashboardPlus::skinSelector()
      ),
      shinydashboardPlus::controlbarItem(
        title = "Analysis Settings",
        sliderInput("confidence", "Confidence Level:", min = 0.8, max = 0.99, value = 0.95),
        checkboxInput("showUncertainty", "Show Uncertainty Bands", value = TRUE)
      )
    )
  )
)

shinyUI(ui)
