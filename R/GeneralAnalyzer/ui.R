library(shiny)
library(DT)
library(plotly)
library(shinydashboardPlus)

ui <- dashboardPage(
  header = dashboardHeader(
    title = "General Analytics Platform",
    dropdownMenu(
      type = "tasks",
      badgeStatus = "success",
      taskItem(value = 80, "Data Processing"),
      taskItem(value = 90, "Analysis Pipeline"),
      taskItem(value = 70, "Model Deployment")
    )
  ),
  
  sidebar = dashboardSidebar(
    width = 0  # Hide sidebar for single page layout
  ),
  
  body = dashboardBody(
    # Custom CSS for better spacing
    tags$head(
      tags$style(HTML("
        .content-wrapper { margin-left: 0 !important; }
        .box { margin-bottom: 15px; }
        .analysis-section { padding: 10px; }
      "))
    ),
    
    fluidRow(
      # Data Input and Preview Section
      box(
        width = 12,
        title = "Data Input & Preview",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        fluidRow(
          column(3,
            fileInput("file", "Upload Data File",
                     accept = c(".csv", ".xls", ".xlsx")),
            actionButton("processData", "Process Data", 
                        class = "btn-primary btn-block")
          ),
          column(9,
            DT::dataTableOutput("previewTableMain")
          )
        )
      )
    ),
    
    fluidRow(
      # Analysis Configuration Section
      box(
        width = 3,
        title = "Analysis Configuration",
        status = "primary",
        solidHeader = TRUE,
        
        selectInput("analysisType", "Analysis Type",
                   choices = c("Time Series", "Regression", 
                             "Classification", "Clustering", 
                             "Anomaly Detection")),
        
        # Dynamic UI based on analysis type
        conditionalPanel(
          condition = "input.analysisType == 'Time Series'",
          selectInput("time_col", "Time Column:", choices = NULL),
          selectInput("target_col", "Target Column:", choices = NULL),
          selectizeInput("multivar_cols", "Additional Variables:", 
                       choices = NULL, multiple = TRUE),
          selectizeInput("algorithms", "Select Algorithm(s):", 
                       choices = c("ARIMA", "ETS", "NNETAR", "VAR", 
                                 "SVAR", "Random Forest"),
                       multiple = TRUE),
          sliderInput("split_ratio", "Training Split:", 
                     min = 0.1, max = 0.9, value = 0.8),
          numericInput("future_periods", "Forecast Horizon:", 
                      value = 10, min = 1)
        ),
        
        conditionalPanel(
          condition = "input.analysisType == 'Regression'",
          selectInput("reg_target", "Target Variable:", choices = NULL),
          selectizeInput("reg_features", "Features:", 
                       choices = NULL, multiple = TRUE),
          selectInput("reg_type", "Method:",
                     choices = c("Linear", "Ridge", "Lasso", "Random Forest"))
        ),
        
        conditionalPanel(
          condition = "input.analysisType == 'Classification'",
          selectInput("class_target", "Target Class:", choices = NULL),
          selectizeInput("class_features", "Features:", 
                       choices = NULL, multiple = TRUE),
          selectInput("class_method", "Method:",
                     choices = c("Random Forest", "SVM", "Decision Tree"))
        ),
        
        conditionalPanel(
          condition = "input.analysisType == 'Clustering'",
          selectizeInput("cluster_features", "Features:", 
                       choices = NULL, multiple = TRUE),
          selectInput("cluster_method", "Method:",
                     choices = c("K-Means", "Hierarchical", "DBSCAN")),
          conditionalPanel(
            condition = "input.cluster_method != 'DBSCAN'",
            numericInput("n_clusters", "Number of Clusters:", 
                       value = 3, min = 2)
          )
        ),
        
        conditionalPanel(
          condition = "input.analysisType == 'Anomaly Detection'",
          selectInput("anom_target", "Target Variable:", choices = NULL),
          selectInput("anom_method", "Method:",
                     choices = c("Statistical", "IQR", "Isolation Forest")),
          numericInput("anom_threshold", "Threshold:", 
                      value = 0.95, min = 0.8, max = 0.99)
        ),
        
        actionButton("runAnalysis", "Run Analysis", 
                    class = "btn-primary btn-block")
      ),
      
      # Results Section
      box(
        width = 9,
        title = "Analysis Results",
        status = "primary",
        solidHeader = TRUE,
        
        tabsetPanel(
          id = "resultsTabs",
          
          # Overview Tab
          tabPanel("Overview",
            fluidRow(
              valueBoxOutput("modelPerformance", width = 6),
              valueBoxOutput("modelAccuracy", width = 6)
            ),
            plotlyOutput("actualVsPredicted", height = "400px")
          ),
          
          # Detailed Results Tab
          tabPanel("Detailed Results",
            conditionalPanel(
              condition = "input.analysisType == 'Time Series'",
              plotlyOutput("combinedForecast"),
              plotlyOutput("residualPlot"),
              DT::dataTableOutput("errorMetrics")
            ),
            conditionalPanel(
              condition = "input.analysisType == 'Regression'",
              plotlyOutput("regressionPlot"),
              verbatimTextOutput("regressionSummary")
            ),
            conditionalPanel(
              condition = "input.analysisType == 'Classification'",
              plotlyOutput("confusionMatrix"),
              plotlyOutput("rocCurve"),
              verbatimTextOutput("classificationMetrics")
            ),
            conditionalPanel(
              condition = "input.analysisType == 'Clustering'",
              plotlyOutput("clusteringPlot"),
              verbatimTextOutput("clusteringSummary")
            ),
            conditionalPanel(
              condition = "input.analysisType == 'Anomaly Detection'",
              plotlyOutput("anomalyPlot"),
              DT::dataTableOutput("anomalyTable")
            )
          ),
          
          # Model Management Tab
          tabPanel("Model Management",
            DT::dataTableOutput("modelsList"),
            actionButton("deployModel", "Deploy Selected Model",
                        class = "btn-success")
          )
        )
      )
    )
  ),
  
  controlbar = dashboardControlbar(
    skin = "light",
    pinned = TRUE,
    collapsed = TRUE,
    
    controlbarMenu(
      id = "controlbarMenu",
      controlbarItem(
        "Settings",
        sliderInput("confidence", "Confidence Level:",
                   min = 0.8, max = 0.99, value = 0.95),
        checkboxInput("showUncertainty", "Show Uncertainty Bands", 
                     value = TRUE)
      ),
      controlbarItem(
        "Appearance",
        selectInput("plotTheme", "Plot Theme:",
                   choices = c("Light", "Dark", "Minimal")),
        selectInput("colorPalette", "Color Palette:",
                   choices = c("Viridis", "Magma", "Blues", "Set1"))
      )
    )
  )
)

shinyUI(ui)
