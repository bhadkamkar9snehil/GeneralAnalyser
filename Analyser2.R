# Install the packages if you haven't already:
# remotes::install_github("RinteRface/shinydashboardPlus")
# install.packages("shiny")
# install.packages("shinydashboard")

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

ui <- shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = "General Analytics Platform",
    enable_rightsidebar = TRUE,   # Enables the control bar toggle button
    rightSidebarIcon = "gears"    # Icon for the control bar toggle
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Data Hub", tabName = "datahub", icon = icon("database")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Models", tabName = "models", icon = icon("cogs")),
      menuItem("Live Dashboard", tabName = "live", icon = icon("signal")),
      menuItem("Settings", tabName = "settings", icon = icon("sliders-h"))
    )
  ),
  body = dashboardBody(
    # Include Tailwind CSS and custom glassmorphic styles
    tags$head(
      # Tailwind CSS via CDN
      tags$script(src = "https://cdn.tailwindcss.com"),
      tags$style(HTML("
        /* Glassmorphic UI styles */
        .glass {
          background: rgba(255, 255, 255, 0.2);
          box-shadow: 0 4px 30px rgba(0, 0, 0, 0.1);
          backdrop-filter: blur(5px);
          -webkit-backdrop-filter: blur(5px);
          border: 1px solid rgba(255, 255, 255, 0.3);
          border-radius: 15px;
          padding: 1rem;
          margin: 1rem;
        }
        body {
          background: url('https://www.transparenttextures.com/patterns/cubes.png');
        }
      "))
    ),
    tabItems(
      # Dashboard Overview (Phase 1)
      tabItem(
        tabName = "dashboard",
        fluidRow(
          column(
            width = 12,
            div(
              class = "glass",
              h2("Dashboard Overview"),
              p("Overview of MVP Release and core platform features (Phase 1).")
            )
          )
        )
      ),
      # Data Hub: File Upload, API Integration, Database Connectivity (Phase 1)
      tabItem(
        tabName = "datahub",
        fluidRow(
          column(
            width = 6,
            div(
              class = "glass",
              h3("File Upload"),
              fileInput("file", "Upload CSV/Excel/JSON/XML", 
                        accept = c(".csv", ".xlsx", ".json", ".xml"))
            )
          ),
          column(
            width = 6,
            div(
              class = "glass",
              h3("API Integration"),
              actionButton("api_config", "Configure API")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "glass",
              h3("Database Connectivity"),
              actionButton("db_connect", "Connect to Database")
            )
          )
        )
      ),
      # Analysis: Time Series, Regression, Classification, Clustering, Anomaly Detection (Phase 1)
      tabItem(
        tabName = "analysis",
        fluidRow(
          column(
            width = 4,
            div(
              class = "glass",
              h3("Time Series Analysis"),
              actionButton("ts_analysis", "Run ARIMA / Exponential Smoothing")
            )
          ),
          column(
            width = 4,
            div(
              class = "glass",
              h3("Regression Analysis"),
              actionButton("reg_analysis", "Run Linear / Logistic / Polynomial Regression")
            )
          ),
          column(
            width = 4,
            div(
              class = "glass",
              h3("Classification"),
              actionButton("class_analysis", "Run Decision Trees / Random Forest / SVM")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            div(
              class = "glass",
              h3("Clustering"),
              actionButton("clust_analysis", "Run K-Means / Hierarchical / DBSCAN")
            )
          ),
          column(
            width = 6,
            div(
              class = "glass",
              h3("Anomaly Detection"),
              actionButton("anom_detect", "Run Statistical / ML-based Detection")
            )
          )
        )
      ),
      # Models: Training, Deployment, Monitoring, Version Control & Explainability (Phase 2)
      tabItem(
        tabName = "models",
        fluidRow(
          column(
            width = 4,
            div(
              class = "glass",
              h3("Model Training"),
              actionButton("model_train", "Automated Hyperparameter Tuning")
            )
          ),
          column(
            width = 4,
            div(
              class = "glass",
              h3("Model Deployment"),
              actionButton("model_deploy", "One-click Deployment")
            )
          ),
          column(
            width = 4,
            div(
              class = "glass",
              h3("Model Monitoring"),
              actionButton("model_monitor", "Performance Tracking")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              class = "glass",
              h3("Version Control & Explainability"),
              actionButton("model_version", "View Version History & SHAP Values")
            )
          )
        )
      ),
      # Live Dashboard: Real-time updates (Phase 2)
      tabItem(
        tabName = "live",
        fluidRow(
          column(
            width = 12,
            div(
              class = "glass",
              h3("Real-time Dashboard"),
              p("Live data visualization and real-time updates will appear here.")
            )
          )
        )
      ),
      # Settings: Security and configuration settings
      tabItem(
        tabName = "settings",
        fluidRow(
          column(
            width = 12,
            div(
              class = "glass",
              h3("Settings"),
              p("Configure preferences, security settings, and other options.")
            )
          )
        )
      )
    )
  ),
  controlbar = dashboardControlbar(
    skin = "light",
    controlbarMenu(
      controlbarItem(
        title = "Quick Actions",
        icon = icon("sliders-h"),
        div(
          class = "p-4", 
          actionButton("quick_new_analysis", "New Analysis"),
          br(), br(),
          actionButton("quick_import", "Quick Import"),
          br(), br(),
          actionButton("quick_recent", "Recent Items")
        )
      )
    )
  ),
  title = "General Analytics Platform"
)

server <- function(input, output, session) {
  observeEvent(input$file, {
    showNotification("File uploaded successfully!", type = "message")
  })
  
  observeEvent(input$api_config, {
    showNotification("API configuration initiated.", type = "message")
  })
  
  observeEvent(input$db_connect, {
    showNotification("Database connection started.", type = "message")
  })
  
  observeEvent(input$ts_analysis, {
    showNotification("Time Series Analysis running...", type = "message")
  })
  
  observeEvent(input$reg_analysis, {
    showNotification("Regression Analysis running...", type = "message")
  })
  
  observeEvent(input$class_analysis, {
    showNotification("Classification Analysis running...", type = "message")
  })
  
  observeEvent(input$clust_analysis, {
    showNotification("Clustering Analysis running...", type = "message")
  })
  
  observeEvent(input$anom_detect, {
    showNotification("Anomaly Detection running...", type = "message")
  })
  
  observeEvent(input$model_train, {
    showNotification("Model training initiated...", type = "message")
  })
  
  observeEvent(input$model_deploy, {
    showNotification("Model deployment in progress...", type = "message")
  })
  
  observeEvent(input$model_monitor, {
    showNotification("Monitoring model performance...", type = "message")
  })
  
  observeEvent(input$model_version, {
    showNotification("Viewing model version and explainability metrics...", type = "message")
  })
  
  observeEvent(input$quick_new_analysis, {
    showNotification("New Analysis initiated.", type = "message")
  })
  
  observeEvent(input$quick_import, {
    showNotification("Quick Import initiated.", type = "message")
  })
  
  observeEvent(input$quick_recent, {
    showNotification("Displaying recent items.", type = "message")
  })
}

shinyApp(ui, server)
