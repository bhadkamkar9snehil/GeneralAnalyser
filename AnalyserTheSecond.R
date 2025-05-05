# Load required packages
library(shiny)
library(shinydashboard)      # dashboard layout components&#8203;:contentReference[oaicite:0]{index=0}
library(shinyWidgets)       # additional UI widgets (e.g., actionBttn, switchInput)
library(DT)                 # data table output for data preview and model list

# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "AI App Dashboard"),  # header with title
  dashboardSidebar(
    sidebarMenu(
      # Sidebar menu with items and sub-items&#8203;:contentReference[oaicite:1]{index=1}
      menuItem("Dashboard", tabName = "overview", icon = icon("dashboard")),
      menuItem("Data Hub", icon = icon("database"),
               menuSubItem("File Upload", tabName = "fileupload", icon = icon("upload")),
               menuSubItem("API Configuration", tabName = "apiconfig", icon = icon("cloud")),
               menuSubItem("Database Connection", tabName = "database", icon = icon("server"))
      ),
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
      ),
      menuItem("Live Dashboard", tabName = "live", icon = icon("wifi")),
      menuItem("Settings", tabName = "settings", icon = icon("sliders"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard Overview tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("dataStatus"),    # dynamic value box for data status
                valueBoxOutput("modelCount"),    # dynamic value box for model count
                valueBoxOutput("anomalyCount")   # dynamic value box for anomalies detected
              ),
              fluidRow(
                box(title = "Overview", width = 12, solidHeader = TRUE, status = "primary",
                    "Welcome to the AI App Dashboard. This overview shows key metrics and system status.")
              )
      ),
      # Data Hub - File Upload tab
      tabItem(tabName = "fileupload",
              h3("Upload Data File"),
              fluidRow(
                box(title = "File Upload", status = "info", solidHeader = TRUE, width = 6,
                    fileInput("datafile", "Choose CSV File", accept = c(".csv", ".txt")),
                    verbatimTextOutput("uploadInfo")  # text output for file info or status
                ),
                box(title = "Data Preview", status = "info", solidHeader = TRUE, width = 6,
                    DTOutput("fileDataPreview")  # preview of uploaded data (first rows)
                )
              )
      ),
      # Data Hub - API Configuration tab
      tabItem(tabName = "apiconfig",
              h3("Configure API Data Source"),
              fluidRow(
                box(title = "API Configuration", status = "info", solidHeader = TRUE, width = 6,
                    textInput("apiEndpoint", "API Endpoint URL:"),
                    textInput("apiKey", "API Key:"),
                    shinyWidgets::actionBttn("fetchData", "Fetch Data", 
                                             style = "gradient", color = "primary")
                ),
                box(title = "API Data Preview", status = "info", solidHeader = TRUE, width = 6,
                    DTOutput("apiDataPreview")  # preview of data fetched from API
                )
              ),
              verbatimTextOutput("apiStatus")  # status message for API fetch
      ),
      # Data Hub - Database Connection tab
      tabItem(tabName = "database",
              h3("Connect to Database"),
              fluidRow(
                box(title = "Database Connection", status = "info", solidHeader = TRUE, width = 6,
                    textInput("dbHost", "Host:", placeholder = "e.g. localhost"),
                    numericInput("dbPort", "Port:", value = 3306, min = 1, max = 65535),
                    textInput("dbUser", "User:"),
                    passwordInput("dbPassword", "Password:"),
                    textInput("dbName", "Database Name:"),
                    shinyWidgets::actionBttn("dbConnect", "Connect", 
                                             style = "gradient", color = "success")
                ),
                box(title = "DB Preview / Status", status = "info", solidHeader = TRUE, width = 6,
                    DTOutput("dbDataPreview")   # preview of data from DB or status message
                )
              ),
              verbatimTextOutput("dbStatus")  # connection status message
      ),
      # Analysis - Time Series tab
      tabItem(tabName = "timeseries",
              h3("Time Series Analysis"),
              conditionalPanel(
                "output.dataLoaded",
                # Only show controls when data is available
                selectInput("tsDateCol", "Date/Time Column:", choices = NULL),
                selectInput("tsValueCol", "Value Column:", choices = NULL)
              ),
              actionButton("runTS", "Run Analysis"),
              plotOutput("tsPlot")
      ),
      # Analysis - Regression tab
      tabItem(tabName = "regression",
              h3("Regression Analysis"),
              selectInput("regY", "Response Variable:", choices = NULL),
              selectInput("regX", "Predictors:", choices = NULL, multiple = TRUE),
              actionButton("runReg", "Run Regression"),
              verbatimTextOutput("regResult")
      ),
      # Analysis - Classification tab
      tabItem(tabName = "classification",
              h3("Classification Analysis"),
              selectInput("classTarget", "Target Variable (categorical):", choices = NULL),
              selectInput("classFeatures", "Features:", choices = NULL, multiple = TRUE),
              actionButton("runClass", "Run Classification"),
              verbatimTextOutput("classResult")
      ),
      # Analysis - Clustering tab
      tabItem(tabName = "clustering",
              h3("Clustering Analysis"),
              numericInput("clusterK", "Number of Clusters (K):", value = 3, min = 2, max = 10),
              actionButton("runCluster", "Run Clustering"),
              plotOutput("clusterPlot")
      ),
      # Analysis - Anomaly Detection tab
      tabItem(tabName = "anomaly",
              h3("Anomaly Detection"),
              selectInput("anomalyVar", "Select Numeric Variable:", choices = NULL),
              actionButton("runAnomaly", "Detect Anomalies"),
              verbatimTextOutput("anomalyResult")
      ),
      # Model Management - Training tab
      tabItem(tabName = "training",
              h3("Train a New Model"),
              selectInput("modelType", "Model Type:", 
                          choices = c("Linear Regression", "Logistic Regression", "Decision Tree", "Random Forest")),
              actionButton("trainModel", "Train Model"),
              verbatimTextOutput("trainStatus")
      ),
      # Model Management - Deployment tab
      tabItem(tabName = "deployment",
              h3("Deploy Trained Models"),
              DTOutput("modelTable"),           # table of trained models
              actionButton("deployModel", "Deploy Selected Model"),
              verbatimTextOutput("deployStatus")
      ),
      # Model Management - Monitoring tab
      tabItem(tabName = "monitoring",
              h3("Model Monitoring"),
              valueBoxOutput("performanceBox"), # dynamic performance metric
              plotOutput("monitorPlot")
      ),
      # Model Management - Explainability tab
      tabItem(tabName = "explain",
              h3("Model Explainability"),
              selectInput("explainModel", "Select Model to Explain:", choices = NULL),
              plotOutput("explainPlot"),
              verbatimTextOutput("explainText")
      ),
      # Live Dashboard tab
      tabItem(tabName = "live",
              h3("Live Dashboard"),
              fluidRow(
                valueBoxOutput("liveData"),   # live updating value box
                valueBoxOutput("liveData2")   # second live metric (optional)
              ),
              fluidRow(
                box(title = "Real-time Data", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("livePlot1", height = 250)
                ),
                box(title = "Real-time Distribution", status = "primary", solidHeader = TRUE, width = 6,
                    plotOutput("livePlot2", height = 250)
                )
              )
      ),
      # Settings tab
      tabItem(tabName = "settings",
              h3("Application Settings"),
              pickerInput("theme", "Theme Color:", 
                          choices = c("Blue", "Black", "Purple", "Green", "Red", "Yellow"), selected = "Blue"),
              shinyWidgets::switchInput("darkMode", label = "Dark Mode", value = FALSE),
              sliderInput("refreshRate", "Live Refresh Interval (seconds):", min = 1, max = 10, value = 1),
              actionButton("saveSettings", "Save Settings"),
              verbatimTextOutput("settingsStatus")
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  # Reactive storage for data and models
  dataVals <- reactiveValues(data = NULL, models = list())
  
  # Flag for data loaded (used in conditionalPanel)
  output$dataLoaded <- reactive({
    return(!is.null(dataVals$data))
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  # File Upload handling
  observeEvent(input$datafile, {
    req(input$datafile)
    # Read CSV file into data frame
    df <- tryCatch(read.csv(input$datafile$datapath, stringsAsFactors = FALSE), 
                   error = function(e) { NULL })
    if (!is.null(df)) {
      dataVals$data <- df
      output$uploadInfo <- renderText({
        paste("Uploaded file:", input$datafile$name, "with", nrow(df), "rows and", ncol(df), "columns.")
      })
    } else {
      output$uploadInfo <- renderText({ "Error in reading file. Please check format." })
    }
  })
  output$fileDataPreview <- renderDT({
    req(dataVals$data)
    datatable(head(dataVals$data, 50), options = list(scrollX = TRUE))
  })
  
  # API Data fetch handling
  observeEvent(input$fetchData, {
    # Simulate fetching data from API
    req(nzchar(input$apiEndpoint))
    # (In a real app, would use httr or similar to GET data; here we simulate a result)
    dummy <- data.frame(Result = "Sample API data fetched", Timestamp = Sys.time())
    dataVals$data <- dummy
    output$apiStatus <- renderText({
      paste("Fetched data from API at", input$apiEndpoint)
    })
  })
  output$apiDataPreview <- renderDT({
    req(dataVals$data)
    datatable(dataVals$data, options = list(scrollX = TRUE, dom = 't'))
  })
  
  # Database Connection handling
  observeEvent(input$dbConnect, {
    # Simulate a database connection and data retrieval
    if (nzchar(input$dbHost) && nzchar(input$dbUser) && nzchar(input$dbName)) {
      # (In real app, use DBI to connect and retrieve data)
      dummy <- data.frame(Status = "Connected to DB", Time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      dataVals$data <- dummy
      output$dbStatus <- renderText({ "Database connection successful." })
    } else {
      output$dbStatus <- renderText({ "Please provide host, user, and database name." })
    }
  })
  output$dbDataPreview <- renderDT({
    req(dataVals$data)
    datatable(dataVals$data, options = list(dom = 't'))
  })
  
  # Update analysis inputs when data is loaded
  observeEvent(dataVals$data, {
    df <- dataVals$data
    # Time Series: populate date and value column choices
    dateCols <- names(df)[sapply(df, function(x) inherits(x, c("Date", "POSIXct", "POSIXt")))]
    numCols <- names(df)[sapply(df, is.numeric)]
    updateSelectInput(session, "tsDateCol", choices = dateCols, selected = if(length(dateCols)>0) dateCols[1] else NULL)
    updateSelectInput(session, "tsValueCol", choices = numCols, selected = if(length(numCols)>0) numCols[1] else NULL)
    # Regression: choices for response (numeric) and predictors (all except response)
    updateSelectInput(session, "regY", choices = numCols, selected = if(length(numCols)>0) numCols[1] else NULL)
    updateSelectInput(session, "regX", choices = names(df), selected = if(length(numCols)>1) numCols[-1] else NULL)
    # Classification: target choices (factor or character) and features
    catCols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    updateSelectInput(session, "classTarget", choices = catCols, selected = if(length(catCols)>0) catCols[1] else NULL)
    updateSelectInput(session, "classFeatures", choices = names(df), selected = NULL)
    # Clustering: (numeric columns already have numCols)
    # Anomaly: numeric columns
    updateSelectInput(session, "anomalyVar", choices = numCols, selected = if(length(numCols)>0) numCols[1] else NULL)
    # Model explainability: model choices (when models exist, handled separately in model observe)
  })
  
  # Time Series analysis output
  observeEvent(input$runTS, {
    output$tsPlot <- renderPlot({
      req(dataVals$data)
      df <- dataVals$data
      if (!is.null(input$tsDateCol) && !is.null(input$tsValueCol) &&
          input$tsDateCol %in% names(df) && input$tsValueCol %in% names(df)) {
        # Plot selected date vs value columns as time series
        plot(df[[input$tsDateCol]], df[[input$tsValueCol]], type = "l", col = "blue",
             xlab = input$tsDateCol, ylab = input$tsValueCol, main = "Time Series Plot")
      } else {
        plot.new()
        text(0.5, 0.5, "Please select a date/time and value column for time series.", cex = 1.1)
      }
    })
  })
  
  # Regression analysis output
  observeEvent(input$runReg, {
    output$regResult <- renderPrint({
      req(dataVals$data)
      df <- dataVals$data
      # Ensure selections exist
      y <- input$regY; x <- input$regX
      if (is.null(y) || y == "" || length(x) == 0) {
        cat("Please select a response and at least one predictor.")
      } else if (!(y %in% names(df)) || any(!(x %in% names(df)))) {
        cat("Invalid selection.")
      } else {
        # Build formula and fit linear model
        form <- as.formula(paste(y, "~", paste(x, collapse = "+")))
        model <- lm(form, data = df)
        print(summary(model)$coefficients)
      }
    })
  })
  
  # Classification analysis output
  observeEvent(input$runClass, {
    output$classResult <- renderPrint({
      req(dataVals$data)
      df <- dataVals$data
      target <- input$classTarget; feats <- input$classFeatures
      if (is.null(target) || target == "" || length(feats) == 0) {
        cat("Please select a target variable and features.")
      } else if (!(target %in% names(df))) {
        cat("Invalid target selection.")
      } else {
        # Convert target to factor if not already
        df[[target]] <- as.factor(df[[target]])
        if (nlevels(df[[target]]) != 2) {
          cat("Only binary classification is supported in this demo.")
        } else {
          # Simple logistic regression as example
          fmla <- as.formula(paste(target, "~", paste(feats, collapse = "+")))
          model <- glm(fmla, data = df, family = binomial)
          coef_table <- summary(model)$coefficients
          print(coef_table)
        }
      }
    })
  })
  
  # Clustering analysis output
  observeEvent(input$runCluster, {
    output$clusterPlot <- renderPlot({
      req(dataVals$data)
      df <- dataVals$data
      numVars <- df[sapply(df, is.numeric)]
      if (ncol(numVars) < 1) {
        plot.new()
        text(0.5, 0.5, "No numeric data available for clustering.", cex = 1.2)
      } else {
        # Use first two numeric variables for plotting if available
        vars <- if (ncol(numVars) >= 2) numVars[, 1:2] else data.frame(V1 = numVars[[1]])
        k <- input$clusterK
        km <- kmeans(vars, centers = k)
        if (ncol(vars) == 1) {
          # One-dimensional clustering: plot as strip with points colored by cluster
          stripchart(vars[[1]] ~ km$cluster, method = "jitter", pch = 19, col = km$cluster,
                     main = paste("Clustering on", names(numVars)[1]), xlab = "Cluster", ylab = names(numVars)[1])
        } else {
          plot(vars[,1], vars[,2], col = km$cluster, pch = 19,
               xlab = names(vars)[1], ylab = names(vars)[2],
               main = paste("Clustering (k=", k, ")", sep=""))
        }
      }
    })
  })
  
  # Anomaly detection output
  observeEvent(input$runAnomaly, {
    output$anomalyResult <- renderPrint({
      req(dataVals$data)
      df <- dataVals$data
      var <- input$anomalyVar
      if (is.null(var) || var == "" || !(var %in% names(df)) || !is.numeric(df[[var]])) {
        cat("Please select a numeric variable for anomaly detection.")
      } else {
        x <- df[[var]]
        mu <- mean(x, na.rm = TRUE); sigma <- sd(x, na.rm = TRUE)
        threshHigh <- mu + 3 * sigma; threshLow <- mu - 3 * sigma
        anomalies <- which(x < threshLow | x > threshHigh)
        if (length(anomalies) == 0) {
          cat("No anomalies detected in", var)
        } else {
          cat("Anomalies detected in", var, "at rows:", paste(anomalies, collapse = ", "))
        }
      }
    })
  })
  
  # Model Training
  observeEvent(input$trainModel, {
    req(dataVals$data)
    # Simulate training a model (no actual model fitting for demo)
    model_id <- paste0("Model_", length(dataVals$models) + 1)
    dataVals$models[[model_id]] <- list(
      type = input$modelType,
      trained = Sys.time(),
      status = "Trained",
      accuracy = round(runif(1, 0.70, 0.95), 3)  # random accuracy metric
    )
    output$trainStatus <- renderText({
      paste("Trained", input$modelType, "-", model_id, "added to model list.")
    })
    # Update model selection choices for explainability
    updateSelectInput(session, "explainModel", choices = names(dataVals$models))
  })
  
  # Model list table (for Deployment)
  output$modelTable <- renderDT({
    models <- dataVals$models
    if (length(models) == 0) {
      return(datatable(data.frame(Message = "No models trained yet"), options = list(dom = 't')))
    }
    df <- data.frame(
      ModelID = names(models),
      Type = sapply(models, function(m) m$type),
      Status = sapply(models, function(m) m$status),
      Accuracy = sapply(models, function(m) m$accuracy),
      Trained = sapply(models, function(m) format(m$trained, "%Y-%m-%d %H:%M:%S")),
      stringsAsFactors = FALSE, row.names = NULL
    )
    datatable(df, selection = "single", options = list(scrollX = TRUE))
  })
  
  # Deploy model
  observeEvent(input$deployModel, {
    req(length(dataVals$models) > 0)
    sel <- input$modelTable_rows_selected
    if (length(sel)) {
      modNames <- names(dataVals$models)
      model_id <- modNames[sel]
      dataVals$models[[model_id]]$status <- "Deployed"
      output$deployStatus <- renderText({ paste(model_id, "deployed.") })
    }
  })
  
  # Performance monitoring (simulate real-time metrics for deployed model)
  output$performanceBox <- renderValueBox({
    # Show current number of deployed models as performance indicator
    deployed_count <- sum(sapply(dataVals$models, function(m) m$status == "Deployed"))
    valueBox(value = deployed_count, subtitle = "Models Deployed", icon = icon("signal"),
             color = if (deployed_count > 0) "green" else "yellow")
  })
  # Monitor plot updating over time
  vals <- reactiveValues(perf = numeric())
  observe({
    invalidateLater(input$refreshRate * 1000, session)  # refresh based on settings
    if (any(sapply(dataVals$models, function(m) m$status == "Deployed"))) {
      # Append a new random performance metric
      vals$perf <- c(vals$perf, runif(1, min = 0.80, max = 0.95))
      if (length(vals$perf) > 100) vals$perf <- vals$perf[-1]  # keep last 100 points
    } else {
      vals$perf <- numeric(0)  # reset if no deployed model
    }
  })
  output$monitorPlot <- renderPlot({
    # Plot performance metric trend&#8203;:contentReference[oaicite:2]{index=2}
    if (length(vals$perf) == 0) {
      plot.new()
      text(0.5, 0.5, "No deployed model to monitor.", cex = 1.2)
    } else {
      plot(vals$perf, type = "l", ylim = c(0,1), col = "orange",
           xlab = "Time (ticks)", ylab = "Metric", main = "Model Performance Over Time")
      abline(h = mean(vals$perf), col = "grey", lty = 2)
    }
  })
  
  # Model Explainability
  output$explainPlot <- renderPlot({
    if (length(dataVals$models) == 0) {
      plot.new()
      text(0.5, 0.5, "No models available for explanation.", cex = 1.2)
    } else {
      # Choose the selected model or default to last model
      modName <- input$explainModel
      if (is.null(modName) || modName == "" || !(modName %in% names(dataVals$models))) {
        modName <- tail(names(dataVals$models), 1)
      }
      mod_type <- dataVals$models[[modName]]$type
      # Generate dummy feature importance values
      features <- paste("Feature", 1:5)
      importance <- sort(runif(5, 0, 1), decreasing = TRUE)
      barplot(importance, names.arg = features, col = "steelblue",
              main = paste(mod_type, "- Feature Importance"))
    }
  })
  output$explainText <- renderText({
    if (length(dataVals$models) == 0) {
      "No model to explain."
    } else {
      paste("Displaying feature importance for model:", 
            ifelse(input$explainModel == "" || is.null(input$explainModel), 
                   tail(names(dataVals$models), 1), input$explainModel))
    }
  })
  
  # Live Dashboard outputs (real-time updates)
  output$liveData <- renderValueBox({
    # Dynamic value (e.g., system utilization percentage)
    invalidateLater(input$refreshRate * 1000, session)
    value <- round(runif(1, 0, 100), 1)
    valueBox(value = paste(value, "%"), subtitle = "Utilization", icon = icon("tachometer"),
             color = if (value > 80) "red" else if (value > 50) "yellow" else "green")
  })
  output$liveData2 <- renderValueBox({
    # Another dynamic value (e.g., active connections)
    invalidateLater(input$refreshRate * 1000, session)
    count <- sample(0:1000, 1)
    valueBox(value = count, subtitle = "Active Connections", icon = icon("users"),
             color = if (count > 500) "purple" else "blue")
  })
  output$livePlot1 <- renderPlot({
    invalidateLater(input$refreshRate * 1000, session)
    # Plot a simple real-time trend (e.g., random walk)
    plot(cumsum(rnorm(20)), type = "l", col = "darkgreen",
         main = "Real-time Metric", xlab = "Time", ylab = "Value")
  })
  output$livePlot2 <- renderPlot({
    invalidateLater(input$refreshRate * 1000, session)
    hist(rnorm(100), main = "Live Data Distribution", xlab = NULL, col = "skyblue")
  })
  
  # Settings logic
  observeEvent(input$saveSettings, {
    # (In a real app, apply theme or preferences as needed)
    showNotification("Settings have been saved.", type = "message")
    output$settingsStatus <- renderText({
      paste("Settings updated. (Theme:", input$theme, 
            "| Dark Mode:", ifelse(input$darkMode, "On", "Off"), 
            "| Refresh Interval:", input$refreshRate, "s)")
    })
  })
  
  # Overview dashboard value boxes
  output$dataStatus <- renderValueBox({
    # Show whether data is loaded and basic info&#8203;:contentReference[oaicite:3]{index=3}
    if (is.null(dataVals$data)) {
      valueBox("No Data", "Data Loaded", icon = icon("database"), color = "red")
    } else {
      rows <- nrow(dataVals$data)
      valueBox(paste(rows, "rows"), "Data Loaded", icon = icon("database"), color = "green")
    }
  })
  output$modelCount <- renderValueBox({
    # Number of models trained
    count <- length(dataVals$models)
    valueBox(count, "Models Trained", icon = icon("cubes"),
             color = if (count > 0) "green" else "yellow")
  })
  output$anomalyCount <- renderValueBox({
    # Count anomalies in selected numeric var (if any) as a simple metric
    if (is.null(dataVals$data) || !any(sapply(dataVals$data, is.numeric))) {
      valueBox("-", "Anomalies Detected", icon = icon("exclamation-triangle"), color = "yellow")
    } else {
      # Use the variable selected in anomaly tab or first numeric
      var <- if (!is.null(input$anomalyVar) && input$anomalyVar != "" && input$anomalyVar %in% names(dataVals$data)) {
        input$anomalyVar
      } else {
        names(dataVals$data)[sapply(dataVals$data, is.numeric)][1]
      }
      x <- dataVals$data[[var]]
      mu <- mean(x, na.rm = TRUE); sigma <- sd(x, na.rm = TRUE)
      anoms <- length(which(x < mu - 3 * sigma | x > mu + 3 * sigma))
      valueBox(anoms, "Anomalies Detected", icon = icon("exclamation-triangle"),
               color = if (anoms > 0) "red" else "green")
    }
  })
}

# Create Shiny app
shinyApp(ui = ui, server = server)
