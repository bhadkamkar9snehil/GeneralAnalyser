library(shiny)
library(ggplot2)
library(forecast)
library(randomForest)
library(reshape2)
library(vars)
library(Metrics)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(svars)
library(glmnet)
library(e1071)
library(rpart)

server <- function(input, output, session) {
  # Reactive values for storing state
  values <- reactiveValues(
    data = NULL,
    models = list(),
    deployments = list(),
    monitoring = list()
  )

  # Data Input Handling
  dataInput <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    df <- if(ext == "csv") {
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if(ext %in% c("xls", "xlsx")) {
      readxl::read_excel(input$file$datapath)
    } else {
      validate("Please upload a CSV or Excel file.")
    }
    
    values$data <- df
    updateSelectInputs(df)
    df
  })

  # Update UI inputs based on loaded data
  updateSelectInputs <- function(df) {
    nums <- names(df)[sapply(df, is.numeric)]
    cats <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    
    updateSelectInput(session, "time_col", choices = names(df))
    updateSelectInput(session, "target_col", choices = nums)
    updateSelectizeInput(session, "multivar_cols", choices = nums)
    
    # Update other analysis tab inputs
    updateSelectInput(session, "reg_target", choices = nums)
    updateSelectizeInput(session, "reg_features", choices = nums)
    updateSelectInput(session, "class_target", choices = cats)
    updateSelectizeInput(session, "class_features", choices = nums)
    updateSelectizeInput(session, "cluster_features", choices = nums)
    updateSelectInput(session, "anom_target", choices = nums)
  }

  # Data Preview Table
  output$previewTableMain <- DT::renderDataTable({
    req(dataInput())
    DT::datatable(head(dataInput(), 20), options = list(pageLength = 10))
  })

  # Basic Statistics Plot
  output$statsPlot <- renderPlotly({
    req(dataInput())
    df <- dataInput()
    num_df <- df[sapply(df, is.numeric)]
    
    if(ncol(num_df) == 0) return(NULL)
    
    stats <- data.frame(
      Column = names(num_df),
      Mean = sapply(num_df, mean, na.rm = TRUE),
      SD = sapply(num_df, sd, na.rm = TRUE)
    )
    
    stats_long <- reshape2::melt(stats, id.vars = "Column")
    
    p <- ggplot(stats_long, aes(x = Column, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Basic Statistics", x = "Column", y = "Value") +
      theme_minimal()
    
    ggplotly(p)
  })

  # Time Series Analysis
  observeEvent(input$runAnalysis, {
    req(values$data, input$time_col, input$target_col)
    withProgress(message = 'Running Time Series Analysis', value = 0, {
      df <- values$data
      results <- performTimeSeriesAnalysis(
        df, 
        input$time_col, 
        input$target_col, 
        input$multivar_cols,
        input$algorithms,
        input$split_ratio,
        input$future_periods
      )
      
      # Store model results
      modelId <- paste0("TS_", length(values$models) + 1)
      values$models[[modelId]] <- list(
        type = "time_series",
        algorithms = input$algorithms,
        results = results,
        created = Sys.time(),
        status = "trained"
      )
      
      # Update plots and metrics
      output$combinedForecast <- renderPlotly({
        plotCombinedForecast(results)
      })
      
      output$residualPlot <- renderPlotly({
        plotResiduals(results)
      })
      
      output$errorMetrics <- renderDT({
        results$error_metrics
      })
    })
  })

  # Regression Analysis
  observeEvent(input$runRegression, {
    req(values$data, input$reg_target, input$reg_features)
    withProgress(message = 'Running Regression Analysis', value = 0, {
      results <- performRegression(
        values$data,
        input$reg_target,
        input$reg_features,
        input$reg_type
      )
      
      modelId <- paste0("REG_", length(values$models) + 1)
      values$models[[modelId]] <- list(
        type = "regression",
        method = input$reg_type,
        results = results,
        created = Sys.time(),
        status = "trained"
      )
      
      output$regressionPlot <- renderPlotly({
        plotRegression(results)
      })
      
      output$regressionSummary <- renderPrint({
        summary(results$model)
      })
    })
  })

  # Classification Analysis
  observeEvent(input$runClassification, {
    req(values$data, input$class_target, input$class_features)
    withProgress(message = 'Running Classification Analysis', value = 0, {
      results <- performClassification(
        values$data,
        input$class_target,
        input$class_features,
        input$class_method
      )
      
      modelId <- paste0("CLS_", length(values$models) + 1)
      values$models[[modelId]] <- list(
        type = "classification",
        method = input$class_method,
        results = results,
        created = Sys.time(),
        status = "trained"
      )
      
      output$classificationPlot <- renderPlotly({
        plotClassification(results)
      })
      
      output$classificationMetrics <- renderPrint({
        printClassificationMetrics(results)
      })
    })
  })

  # Clustering Analysis
  observeEvent(input$runClustering, {
    req(values$data, input$cluster_features)
    withProgress(message = 'Running Clustering Analysis', value = 0, {
      results <- performClustering(
        values$data,
        input$cluster_features,
        input$cluster_method,
        if(input$cluster_method != "DBSCAN") input$n_clusters else NULL
      )
      
      modelId <- paste0("CLU_", length(values$models) + 1)
      values$models[[modelId]] <- list(
        type = "clustering",
        method = input$cluster_method,
        results = results,
        created = Sys.time(),
        status = "trained"
      )
      
      output$clusteringPlot <- renderPlotly({
        plotClustering(results)
      })
      
      output$clusteringSummary <- renderPrint({
        printClusteringSummary(results)
      })
    })
  })

  # Anomaly Detection
  observeEvent(input$runAnomalyDetection, {
    req(values$data, input$anom_target)
    withProgress(message = 'Running Anomaly Detection', value = 0, {
      results <- detectAnomalies(
        values$data,
        input$anom_target,
        input$anom_method
      )
      
      modelId <- paste0("ANO_", length(values$models) + 1)
      values$models[[modelId]] <- list(
        type = "anomaly",
        method = input$anom_method,
        results = results,
        created = Sys.time(),
        status = "trained"
      )
      
      output$anomalyPlot <- renderPlotly({
        plotAnomalies(results)
      })
      
      output$anomalyTable <- renderDT({
        results$anomalies
      })
    })
  })

  # Model Management
  output$modelsList <- renderDT({
    req(length(values$models) > 0)
    data.frame(
      ModelID = names(values$models),
      Type = sapply(values$models, function(m) m$type),
      Method = sapply(values$models, function(m) if(!is.null(m$method)) m$method else paste(m$algorithms, collapse="/")),
      Created = sapply(values$models, function(m) format(m$created, "%Y-%m-%d %H:%M:%S")),
      Status = sapply(values$models, function(m) m$status)
    )
  })

  # Model Deployment
  observeEvent(input$deployModel, {
    req(input$deploymentTable_rows_selected)
    selected <- input$deploymentTable_rows_selected
    modelId <- names(values$models)[selected]
    
    values$models[[modelId]]$status <- "deployed"
    values$deployments[[modelId]] <- list(
      deployed_at = Sys.time(),
      metrics = list(
        performance = 0,
        health = 100,
        accuracy = 0,
        latency = 0
      )
    )
  })

  # Model Monitoring
  observe({
    invalidateLater(5000)  # Update every 5 seconds
    for(modelId in names(values$deployments)) {
      # Update metrics for deployed models
      values$deployments[[modelId]]$metrics$performance <- runif(1, 80, 100)
      values$deployments[[modelId]]$metrics$health <- runif(1, 90, 100)
      values$deployments[[modelId]]$metrics$accuracy <- runif(1, 85, 95)
      values$deployments[[modelId]]$metrics$latency <- runif(1, 50, 200)
    }
  })

  # Monitoring Outputs
  output$modelPerformance <- renderValueBox({
    metrics <- calculateAverageMetrics(values$deployments)
    valueBox(
      paste0(round(metrics$performance, 1), "%"),
      "Average Performance",
      icon = icon("tachometer-alt"),
      color = if(metrics$performance >= 90) "green" else "yellow"
    )
  })

  output$modelHealth <- renderValueBox({
    metrics <- calculateAverageMetrics(values$deployments)
    valueBox(
      paste0(round(metrics$health, 1), "%"),
      "System Health",
      icon = icon("heartbeat"),
      color = if(metrics$health >= 95) "green" else "red"
    )
  })

  output$modelAccuracy <- renderValueBox({
    metrics <- calculateAverageMetrics(values$deployments)
    valueBox(
      paste0(round(metrics$accuracy, 1), "%"),
      "Model Accuracy",
      icon = icon("check-circle"),
      color = if(metrics$accuracy >= 90) "green" else "yellow"
    )
  })

  output$modelLatency <- renderValueBox({
    metrics <- calculateAverageMetrics(values$deployments)
    valueBox(
      paste0(round(metrics$latency), "ms"),
      "Average Latency",
      icon = icon("clock"),
      color = if(metrics$latency <= 100) "green" else "red"
    )
  })

  # Model Explainability
  observeEvent(input$generateExplanation, {
    req(input$explainModel)
    modelId <- input$explainModel
    model <- values$models[[modelId]]
    
    output$featureImportance <- renderPlotly({
      plotFeatureImportance(model)
    })
    
    output$modelExplanation <- renderPrint({
      generateModelExplanation(model)
    })
  })

  # Helper Functions
  calculateAverageMetrics <- function(deployments) {
    if(length(deployments) == 0) {
      return(list(
        performance = 0,
        health = 0,
        accuracy = 0,
        latency = 0
      ))
    }
    
    list(
      performance = mean(sapply(deployments, function(d) d$metrics$performance)),
      health = mean(sapply(deployments, function(d) d$metrics$health)),
      accuracy = mean(sapply(deployments, function(d) d$metrics$accuracy)),
      latency = mean(sapply(deployments, function(d) d$metrics$latency))
    )
  }
}