### 1) INSTALL AND LOAD REQUIRED PACKAGES
packages <- c("shiny", "shinydashboardPlus", "shinydashboard", "ggplot2", 
              "dplyr", "readxl", "forecast", "randomForest", "DT", 
              "reshape2", "vars", "Metrics")

for(pkg in packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

### 2) UI
ui <- shinydashboardPlus::dashboardPage(
  header = shinydashboardPlus::dashboardHeader(
    title = "General Forecasting Tool"
  ),
  sidebar = shinydashboardPlus::dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line")),
      menuItem("Analysis", tabName = "analysis", icon = icon("bar-chart"))
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              box(
                title = "Data Upload",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                fileInput("file", "Choose CSV or Excel file", accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv",
                  ".xls",
                  ".xlsx"
                )),
                selectInput("time_col", "Time Column:", choices = NULL),
                selectInput("target_col", "Target Column:", choices = NULL),
                selectizeInput("multivar_cols", "Multivariate Columns:", choices = NULL, multiple = TRUE),
                numericInput("split_ratio", "Training Ratio:", value = 0.8, min = 0.1, max = 0.9),
                numericInput("future_periods", "Future Periods:", value = 12, min = 1),
                selectInput("algorithms", "Algorithms:", 
                            choices = c("ARIMA", "ETS", "NNETAR", "VAR", "SVAR", "Random Forest"), 
                            multiple = TRUE)
              ),
              box(
                title = "Preview Table",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                DT::dataTableOutput("previewTableMain")
              )
      ),
      tabItem(tabName = "forecasting",
              box(
                title = "Forecasting Results",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                plotOutput("combinedForecastPlot", height = "600px")
              ),
              box(
                title = "Forecast Comparison",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                plotOutput("forecastPlot", height = "300px"),
                plotOutput("residualPlot", height = "300px")
              ),
              box(
                title = "Error Metrics",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                DT::dataTableOutput("errorMetrics")
              )
      ),
      tabItem(tabName = "analysis",
              box(
                title = "Algorithm Results",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                plotOutput("combined_ARIMA", height = "300px"),
                plotOutput("combined_ETS", height = "300px"),
                plotOutput("combined_NNETAR", height = "300px"),
                plotOutput("combined_VAR", height = "300px"),
                plotOutput("combined_SVAR", height = "300px"),
                plotOutput("combined_RF", height = "300px")
              ),
              box(
                title = "Best Algorithm",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                uiOutput("bestAlgorithm")
              )
      )
    )
  ),
  title = "General Forecasting Tool"
)

### 3) SERVER LOGIC
server <- function(input, output, session) {
  # Reactive: load and parse file
  dataInput <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    df <- if(ext == "csv") {
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if(ext %in% c("xls", "xlsx")) {
      readxl::read_excel(input$file$datapath)
    } else {
      stop("Unsupported file type. Please upload a CSV or Excel file.")
    }
    
    df[[input$time_col]] <- as.POSIXct(df[[input$time_col]])
    df <- df[order(df[[input$time_col]]), ]
    return(df)
  })
  
  # Update column choices when data is loaded
  observeEvent(dataInput(), {
    req(dataInput())
    updateSelectInput(session, "time_col", choices = names(dataInput()))
    updateSelectInput(session, "target_col", choices = names(dataInput()))
    updateSelectizeInput(session, "multivar_cols", choices = names(dataInput()))
  })
  
  # Preview table
  output$previewTableMain <- DT::renderDataTable({
    req(dataInput())
    DT::datatable(head(dataInput(), 20))
  })
  
  # Main Analysis Logic
  results <- eventReactive(input$runAnalysis, {
    req(dataInput(), input$time_col, input$target_col)
    df <- dataInput()
    
    target <- as.numeric(df[[input$target_col]])
    n <- nrow(df)
    train_size <- floor(input$split_ratio * n)
    train_df <- df[1:train_size, ]
    test_df <- df[(train_size + 1):n, ]
    train_target <- target[1:train_size]
    test_target <- target[(train_size + 1):n]
    
    model_preds <- list()
    error_metrics <- data.frame(Model = character(), RMSE = numeric(), MAPE = numeric(), 
                                stringsAsFactors = FALSE)
    
    # ARIMA
    if("ARIMA" %in% input$algorithms) {
      arima_model <- forecast::auto.arima(train_target)
      fc_test <- forecast::forecast(arima_model, h = length(test_target))
      preds_test <- as.numeric(fc_test$mean)
      model_preds[["ARIMA_test"]] <- preds_test
      model_preds[["ARIMA_fc"]] <- fc_test
      error_metrics <- rbind(error_metrics, 
                             data.frame(Model = "ARIMA", 
                                        RMSE = Metrics::rmse(test_target, preds_test),
                                        MAPE = Metrics::mape(test_target, preds_test) * 100))
    }
    
    # ETS
    if("ETS" %in% input$algorithms) {
      ets_model <- forecast::ets(train_target)
      fc_test <- forecast::forecast(ets_model, h = length(test_target))
      preds_test <- as.numeric(fc_test$mean)
      model_preds[["ETS_test"]] <- preds_test
      model_preds[["ETS_fc"]] <- fc_test
      error_metrics <- rbind(error_metrics, 
                             data.frame(Model = "ETS", 
                                        RMSE = Metrics::rmse(test_target, preds_test),
                                        MAPE = Metrics::mape(test_target, preds_test) * 100))
    }
    
    # NNETAR
    if("NNETAR" %in% input$algorithms) {
      nnet_model <- forecast::nnetar(train_target)
      fc_test <- forecast::forecast(nnet_model, h = length(test_target))
      preds_test <- as.numeric(fc_test$mean)
      model_preds[["NNETAR_test"]] <- preds_test
      model_preds[["NNETAR_fc"]] <- fc_test
      error_metrics <- rbind(error_metrics, 
                             data.frame(Model = "NNETAR", 
                                        RMSE = Metrics::rmse(test_target, preds_test),
                                        MAPE = Metrics::mape(test_target, preds_test) * 100))
    }
    
    # VAR/SVAR
    if(any(c("VAR", "SVAR") %in% input$algorithms) && length(input$multivar_cols) > 0) {
      mv_cols <- c(input$target_col, input$multivar_cols)
      mv_data <- df[, mv_cols, drop = FALSE]
      mv_data <- as.data.frame(lapply(mv_data, as.numeric))
      train_mv <- mv_data[1:train_size, ]
      
      if("VAR" %in% input$algorithms) {
        var_model <- vars::VAR(train_mv, p = 1, type = "const")
        fc_test <- predict(var_model, n.ahead = length(test_target))
        preds_test <- fc_test$fcst[[input$target_col]][, "fcst"]
        model_preds[["VAR_test"]] <- preds_test
        error_metrics <- rbind(error_metrics, 
                               data.frame(Model = "VAR", 
                                          RMSE = Metrics::rmse(test_target, preds_test),
                                          MAPE = Metrics::mape(test_target, preds_test) * 100))
      }
    }
    
    # Random Forest
    if("Random Forest" %in% input$algorithms) {
      rf_data <- data.frame(
        target = train_target,
        lag1 = c(NA, head(train_target, -1)),
        lag2 = c(NA, NA, head(train_target, -2)),
        lag3 = c(NA, NA, NA, head(train_target, -3))
      )
      rf_data <- na.omit(rf_data)
      
      rf_model <- randomForest::randomForest(target ~ ., data = rf_data)
      
      test_rf <- data.frame(
        lag1 = tail(train_target, 1),
        lag2 = tail(train_target, 2)[1],
        lag3 = tail(train_target, 3)[1]
      )
      
      preds_test <- numeric(length(test_target))
      for(i in seq_along(test_target)) {
        preds_test[i] <- predict(rf_model, newdata = test_rf)
        test_rf$lag3 <- test_rf$lag2
        test_rf$lag2 <- test_rf$lag1
        test_rf$lag1 <- preds_test[i]
      }
      
      model_preds[["RF_test"]] <- preds_test
      error_metrics <- rbind(error_metrics, 
                             data.frame(Model = "Random Forest", 
                                        RMSE = Metrics::rmse(test_target, preds_test),
                                        MAPE = Metrics::mape(test_target, preds_test) * 100))
    }
    
    list(
      train_df = train_df,
      test_df = test_df,
      target_actual_test = test_target,
      model_preds = model_preds,
      error_metrics = error_metrics,
      full_data = df,
      full_target = target
    )
  })
  
  # Plot outputs
  output$combinedForecastPlot <- renderPlot({
    req(results())
    res <- results()
    df_full <- res$full_data
    time_full <- df_full[[input$time_col]]
    actual_full <- res$full_target
    
    p <- ggplot() +
      geom_line(data = data.frame(Time = time_full, Actual = actual_full),
                aes(x = Time, y = Actual), color = "black")
    
    model_colors <- c("ARIMA" = "blue", "ETS" = "red", "NNETAR" = "green",
                      "VAR" = "purple", "SVAR" = "orange", "Random Forest" = "brown")
    
    for(mod in names(model_colors)) {
      mod_key <- paste0(mod, "_test")
      if(mod_key %in% names(res$model_preds)) {
        fc <- res$model_preds[[mod_key]]
        if(!is.null(fc) && !all(is.na(fc))) {
          test_data <- data.frame(
            Time = res$test_df[[input$time_col]],
            Forecast = fc
          )
          p <- p + geom_line(data = test_data,
                             aes(x = Time, y = Forecast),
                             color = model_colors[mod],
                             linetype = "dashed")
        }
      }
    }
    
    p + theme_minimal() +
      labs(title = "Combined Forecast Plot",
           x = "Time",
           y = "Value")
  })
  
  output$forecastPlot <- renderPlot({
    req(results())
    res <- results()
    time_vals <- res$test_df[[input$time_col]]
    actual <- res$target_actual_test
    plot_df <- data.frame(Time = time_vals, Actual = actual)
    
    for(model in c("ARIMA", "ETS", "NNETAR", "VAR", "SVAR", "Random Forest")) {
      key <- paste0(model, "_test")
      if(key %in% names(res$model_preds)) {
        plot_df[[model]] <- res$model_preds[[key]]
      }
    }
    
    plot_df_long <- reshape2::melt(plot_df, id.vars = "Time",
                                   variable.name = "Model",
                                   value.name = "Value")
    
    ggplot(plot_df_long, aes(x = Time, y = Value, color = Model)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Test Set Forecasts")
  })
  
  output$residualPlot <- renderPlot({
    req(results())
    res <- results()
    time_vals <- res$test_df[[input$time_col]]
    actual <- res$target_actual_test
    resid_df <- data.frame(Time = time_vals)
    
    for(model in c("ARIMA", "ETS", "NNETAR", "VAR", "SVAR", "Random Forest")) {
      key <- paste0(model, "_test")
      if(key %in% names(res$model_preds)) {
        resid_df[[model]] <- actual - res$model_preds[[key]]
      }
    }
    
    resid_long <- reshape2::melt(resid_df, id.vars = "Time",
                                 variable.name = "Model",
                                 value.name = "Residual")
    
    ggplot(resid_long, aes(x = Time, y = Residual, color = Model)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Residuals Plot")
  })
  
  output$errorMetrics <- DT::renderDataTable({
    req(results())
    DT::datatable(results()$error_metrics)
  })
  
  # Individual model plots
  plotCombined <- function(time_vals, actual, forecast, algName) {
    if(is.null(forecast)) return(NULL)
    residual <- actual - forecast
    ggplot() +
      geom_line(aes(x = time_vals, y = actual), color = "black") +
      geom_line(aes(x = time_vals, y = forecast), color = "blue", linetype = "dashed") +
      geom_line(aes(x = time_vals, y = residual), color = "red", linetype = "dotted") +
      labs(title = paste(algName, "Results"), x = "Time", y = "Value") +
      theme_minimal()
  }
  
  output$combined_ARIMA <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["ARIMA_test"]]
    if(!is.null(fc)){
      plotCombined(res$test_df[[input$time_col]], res$target_actual_test, fc, "ARIMA")
    }
  })
  
  output$combined_ETS <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["ETS_test"]]
    if(!is.null(fc)){
      plotCombined(res$test_df[[input$time_col]], res$target_actual_test, fc, "ETS")
    }
  })
  
  output$combined_NNETAR <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["NNETAR_test"]]
    if(!is.null(fc)){
      plotCombined(res$test_df[[input$time_col]], res$target_actual_test, fc, "NNETAR")
    }
  })
  
  output$combined_VAR <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["VAR_test"]]
    if(!is.null(fc)){
      plotCombined(res$test_df[[input$time_col]], res$target_actual_test, fc, "VAR")
    }
  })
  
  output$combined_SVAR <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["SVAR_test"]]
    if(!is.null(fc)){
      plotCombined(res$test_df[[input$time_col]], res$target_actual_test, fc, "SVAR")
    }
  })
  
  output$combined_RF <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["RF_test"]]
    if(!is.null(fc)){
      plotCombined(res$test_df[[input$time_col]], res$target_actual_test, fc, "Random Forest")
    }
  })
  
  output$bestAlgorithm <- renderUI({
    req(results())
    res <- results()
    if(nrow(res$error_metrics) > 0) {
      bestIdx <- which.min(res$error_metrics$RMSE)
      bestModel <- res$error_metrics$Model[bestIdx]
      bestRmse <- round(res$error_metrics$RMSE[bestIdx], 2)
      tags$div(
        paste("Best Performing Model:", bestModel, "(RMSE:", bestRmse, ")")
      )
    }
  })
}

### 4) RUN THE SHINY APP
shinyApp(ui, server)