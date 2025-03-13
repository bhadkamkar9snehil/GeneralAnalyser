# general_forecasting_app.R
# A general-purpose forecasting tool using Shiny and shinydashboardPlus.
# - Accepts CSV or Excel files.
# - All inputs (file, column selection, options, algorithms, Run Analysis) are in the right-hand controlbar under "Inputs".
# - The Data Preview tab displays a table, basic summary statistics, a trend plot (faceted by column), and a correlation heatmap.
# - Combined Forecast, Aggregated Outputs, and Best Model are shown in separate menu items.
# - VAR/SVAR require at least one additional numeric predictor column (without missing values).
# - The Settings tab in the controlbar includes a skin selector and a ggplot theme selector.
# - Changing the ggplot theme re-renders all plots.

### 1) INSTALL AND LOAD REQUIRED PACKAGES
packages <- c("shiny", "shinydashboardPlus", "shinydashboard", "ggplot2", 
              "dplyr", "readxl", "forecast", "randomForest", "DT", 
              "reshape2", "vars", "Metrics", "ggthemes")
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
      menuItem("Data Preview", tabName = "data_preview", icon = icon("table")),
      menuItem("Combined Forecast", tabName = "combined_forecast", icon = icon("line-chart")),
      menuItem("Aggregated Outputs", tabName = "aggregated_outputs", icon = icon("chart-bar")),
      menuItem("Best Model", tabName = "best_model", icon = icon("trophy"))
    )
  ),
  body = dashboardBody(
    tabItems(
      # Data Preview Tab
      tabItem(tabName = "data_preview",
              fluidRow(
                shinydashboardPlus::box(
                  title = "Data Table (First 20 Rows)",
                  width = 12,
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  DT::dataTableOutput("previewTableMain")
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Basic Statistics",
                  width = 12,
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  DT::dataTableOutput("summaryStats")
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Trends for Numeric Columns",
                  width = 12,
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  plotOutput("trendPlot", height = "300px")
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Correlation Heatmap",
                  width = 12,
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  plotOutput("corPlot", height = "300px")
                )
              )
      ),
      # Combined Forecast Tab
      tabItem(tabName = "combined_forecast",
              fluidRow(
                shinydashboardPlus::box(
                  title = "Combined Forecast with Confidence Intervals",
                  width = 12,
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  plotOutput("combinedForecastPlot", height = "400px")
                )
              )
      ),
      # Aggregated Outputs Tab
      tabItem(tabName = "aggregated_outputs",
              fluidRow(
                shinydashboardPlus::box(
                  title = "Overall Forecasts (Test Set)",
                  width = 12,
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  plotOutput("forecastPlot", height = "300px")
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Overall Residuals (Test Set)",
                  width = 12,
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  plotOutput("residualPlot", height = "300px"),
                  DT::dataTableOutput("errorMetrics")
                )
              ),
              fluidRow(
                shinydashboardPlus::box(
                  title = "Algorithm Comparison (RMSE)",
                  width = 12,
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  plotOutput("comparisonPlot", height = "300px")
                )
              )
      ),
      # Best Model Tab
      tabItem(tabName = "best_model",
              fluidRow(
                shinydashboardPlus::box(
                  title = "Best Performing Model",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  uiOutput("bestAlgorithm")
                )
              )
      )
    )
  ),
  controlbar = shinydashboardPlus::dashboardControlbar(
    id = "controlbar",
    disable = FALSE,
    width = 500,
    collapsed = FALSE,
    overlay = TRUE,
    skin = "light",
    .list = list(
      shinydashboardPlus::controlbarMenu(
        id = "inputMenu",
        # Single merged Inputs tab
        shinydashboardPlus::controlbarItem(
          title = "Inputs",
          fileInput("file", "Upload CSV/Excel File", accept = c(".csv", ".xls", ".xlsx")),
          selectInput("time_col", "Select Time Column:", choices = NULL),
          selectInput("target_col", "Select Target Column:", choices = NULL),
          selectizeInput("multivar_cols", "Select Additional Columns (optional):",
                         choices = NULL, multiple = TRUE),
          sliderInput("split_ratio", "Training Ratio:", min = 0.1, max = 0.9, value = 0.8, step = 0.1),
          numericInput("future_periods", "Future Periods:", value = 10, min = 1),
          selectizeInput("algorithms", "Select Algorithm(s):", 
                         choices = c("ARIMA", "ETS", "NNETAR", "VAR", "SVAR", "Random Forest"),
                         multiple = TRUE),
          shiny::actionButton("runAnalysis", "Run Analysis", 
                              class = "btn btn-danger", 
                              style = "font-size: 18px; padding: 10px 20px; margin-top: 15px;")
        ),
        # Settings Tab with ggplot theme selector (including themes from ggthemes)
        shinydashboardPlus::controlbarItem(
          title = "Settings",
          shinydashboardPlus::skinSelector(),
          selectInput("ggplot_theme", "Select ggplot Theme:",
                      choices = c("Minimal", "Classic", "BW", "Light", "Void", 
                                  "Gray", "Economist", "Fivethirtyeight", "Stata", "Solarized"),
                      selected = "Minimal")
        )
      )
    )
  ),
  title = "General Forecasting Tool",
  skin = "black-light"
)

### 3) SERVER LOGIC
server <- function(input, output, session) {
  
  # Helper: Console logging
  logMsg <- function(msg) {
    message(paste(Sys.time(), "-", msg))
  }
  logMsg("Application started.")
  
  # Reactive: Load and parse file
  dataInput <- reactive({
    req(input$file)
    logMsg("File uploaded.")
    ext <- tools::file_ext(input$file$name)
    if(ext == "csv") {
      df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    } else if(ext %in% c("xls", "xlsx")) {
      df <- readxl::read_excel(input$file$datapath)
    } else {
      validate("Unsupported file type. Please upload a CSV or Excel file.")
    }
    logMsg(paste("File loaded with", nrow(df), "rows and", length(names(df)), "columns."))
    df
  })
  
  # Data Preview: Render table
  output$previewTableMain <- DT::renderDataTable({
    req(dataInput())
    DT::datatable(head(dataInput(), 20), options = list(pageLength = 10))
  })
  
  # Basic statistics for each column
  output$summaryStats <- DT::renderDataTable({
    req(dataInput())
    stats <- as.data.frame(summary(dataInput()))
    DT::datatable(stats, options = list(pageLength = 10))
  })
  
  # Trends for Numeric Columns with facets
  output$trendPlot <- renderPlot({
    req(dataInput(), input$time_col)
    df <- dataInput()
    df[[input$time_col]] <- as.POSIXct(df[[input$time_col]])
    df <- df[order(df[[input$time_col]]), ]
    num_cols <- names(df)[sapply(df, is.numeric)]
    num_cols <- setdiff(num_cols, input$time_col)
    if(length(num_cols) == 0) return(NULL)
    df_long <- reshape2::melt(df, id.vars = input$time_col, measure.vars = num_cols)
    ggplot(df_long, aes_string(x = input$time_col, y = "value")) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y") +
      labs(title = "Trends for Numeric Columns", x = "Time", y = "Value") +
      ggTheme()
  })
  
  # Correlation Heatmap using selected ggplot theme
  output$corPlot <- renderPlot({
    req(dataInput())
    df <- dataInput()
    num_df <- df[sapply(df, is.numeric)]
    if(ncol(num_df) < 2) return(NULL)
    cor_mat <- cor(num_df, use = "complete.obs")
    cor_df <- as.data.frame(as.table(cor_mat))
    ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      ggTheme()
  })
  
  # Update column choices when data is loaded
  observeEvent(dataInput(), {
    req(dataInput())
    allCols <- names(dataInput())
    updateSelectInput(session, "time_col", choices = allCols, selected = allCols[1])
    updateSelectInput(session, "target_col", choices = allCols, selected = if(length(allCols) > 1) allCols[2] else allCols[1])
    updateSelectizeInput(session, "multivar_cols", choices = allCols, selected = character(0))
  })
  
  # Reactive ggplot theme based on user selection
  ggTheme <- reactive({
    switch(input$ggplot_theme,
           "Minimal" = ggplot2::theme_minimal(),
           "Classic" = ggplot2::theme_classic(),
           "BW" = ggplot2::theme_bw(),
           "Light" = ggplot2::theme_light(),
           "Void" = ggplot2::theme_void(),
           "Gray" = ggplot2::theme_gray(),
           "Economist" = ggthemes::theme_economist(),
           "Fivethirtyeight" = ggthemes::theme_fivethirtyeight(),
           "Stata" = ggthemes::theme_stata(),
           "Solarized" = ggthemes::theme_solarized(),
           ggplot2::theme_gray()
    )
  })
  
  # Main Analysis Logic triggered solely by Run Analysis button
  results <- eventReactive(input$runAnalysis, {
    withProgress(message = "Running Analysis", value = 0, {
      logMsg("Analysis started.")
      
      incProgress(0.1, detail = "Preparing data...")
      req(dataInput(), input$time_col, input$target_col)
      df <- dataInput()
      df[[input$time_col]] <- as.POSIXct(df[[input$time_col]])
      df <- df[order(df[[input$time_col]]), ]
      logMsg("Data sorted by time.")
      
      target <- as.numeric(df[[input$target_col]])
      n <- nrow(df)
      train_size <- floor(input$split_ratio * n)
      train_df <- df[1:train_size, ]
      test_df <- df[(train_size + 1):n, ]
      train_target <- target[1:train_size]
      test_target <- target[(train_size + 1):n]
      logMsg(paste("Data split: Training =", train_size, "rows; Testing =", n - train_size, "rows."))
      
      incProgress(0.2, detail = "Generating future timestamps...")
      last_time <- tail(df[[input$time_col]], 1)
      diffs <- diff(df[[input$time_col]])
      interval_sec <- if(length(diffs) > 0) median(as.numeric(diffs, units = "secs")) else 86400
      future_times <- seq(from = last_time + interval_sec, by = interval_sec, length.out = input$future_periods)
      logMsg("Future timestamps generated.")
      
      model_preds <- list()
      error_metrics <- data.frame(Model = character(), RMSE = numeric(), MAPE = numeric(), stringsAsFactors = FALSE)
      
      incProgress(0.3, detail = "Running univariate models...")
      # ARIMA
      if("ARIMA" %in% input$algorithms) {
        logMsg("Running ARIMA model.")
        arima_model <- forecast::auto.arima(train_target)
        fc_test <- forecast::forecast(arima_model, h = length(test_target))
        preds_test <- as.numeric(fc_test$mean)
        rmse_val <- Metrics::rmse(test_target, preds_test)
        mape_val <- Metrics::mape(test_target, preds_test) * 100
        fc_future <- forecast::forecast(arima_model, h = length(test_target) + input$future_periods)
        future_preds <- tail(as.numeric(fc_future$mean), input$future_periods)
        model_preds[["ARIMA_test"]] <- preds_test
        model_preds[["ARIMA_future"]] <- future_preds
        model_preds[["ARIMA_fc"]] <- fc_test
        error_metrics <- rbind(error_metrics, data.frame(Model = "ARIMA", RMSE = rmse_val, MAPE = mape_val))
        logMsg("ARIMA model complete.")
      }
      # ETS
      if("ETS" %in% input$algorithms) {
        logMsg("Running ETS model.")
        ets_model <- forecast::ets(train_target)
        fc_test <- forecast::forecast(ets_model, h = length(test_target))
        preds_test <- as.numeric(fc_test$mean)
        rmse_val <- Metrics::rmse(test_target, preds_test)
        mape_val <- Metrics::mape(test_target, preds_test) * 100
        fc_future <- forecast::forecast(ets_model, h = length(test_target) + input$future_periods)
        future_preds <- tail(as.numeric(fc_future$mean), input$future_periods)
        model_preds[["ETS_test"]] <- preds_test
        model_preds[["ETS_future"]] <- future_preds
        model_preds[["ETS_fc"]] <- fc_test
        error_metrics <- rbind(error_metrics, data.frame(Model = "ETS", RMSE = rmse_val, MAPE = mape_val))
        logMsg("ETS model complete.")
      }
      # NNETAR
      if("NNETAR" %in% input$algorithms) {
        logMsg("Running NNETAR model.")
        nnet_model <- forecast::nnetar(train_target)
        fc_test <- forecast::forecast(nnet_model, h = length(test_target))
        preds_test <- as.numeric(fc_test$mean)
        rmse_val <- Metrics::rmse(test_target, preds_test)
        mape_val <- Metrics::mape(test_target, preds_test) * 100
        fc_future <- forecast::forecast(nnet_model, h = length(test_target) + input$future_periods)
        future_preds <- tail(as.numeric(fc_future$mean), input$future_periods)
        model_preds[["NNETAR_test"]] <- preds_test
        model_preds[["NNETAR_future"]] <- future_preds
        model_preds[["NNETAR_fc"]] <- fc_test
        error_metrics <- rbind(error_metrics, data.frame(Model = "NNETAR", RMSE = rmse_val, MAPE = mape_val))
        logMsg("NNETAR model complete.")
      }
      
      incProgress(0.5, detail = "Running multivariate models (VAR/SVAR)...")
      if(any(c("VAR", "SVAR") %in% input$algorithms)) {
        mv_cols <- c(input$target_col, input$multivar_cols)
        if(length(mv_cols) < 2) {
          logMsg("No additional columns selected for VAR/SVAR. Skipping.")
        } else {
          mv_data <- df[, mv_cols, drop = FALSE]
          mv_data <- as.data.frame(lapply(mv_data, as.numeric))
          mv_data <- mv_data[complete.cases(mv_data), ]
          if(nrow(mv_data) < 2) {
            logMsg("Not enough complete cases for VAR/SVAR. Skipping.")
            if("VAR" %in% input$algorithms) {
              error_metrics <- rbind(error_metrics, data.frame(Model = "VAR", RMSE = NA, MAPE = NA))
              model_preds[["VAR_test"]] <- rep(NA, length(test_target))
              model_preds[["VAR_future"]] <- rep(NA, input$future_periods)
            }
            if("SVAR" %in% input$algorithms) {
              error_metrics <- rbind(error_metrics, data.frame(Model = "SVAR", RMSE = NA, MAPE = NA))
              model_preds[["SVAR_test"]] <- rep(NA, length(test_target))
              model_preds[["SVAR_future"]] <- rep(NA, input$future_periods)
            }
          } else {
            n_mv <- nrow(mv_data)
            train_size_mv <- floor(input$split_ratio * n_mv)
            train_mv <- mv_data[1:train_size_mv, ]
            test_mv <- mv_data[(train_size_mv + 1):n_mv, ]
            # VAR
            if("VAR" %in% input$algorithms) {
              logMsg("Attempting VAR model calculation.")
              tryCatch({
                var_model <- vars::VAR(train_mv, p = 1, type = "const")
                fc_test <- predict(var_model, n.ahead = nrow(test_mv))
                preds_test <- fc_test$fcst[[input$target_col]][, "fcst"]
                rmse_val <- Metrics::rmse(test_mv[[input$target_col]], preds_test)
                mape_val <- Metrics::mape(test_mv[[input$target_col]], preds_test) * 100
                fc_future <- predict(var_model, n.ahead = nrow(test_mv) + input$future_periods)
                future_preds <- tail(fc_future$fcst[[input$target_col]][, "fcst"], input$future_periods)
                model_preds[["VAR_test"]] <- preds_test
                model_preds[["VAR_future"]] <- future_preds
                error_metrics <- rbind(error_metrics, data.frame(Model = "VAR", RMSE = rmse_val, MAPE = mape_val))
                logMsg("VAR model calculation successful.")
              }, error = function(e) {
                logMsg(paste("Error in VAR model:", e$message))
                error_metrics <<- rbind(error_metrics, data.frame(Model = "VAR", RMSE = NA, MAPE = NA))
                model_preds[["VAR_test"]] <<- rep(NA, nrow(test_mv))
                model_preds[["VAR_future"]] <<- rep(NA, input$future_periods)
              })
            }
            # SVAR
            if("SVAR" %in% input$algorithms) {
              logMsg("Attempting SVAR model calculation.")
              tryCatch({
                var_model <- vars::VAR(train_mv, p = 1, type = "const")
                svar_model <- try(svars::svar(var_model, estmethod = "direct"), silent = TRUE)
                if(!inherits(svar_model, "try-error")) {
                  fc_test <- predict(svar_model, n.ahead = nrow(test_mv))
                  preds_test <- fc_test$fcst[[input$target_col]][, "fcst"]
                  rmse_val <- Metrics::rmse(test_mv[[input$target_col]], preds_test)
                  mape_val <- Metrics::mape(test_mv[[input$target_col]], preds_test) * 100
                  fc_future <- predict(svar_model, n.ahead = nrow(test_mv) + input$future_periods)
                  future_preds <- tail(fc_test$fcst[[input$target_col]][, "fcst"], input$future_periods)
                  model_preds[["SVAR_test"]] <- preds_test
                  model_preds[["SVAR_future"]] <- future_preds
                  error_metrics <<- rbind(error_metrics, data.frame(Model = "SVAR", RMSE = rmse_val, MAPE = mape_val))
                  logMsg("SVAR model calculation successful.")
                } else {
                  logMsg("SVAR model failed during estimation.")
                  error_metrics <<- rbind(error_metrics, data.frame(Model = "SVAR", RMSE = NA, MAPE = NA))
                  model_preds[["SVAR_test"]] <<- rep(NA, nrow(test_mv))
                  model_preds[["SVAR_future"]] <<- rep(NA, input$future_periods)
                }
              }, error = function(e) {
                logMsg(paste("Error in SVAR model:", e$message))
                error_metrics <<- rbind(error_metrics, data.frame(Model = "SVAR", RMSE = NA, MAPE = NA))
                model_preds[["SVAR_test"]] <<- rep(NA, nrow(test_mv))
                model_preds[["SVAR_future"]] <<- rep(NA, input$future_periods)
              })
            }
          }
        }
      }
      
      incProgress(0.7, detail = "Running Random Forest model...")
      if("Random Forest" %in% input$algorithms) {
        logMsg("Running Random Forest model.")
        max_lag <- 3
        create_lagged_df <- function(series, max_lag) {
          df_lag <- data.frame(Target = series)
          for(i in 1:max_lag) {
            df_lag[[paste0("lag", i)]] <- dplyr::lag(series, n = i)
          }
          df_lag <- df_lag[(max_lag + 1):length(series), ]
          return(df_lag)
        }
        full_series <- target
        full_lagged <- create_lagged_df(full_series, max_lag)
        rf_train <- full_lagged[1:(train_size - max_lag), ]
        rf_test <- full_lagged[(train_size - max_lag + 1):(n - max_lag), ]
        set.seed(123)
        rf_model <- randomForest::randomForest(Target ~ ., data = rf_train)
        preds_test <- predict(rf_model, newdata = rf_test)
        actual_rf <- full_series[(train_size + 1):n]
        rmse_val <- Metrics::rmse(actual_rf, preds_test)
        mape_val <- Metrics::mape(actual_rf, preds_test) * 100
        rf_future <- numeric(input$future_periods)
        last_values <- tail(full_series, max_lag)
        for(i in 1:input$future_periods){
          new_data <- as.data.frame(t(last_values[1:max_lag]))
          colnames(new_data) <- paste0("lag", 1:max_lag)
          pred <- predict(rf_model, newdata = new_data)
          rf_future[i] <- pred
          last_values <- c(pred, last_values[1:(max_lag - 1)])
        }
        model_preds[["RF_test"]] <- preds_test
        model_preds[["RF_future"]] <- rf_future
        error_metrics <- rbind(error_metrics, data.frame(Model = "Random Forest", RMSE = rmse_val, MAPE = mape_val))
        logMsg("Random Forest model complete.")
      }
      
      incProgress(1, detail = "Analysis complete")
      logMsg("Analysis complete.")
      list(
        train_df = train_df,
        test_df = test_df,
        future_times = future_times,
        target_actual_test = test_target,
        model_preds = model_preds,
        error_metrics = error_metrics,
        full_data = df,
        full_target = target
      )
    })
  })
  
  ### Combined Forecast Plot (Historical + Future Forecasts with Confidence Intervals)
  output$combinedForecastPlot <- renderPlot({
    req(results())
    res <- results()
    df_full <- res$full_data
    time_full <- df_full[[input$time_col]]
    actual_full <- res$full_target
    p <- ggplot(data = data.frame(Time = time_full, Actual = actual_full), aes(x = Time, y = Actual)) +
      geom_line(color = "black") +
      labs(title = "Historical Data and Future Forecasts", y = "Target Value")
    
    # Loop through univariate models first
    univariateModels <- intersect(c("ARIMA", "ETS", "NNETAR"), input$algorithms)
    colors <- c("ARIMA" = "blue", "ETS" = "green", "NNETAR" = "purple")
    for(mod in univariateModels) {
      fc_obj <- res$model_preds[[paste0(mod, "_fc")]]
      if(!is.null(fc_obj) && length(fc_obj$mean) >= input$future_periods &&
         !is.null(fc_obj$lower) && nrow(fc_obj$lower) >= input$future_periods) {
        h_total <- length(fc_obj$mean)
        h_future <- input$future_periods
        start_idx <- h_total - h_future + 1
        dt <- if(length(time_full) > 1) median(diff(as.numeric(time_full))) else 1
        fc_time <- seq(from = max(time_full) + dt, by = dt, length.out = h_future)
        df_fc <- data.frame(Time = fc_time,
                            Forecast = as.numeric(fc_obj$mean[start_idx:h_total]),
                            Lower = as.numeric(fc_obj$lower[start_idx:h_total,1]),
                            Upper = as.numeric(fc_obj$upper[start_idx:h_total,1]))
        p <- p +
          geom_line(data = df_fc, aes(x = Time, y = Forecast), color = colors[mod], linetype = "dashed") +
          geom_ribbon(data = df_fc, aes(x = Time, ymin = Lower, ymax = Upper), 
                      fill = colors[mod], alpha = 0.2, inherit.aes = FALSE)
      }
    }
    # For other models, use future predictions vector if available
    otherModels <- setdiff(input$algorithms, univariateModels)
    for(mod in otherModels) {
      fc_future <- res$model_preds[[paste0(mod, "_future")]]
      if(!is.null(fc_future) && length(fc_future) >= input$future_periods) {
        h_future <- input$future_periods
        dt <- if(length(time_full) > 1) median(diff(as.numeric(time_full))) else 1
        fc_time <- seq(from = max(time_full) + dt, by = dt, length.out = h_future)
        df_fc <- data.frame(Time = fc_time, Forecast = fc_future[1:h_future])
        col_default <- switch(mod,
                              "VAR" = "orange",
                              "SVAR" = "red",
                              "Random Forest" = "brown",
                              "gray")
        p <- p +
          geom_line(data = df_fc, aes(x = Time, y = Forecast), color = col_default, linetype = "dashed")
      }
    }
    
    p + geom_vline(xintercept = max(time_full), linetype = "dotted", color = "red") + ggTheme()
  })
  
  ### Aggregated Output Plots
  output$forecastPlot <- renderPlot({
    req(results())
    res <- results()
    time_vals <- res$test_df[[input$time_col]]
    actual <- res$target_actual_test
    plot_df <- data.frame(Time = time_vals, Actual = actual)
    if("ARIMA_test" %in% names(res$model_preds))
      plot_df$ARIMA <- res$model_preds[["ARIMA_test"]]
    if("ETS_test" %in% names(res$model_preds))
      plot_df$ETS <- res$model_preds[["ETS_test"]]
    if("NNETAR_test" %in% names(res$model_preds))
      plot_df$NNETAR <- res$model_preds[["NNETAR_test"]]
    if("VAR_test" %in% names(res$model_preds))
      plot_df$VAR <- res$model_preds[["VAR_test"]]
    if("SVAR_test" %in% names(res$model_preds))
      plot_df$SVAR <- res$model_preds[["SVAR_test"]]
    if("RF_test" %in% names(res$model_preds))
      plot_df$`Random Forest` <- res$model_preds[["RF_test"]]
    
    plot_df_long <- reshape2::melt(plot_df, id.vars = "Time", variable.name = "Model", value.name = "Prediction")
    ggplot(plot_df_long, aes(x = Time, y = Prediction, color = Model)) +
      geom_line() +
      geom_line(data = plot_df, aes(x = Time, y = Actual), color = "black", size = 1.2, linetype = "dashed") +
      labs(title = "Test Set Forecasts", y = "Target Value") + ggTheme()
  })
  
  output$residualPlot <- renderPlot({
    req(results())
    res <- results()
    time_vals <- res$test_df[[input$time_col]]
    actual <- res$target_actual_test
    resid_df <- data.frame(Time = time_vals, Actual = actual)
    if("ARIMA_test" %in% names(res$model_preds))
      resid_df$ARIMA <- actual - res$model_preds[["ARIMA_test"]]
    if("ETS_test" %in% names(res$model_preds))
      resid_df$ETS <- actual - res$model_preds[["ETS_test"]]
    if("NNETAR_test" %in% names(res$model_preds))
      resid_df$NNETAR <- actual - res$model_preds[["NNETAR_test"]]
    if("VAR_test" %in% names(res$model_preds))
      resid_df$VAR <- actual - res$model_preds[["VAR_test"]]
    if("SVAR_test" %in% names(res$model_preds))
      resid_df$SVAR <- actual - res$model_preds[["SVAR_test"]]
    if("RF_test" %in% names(res$model_preds))
      resid_df$`Random Forest` <- actual - res$model_preds[["RF_test"]]
    
    resid_long <- reshape2::melt(resid_df, id.vars = "Time", variable.name = "Model", value.name = "Residual")
    ggplot(resid_long, aes(x = Time, y = Residual, color = Model)) +
      geom_line() +
      labs(title = "Overall Residuals (Test Set)", y = "Residual") + ggTheme()
  })
  
  output$errorMetrics <- DT::renderDataTable({
    req(results())
    res <- results()
    DT::datatable(res$error_metrics, options = list(pageLength = 5))
  })
  
  output$comparisonPlot <- renderPlot({
    req(results())
    res <- results()
    ggplot(res$error_metrics, aes(x = Model, y = RMSE, fill = Model)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Algorithm Comparison (RMSE)", y = "RMSE") + ggTheme()
  })
  
  ### Overlaid Forecast+Residual for each algorithm (Individual Results)
  plotCombined <- function(time_vals, actual, forecast, algName) {
    if(is.null(forecast) || all(is.na(forecast))) return(NULL)
    residual <- actual - forecast
    if(all(is.na(residual))) return(NULL)
    ggplot() +
      geom_line(aes(x = time_vals, y = actual), color = "black", size = 1.2) +
      geom_line(aes(x = time_vals, y = forecast), color = "blue", linetype = "dashed") +
      geom_line(aes(x = time_vals, y = residual), color = "red", linetype = "dotted") +
      labs(title = paste(algName, "Combined Results"), x = "Time", y = "Value") +
      theme_minimal() + ggTheme()
  }
  
  output$combined_ARIMA <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["ARIMA_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "ARIMA")
    }
  })
  
  output$combined_ETS <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["ETS_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "ETS")
    }
  })
  
  output$combined_NNETAR <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["NNETAR_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "NNETAR")
    }
  })
  
  output$combined_VAR <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["VAR_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "VAR")
    }
  })
  
  output$combined_SVAR <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["SVAR_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "SVAR")
    }
  })
  
  output$combined_RF <- renderPlot({
    req(results())
    res <- results()
    fc <- res$model_preds[["RF_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "Random Forest")
    }
  })
  
  ## Best Algorithm Display on Main Body
  output$bestAlgorithm <- renderUI({
    req(results())
    res <- results()
    if(nrow(res$error_metrics) > 0 && any(!is.na(res$error_metrics$RMSE))) {
      bestIdx <- which.min(res$error_metrics$RMSE)
      bestModel <- res$error_metrics$Model[bestIdx]
      bestRmse <- round(res$error_metrics$RMSE[bestIdx], 2)
      tags$div(class = "well", style = "font-size: 18px; padding: 15px; margin: 15px;",
               paste("Best Performing Model:", bestModel, "(RMSE:", bestRmse, ")"))
    } else {
      tags$div(class = "well", "No best model identified yet.")
    }
  })
}

### 4) RUN THE SHINY APP
shinyApp(ui, server)
