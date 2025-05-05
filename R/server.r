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
  
  # --- Missing Data Handling ---
  cleanDecision <- reactiveVal(NULL)
  observeEvent(input$removeMissing, {
    cleanDecision("remove")
    removeModal()
  })
  observeEvent(input$imputeMissing, {
    cleanDecision("impute")
    removeModal()
  })
  
  logMsg("Application started.")
  
  # --- Data Loading ---
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
  
  # --- Data Preview ---
  output$previewTableMain <- DT::renderDataTable({
    req(dataInput())
    DT::datatable(head(dataInput(), 20), options = list(pageLength = 10))
  })
  
  # --- Basic Statistics Plot ---
  output$statsPlot <- renderPlotly({
    req(dataInput())
    df <- dataInput()
    num_df <- df[sapply(df, is.numeric)]
    if(ncol(num_df) < 1) return(NULL)
    stats <- data.frame(Column = names(num_df),
                        Mean = sapply(num_df, mean, na.rm = TRUE),
                        SD = sapply(num_df, sd, na.rm = TRUE))
    stats_long <- melt(stats, id.vars = "Column", variable.name = "Statistic", value.name = "Value")
    p <- ggplot(stats_long, aes(x = Column, y = Value, fill = Statistic)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Basic Statistics", x = "Column", y = "Value") +
      theme_minimal() + getChartTheme("Basic Statistics")
    ggplotly(p)
  })
  
  # --- Trends Plot ---
  output$trendPlot <- renderPlotly({
    req(dataInput(), input$time_col)
    df <- dataInput()
    df[[input$time_col]] <- as.POSIXct(df[[input$time_col]])
    if(any(is.na(df[[input$time_col]]))) {
      stop("Time column could not be parsed correctly. Please check your file.")
    }
    df <- df[order(df[[input$time_col]]), ]
    num_cols <- setdiff(names(df)[sapply(df, is.numeric)], input$time_col)
    if(length(num_cols) == 0) return(NULL)
    df_long <- melt(df, id.vars = input$time_col, measure.vars = num_cols)
    p <- ggplot(df_long, aes_string(x = input$time_col, y = "value")) +
      geom_line() +
      facet_wrap(~variable, scales = "free_y") +
      labs(title = "Trends for Numeric Columns", x = "Time", y = "Value") +
      theme_minimal() + getChartTheme("Trend Plot")
    ggplotly(p)
  })
  
  # --- Correlation Heatmap ---
  output$corPlot <- renderPlotly({
    req(dataInput())
    df <- dataInput()
    num_df <- df[sapply(df, is.numeric)]
    if(ncol(num_df) < 2) return(NULL)
    cor_mat <- cor(num_df, use = "complete.obs")
    cor_df <- as.data.frame(as.table(cor_mat))
    corColors <- switch(input$cor_color_scheme,
                        "Blue-Red" = list(low = "blue", high = "red"),
                        "Purple-Green" = list(low = "purple", high = "green"),
                        "Orange-Blue" = list(low = "orange", high = "blue"),
                        list(low = "blue", high = "red"))
    p <- ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low = corColors$low, mid = "white", high = corColors$high, midpoint = 0) +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      theme_minimal() + getChartTheme("Correlation Heatmap")
    ggplotly(p)
  })
  
  # --- Update Column Choices ---
  observeEvent(dataInput(), {
    req(dataInput())
    allCols <- names(dataInput())
    updateSelectInput(session, "time_col", choices = allCols, selected = allCols[1])
    updateSelectInput(session, "target_col", choices = allCols, selected = if(length(allCols) > 1) allCols[2] else allCols[1])
    updateSelectizeInput(session, "multivar_cols", choices = allCols, selected = character(0))
    updateSelectizeInput(session, "regressors", choices = allCols, selected = character(0))
  })
  
  # --- Reactive Themes and Colour Palettes ---
  ggTheme <- reactive({
    base_theme <- switch(input$ggplot_theme,
                         "Minimal" = theme_minimal(),
                         "Classic" = theme_classic(),
                         "BW" = theme_bw(),
                         "Light" = theme_light(),
                         "Void" = theme_void(),
                         "Gray" = theme_gray(),
                         "Economist" = ggthemes::theme_economist(),
                         "Fivethirtyeight" = ggthemes::theme_fivethirtyeight(),
                         "Stata" = ggthemes::theme_stata(),
                         "Solarized" = ggthemes::theme_solarized(),
                         theme_minimal())
    base_theme +
      theme(
        axis.title = element_text(size = if(input$adjust_font) input$title_text_size else 12),
        axis.text = element_text(size = if(input$adjust_font) input$axis_text_size else 10),
        plot.title = element_text(size = if(input$adjust_font) input$title_text_size else 14, face = "bold"),
        axis.text.x = if(input$rotate_x) element_text(angle = 45, hjust = 1) else element_text(angle = 0)
      )
  })
  
  seriesColors <- reactive({
    pal <- switch(input$series_palette,
                  "Default" = brewer.pal(6, "Set1"),
                  "Set 2"   = brewer.pal(6, "Set2"),
                  "Dark2"   = brewer.pal(6, "Dark2"),
                  "Pastel1" = brewer.pal(6, "Pastel1"),
                  brewer.pal(6, "Set1"))
    names(pal) <- c("ARIMA", "ETS", "NNETAR", "VAR", "SVAR", "Random Forest")
    pal
  })
  
  getChartTheme <- function(chartID) {
    if (is.null(input$chart_to_customize)) return(theme_minimal())
    if (input$chart_to_customize == "All Charts" || input$chart_to_customize == chartID) {
      return(ggTheme())
    } else {
      return(theme_minimal())
    }
  }
  
  # --- Main Analysis Logic ---
  regression_algs <- c("Linear Regression", "Ridge Regression", "Lasso Regression",
                       "Elastic Net", "Random Forest Regression", "Support Vector Regression", "Decision Tree Regression")
  
  results <- eventReactive(input$runAnalysis, {
    withProgress(message = "Running Analysis", value = 0, {
      logMsg("Analysis started.")
      
      incProgress(0.1, detail = "Preparing data...")
      req(dataInput(), input$time_col, input$target_col)
      df <- dataInput()
      df[[input$time_col]] <- as.POSIXct(df[[input$time_col]])
      if(any(is.na(df[[input$time_col]]))) {
        stop("Time column could not be parsed correctly. Please check your file.")
      }
      df <- df[order(df[[input$time_col]]), ]
      logMsg("Data sorted by time.")
      
      target <- as.numeric(df[[input$target_col]])
      n <- nrow(df)
      if(n == 0) stop("No data available after loading.")
      
      n_tail <- min(10, length(target))
      if(any(is.na(tail(target, n_tail)))) {
        showModal(modalDialog(
          title = "Missing Data Detected",
          "There are missing values near the end of your target series. How would you like to handle them?",
          footer = tagList(
            modalButton("Cancel Analysis"),
            actionButton("removeMissing", "Remove Missing Rows"),
            actionButton("imputeMissing", "Impute Missing Values")
          )
        ))
        validate(need(!is.null(cleanDecision()), "Please decide how to handle missing values near the end."))
        if(cleanDecision() == "remove") {
          df <- df[!is.na(df[[input$target_col]]), ]
          target <- as.numeric(df[[input$target_col]])
          logMsg("Missing rows removed.")
        } else if(cleanDecision() == "impute") {
          target <- impute_values(target)
          df[[input$target_col]] <- target
          logMsg("Missing values imputed (forward fill).")
        }
      }
      
      train_size <- floor(input$split_ratio * n)
      train_df <- df[1:train_size, ]
      test_df <- df[(train_size + 1):n, ]
      train_target <- target[1:train_size]
      test_target <- target[(train_size + 1):n]
      logMsg(paste("Data split: Training =", train_size, "rows; Testing =", n - train_size, "rows."))
      
      incProgress(0.2, detail = "Generating future timestamps...")
      last_time <- tail(df[[input$time_col]], 1)
      if(length(last_time) != 1 || is.na(last_time))
        stop("Invalid time value in time column.")
      diffs <- diff(df[[input$time_col]])
      interval_sec <- if(length(diffs) > 0) median(as.numeric(diffs, units = "secs")) else 86400
      future_times <- seq(from = last_time + interval_sec, by = interval_sec, length.out = input$future_periods)
      logMsg("Future timestamps generated.")
      
      model_preds <- list()
      error_metrics <- data.frame(Model = character(), RMSE = numeric(), MAPE = numeric(), stringsAsFactors = FALSE)
      
      incProgress(0.3, detail = "Running forecasting models...")
      if("ARIMA" %in% input$algorithms) {
        logMsg("Running ARIMA model.")
        arima_model <- auto.arima(train_target)
        fc_test <- forecast(arima_model, h = length(test_target))
        preds_test <- as.numeric(fc_test$mean)
        rmse_val <- rmse(test_target, preds_test)
        mape_val <- mape(test_target, preds_test) * 100
        fc_future <- forecast(arima_model, h = length(test_target) + input$future_periods)
        future_preds <- tail(as.numeric(fc_future$mean), input$future_periods)
        model_preds[["ARIMA_test"]] <- preds_test
        model_preds[["ARIMA_future"]] <- future_preds
        model_preds[["ARIMA_fc"]] <- fc_test
        error_metrics <- rbind(error_metrics, data.frame(Model = "ARIMA", RMSE = rmse_val, MAPE = mape_val))
        logMsg("ARIMA model complete.")
      }
      if("ETS" %in% input$algorithms) {
        logMsg("Running ETS model.")
        ets_model <- ets(train_target)
        fc_test <- forecast(ets_model, h = length(test_target))
        preds_test <- as.numeric(fc_test$mean)
        rmse_val <- rmse(test_target, preds_test)
        mape_val <- mape(test_target, preds_test) * 100
        fc_future <- forecast(ets_model, h = length(test_target) + input$future_periods)
        future_preds <- tail(as.numeric(fc_future$mean), input$future_periods)
        model_preds[["ETS_test"]] <- preds_test
        model_preds[["ETS_future"]] <- future_preds
        model_preds[["ETS_fc"]] <- fc_test
        error_metrics <- rbind(error_metrics, data.frame(Model = "ETS", RMSE = rmse_val, MAPE = mape_val))
        logMsg("ETS model complete.")
      }
      if("NNETAR" %in% input$algorithms) {
        logMsg("Running NNETAR model.")
        nnet_model <- nnetar(train_target)
        fc_test <- forecast(nnet_model, h = length(test_target))
        preds_test <- as.numeric(fc_test$mean)
        rmse_val <- rmse(test_target, preds_test)
        mape_val <- mape(test_target, preds_test) * 100
        fc_future <- forecast(nnet_model, h = length(test_target) + input$future_periods)
        future_preds <- tail(as.numeric(fc_test$mean), input$future_periods)
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
          mv_data <- as.data.frame(lapply(mv_data, function(x) as.numeric(as.character(x))))
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
            if("VAR" %in% input$algorithms) {
              logMsg("Attempting VAR model calculation.")
              tryCatch({
                var_model <- VAR(train_mv, p = 1, type = "const")
                fc_test <- predict(var_model, n.ahead = nrow(test_mv))
                preds_test <- fc_test$fcst[[input$target_col]][, "fcst"]
                rmse_val <- rmse(test_mv[[input$target_col]], preds_test)
                mape_val <- mape(test_mv[[input$target_col]], preds_test) * 100
                fc_future <- predict(var_model, n.ahead = nrow(test_mv) + input$future_periods)
                future_preds <- tail(fc_test$fcst[[input$target_col]][, "fcst"], input$future_periods)
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
            if("SVAR" %in% input$algorithms) {
              logMsg("Attempting SVAR model calculation.")
              tryCatch({
                var_model <- VAR(train_mv, p = 1, type = "const")
                svar_model <- try(svars::svar(var_model, estmethod = "direct"), silent = TRUE)
                if(!inherits(svar_model, "try-error")) {
                  fc_test <- predict(svar_model, n.ahead = nrow(test_mv))
                  preds_test <- fc_test$fcst[[input$target_col]][, "fcst"]
                  rmse_val <- rmse(test_mv[[input$target_col]], preds_test)
                  mape_val <- mape(test_mv[[input$target_col]], preds_test) * 100
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
        rf_model <- randomForest(Target ~ ., data = rf_train)
        preds_test <- predict(rf_model, newdata = rf_test)
        actual_rf <- full_series[(train_size + 1):n]
        rmse_val <- rmse(actual_rf, preds_test)
        mape_val <- mape(actual_rf, preds_test) * 100
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
      
      # --- Regression Analysis ---
      reg_results <- list()
      if(input$runRegression && any(input$algorithms %in% regression_algs)) {
        incProgress(0.8, detail = "Running regression analysis...")
        req(input$regressors)
        reg_df <- dataInput()[, c(input$target_col, input$regressors), drop = FALSE]
        reg_df_clean <- na.omit(reg_df)
        if(nrow(reg_df_clean) == 0) {
          stop("No complete cases available for regression analysis.")
        }
        # For each regression algorithm selected, fit a model:
        reg_algs <- intersect(input$algorithms, regression_algs)
        for(algo in reg_algs) {
          # Wrap column names in backticks to handle spaces
          target_formula <- sprintf("`%s`", input$target_col)
          predictor_formula <- paste(sprintf("`%s`", input$regressors), collapse = " + ")
          reg_formula <- as.formula(paste(target_formula, "~", predictor_formula))
          model <- NULL
          pred <- NULL
          switch(algo,
                 "Linear Regression" = {
                   model <- lm(reg_formula, data = reg_df_clean)
                   pred <- predict(model, newdata = reg_df_clean)
                 },
                 "Ridge Regression" = {
                   x <- model.matrix(reg_formula, data = reg_df_clean)[, -1, drop=FALSE]
                   y <- reg_df_clean[[input$target_col]]
                   model <- glmnet::cv.glmnet(x, y, alpha = 0)
                   pred <- as.numeric(predict(model, s = "lambda.min", newx = x))
                 },
                 "Lasso Regression" = {
                   x <- model.matrix(reg_formula, data = reg_df_clean)[, -1, drop=FALSE]
                   y <- reg_df_clean[[input$target_col]]
                   model <- glmnet::cv.glmnet(x, y, alpha = 1)
                   pred <- as.numeric(predict(model, s = "lambda.min", newx = x))
                 },
                 "Elastic Net" = {
                   x <- model.matrix(reg_formula, data = reg_df_clean)[, -1, drop=FALSE]
                   y <- reg_df_clean[[input$target_col]]
                   model <- glmnet::cv.glmnet(x, y, alpha = 0.5)
                   pred <- as.numeric(predict(model, s = "lambda.min", newx = x))
                 },
                 "Random Forest Regression" = {
                   model <- randomForest::randomForest(reg_formula, data = reg_df_clean)
                   pred <- predict(model, newdata = reg_df_clean)
                 },
                 "Support Vector Regression" = {
                   model <- e1071::svm(reg_formula, data = reg_df_clean)
                   pred <- predict(model, newdata = reg_df_clean)
                 },
                 "Decision Tree Regression" = {
                   model <- rpart::rpart(reg_formula, data = reg_df_clean)
                   pred <- predict(model, newdata = reg_df_clean)
                 }
          )
          reg_results[[algo]] <- list(model = model, predictions = pred, data = reg_df_clean)
        }
      }
      
      incProgress(1, detail = "Analysis complete")
      logMsg("Analysis complete.")
      cleanDecision(NULL)
      list(
        train_df = train_df,
        test_df = test_df,
        future_times = future_times,
        target_actual_test = test_target,
        model_preds = model_preds,
        error_metrics = error_metrics,
        full_data = df,
        full_target = target,
        regression = if(length(reg_results) > 0) reg_results else NULL
      )
    })
  })
  
  # --- Combined Forecast Plot ---
  combinedForecastPlotObject <- reactive({
    req(results())
    res <- results()
    df_full <- res$full_data
    time_full <- df_full[[input$time_col]]
    actual_full <- res$full_target
    p <- ggplot(data = data.frame(Time = time_full, Actual = actual_full), 
                aes(x = Time, y = Actual)) +
      geom_line(color = "black") +
      labs(title = input$custom_title,
           x = input$custom_xlab, y = input$custom_ylab) +
      getChartTheme("Combined Forecast")
    
    all_algs <- input$algorithms
    sc <- seriesColors()
    for(mod in all_algs) {
      if(mod %in% c("ARIMA", "ETS", "NNETAR")) {
        fc_obj <- res$model_preds[[paste0(mod, "_fc")]]
        if(!is.null(fc_obj) && length(fc_obj$mean) >= input$future_periods) {
          h_future <- input$future_periods
          h_total <- length(fc_obj$mean)
          start_idx <- h_total - h_future + 1
          dt <- if(length(time_full) > 1) median(diff(as.numeric(time_full))) else 1
          fc_time <- seq(from = max(time_full) + dt, by = dt, length.out = h_future)
          df_fc <- data.frame(Time = fc_time,
                              Forecast = as.numeric(fc_obj$mean[start_idx:h_total]))
          if(!is.null(fc_obj$lower) && nrow(fc_obj$lower) >= h_future) {
            df_fc$Lower <- as.numeric(fc_obj$lower[start_idx:h_total, 1])
            df_fc$Upper <- as.numeric(fc_obj$upper[start_idx:h_total, 1])
          }
          df_fc <- df_fc[complete.cases(df_fc), ]
          if(nrow(df_fc) > 0) {
            colChoice <- if(!is.null(sc[mod])) sc[mod] else "gray"
            p <- p + geom_line(data = df_fc, aes(x = Time, y = Forecast),
                               color = colChoice, linetype = "dotted")
            if("Lower" %in% names(df_fc) && "Upper" %in% names(df_fc)) {
              p <- p + geom_ribbon(data = df_fc, 
                                   aes(x = Time, ymin = Lower, ymax = Upper),
                                   fill = colChoice, alpha = 0.2, inherit.aes = FALSE)
            }
          }
        }
      } else {
        fc_future <- res$model_preds[[paste0(mod, "_future")]]
        if(!is.null(fc_future) && length(fc_future) >= input$future_periods) {
          h_future <- input$future_periods
          dt <- if(length(time_full) > 1) median(diff(as.numeric(time_full))) else 1
          fc_time <- seq(from = max(time_full) + dt, by = dt, length.out = h_future)
          df_fc <- data.frame(Time = fc_time, Forecast = as.numeric(fc_future[1:h_future]))
          df_fc <- df_fc[complete.cases(df_fc), ]
          if(nrow(df_fc) > 0) {
            colChoice <- if(!is.null(sc[mod])) sc[mod] else "gray"
            p <- p + geom_line(data = df_fc, aes(x = Time, y = Forecast),
                               color = colChoice, linetype = "dotted")
          }
        }
      }
    }
    p + geom_vline(xintercept = max(time_full), linetype = "dotted", color = "red")
  })
  
  output$combinedForecastPlot <- renderPlotly({
    req(combinedForecastPlotObject())
    ggplotly(combinedForecastPlotObject())
  })
  
  # --- Additional Plots and Outputs ---
  output$forecastPlot <- renderPlotly({
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
    
    plot_df_long <- melt(plot_df, id.vars = "Time", variable.name = "Model", value.name = "Prediction")
    p <- ggplot(plot_df_long, aes(x = Time, y = Prediction, color = Model)) +
      geom_line() +
      geom_line(data = plot_df, aes(x = Time, y = Actual), color = "black", size = 1.2, linetype = "dashed") +
      labs(title = "Test Set Forecasts", y = "Target Value") +
      getChartTheme("Forecast Plot")
    ggplotly(p)
  })
  
  output$residualPlot <- renderPlotly({
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
    
    resid_long <- melt(resid_df, id.vars = "Time", variable.name = "Model", value.name = "Residual")
    p <- ggplot(resid_long, aes(x = Time, y = Residual, color = Model)) +
      geom_line() +
      labs(title = "Overall Residuals (Test Set)", y = "Residual") +
      getChartTheme("Residual Plot")
    ggplotly(p)
  })
  
  output$errorMetrics <- renderDataTable({
    req(results())
    res <- results()
    DT::datatable(res$error_metrics, options = list(pageLength = 5))
  })
  
  output$comparisonPlot <- renderPlotly({
    req(results())
    res <- results()
    p <- ggplot(res$error_metrics, aes(x = Model, y = RMSE, fill = Model)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Algorithm Comparison (RMSE)", y = "RMSE") +
      getChartTheme("Forecast Plot")
    ggplotly(p)
  })
  
  # --- Individual Combined Plots for each algorithm ---
  plotCombined <- function(time_vals, actual, forecast, algName) {
    if(is.null(forecast) || all(is.na(forecast))) return(NULL)
    residual <- actual - forecast
    if(all(is.na(residual))) return(NULL)
    p <- ggplot() +
      geom_line(aes(x = time_vals, y = actual), color = "black", size = 1.2) +
      geom_line(aes(x = time_vals, y = forecast), color = "blue", linetype = "dashed") +
      geom_line(aes(x = time_vals, y = residual), color = "red", linetype = "dotted") +
      labs(title = paste(algName, "Combined Results"), x = "Time", y = "Value") +
      theme_minimal() + getChartTheme(algName)
    ggplotly(p)
  }
  
  output$combined_ARIMA <- renderPlotly({
    req(results())
    res <- results()
    fc <- res$model_preds[["ARIMA_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "ARIMA")
    }
  })
  
  output$combined_ETS <- renderPlotly({
    req(results())
    res <- results()
    fc <- res$model_preds[["ETS_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "ETS")
    }
  })
  
  output$combined_NNETAR <- renderPlotly({
    req(results())
    res <- results()
    fc <- res$model_preds[["NNETAR_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "NNETAR")
    }
  })
  
  output$combined_VAR <- renderPlotly({
    req(results())
    res <- results()
    fc <- res$model_preds[["VAR_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "VAR")
    }
  })
  
  output$combined_SVAR <- renderPlotly({
    req(results())
    res <- results()
    fc <- res$model_preds[["SVAR_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "SVAR")
    }
  })
  
  output$combined_RF <- renderPlotly({
    req(results())
    res <- results()
    fc <- res$model_preds[["RF_test"]]
    if(!is.null(fc)){
      time_vals <- res$test_df[[input$time_col]]
      actual <- res$target_actual_test
      plotCombined(time_vals, actual, fc, "Random Forest")
    }
  })
  
  # --- Regression Outputs ---
  # Regression summaries: for each regression algorithm, output its summary in a box.
  output$regressionSummary <- renderUI({
    req(results()$regression)
    reg_list <- results()$regression
    summaries <- lapply(names(reg_list), function(algo) {
      mod <- reg_list[[algo]]$model
      txt <- if(inherits(mod, "cv.glmnet")) {
        paste(capture.output(print(mod)), collapse = "\n")
      } else {
        paste(capture.output(summary(mod)), collapse = "\n")
      }
      box(title = algo, width = 12, collapsible = TRUE, solidHeader = TRUE,
          pre(txt))
    })
    do.call(tagList, summaries)
  })
  
  # Regression plots: create a tabsetPanel where each tab shows the Actual vs Predicted plot for one regression algorithm.
  output$regressionPlot <- renderUI({
    req(results()$regression)
    reg_list <- results()$regression
    tabs <- lapply(names(reg_list), function(algo) {
      tabPanel(algo, plotlyOutput(outputId = paste0("reg_plot_", gsub(" ", "_", algo))))
    })
    do.call(tabsetPanel, tabs)
  })
  
  observe({
    req(results()$regression)
    reg_list <- results()$regression
    for(algo in names(reg_list)) {
      local({
        a <- algo
        output[[paste0("reg_plot_", gsub(" ", "_", a))]] <- renderPlotly({
          df_plot <- reg_list[[a]]$data
          df_plot$Predicted <- reg_list[[a]]$predictions
          target_wrapped <- sprintf("`%s`", input$target_col)
          p <- ggplot(df_plot, aes_string(x = target_wrapped, y = "Predicted")) +
            geom_point() +
            geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
            labs(title = paste("Actual vs Predicted -", a), x = "Actual", y = "Predicted") +
            theme_minimal()
          ggplotly(p)
        })
      })
    }
  })
  
  # --- Best Model Details ---
  output$bestAlgorithm <- renderUI({
    req(results())
    res <- results()
    if(nrow(res$error_metrics) > 0 && any(!is.na(res$error_metrics$RMSE))) {
      bestIdx <- which.min(res$error_metrics$RMSE)
      bestModel <- res$error_metrics$Model[bestIdx]
      bestRmse <- round(res$error_metrics$RMSE[bestIdx], 2)
      bestMape <- round(res$error_metrics$MAPE[bestIdx], 2)
      detailText <- paste("Model:", bestModel,
                          "\nRMSE:", bestRmse,
                          "\nMAPE:", bestMape, "%")
      tags$div(class = "well", style = "font-size: 18px; padding: 15px; margin: 15px;",
               pre(detailText))
    } else {
      tags$div(class = "well", "No best model identified yet.")
    }
  })
  
  # --- Shift Focus to Combined Forecast Tab after Analysis ---
  observeEvent(input$runAnalysis, {
    isolate({
      updateTabItems(session, "tabs", "combined_forecast")
    })
  })
}