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
library(DT)
library(readxl)
library(dplyr)

#' Main Server Logic for GeneralAnalyser
#' 
#' This file contains the core server-side functionality for the GeneralAnalyser application.
#' It handles data loading, analysis, visualization, and model management.
#'
#' Features:
#' - Time series forecasting with multiple algorithms (ARIMA, ETS, NNETAR, VAR, SVAR)
#' - Regression analysis with various models
#' - Interactive visualizations using plotly
#' - Automated model selection and evaluation
#' - Support for CSV and Excel file formats
#'
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import forecast
#' @importFrom stats ts na.omit cor as.formula predict
#' @importFrom utils tail capture.output
#' @importFrom rlang .data
NULL

# Import functions from other modules
# Use normalizePath to ensure correct path resolution
analysis_dir <- normalizePath(file.path(".", "analysis"))
source(file.path(analysis_dir, "time_series.R"))
source(file.path(analysis_dir, "visualization.R"))
source(file.path(analysis_dir, "anomaly_detection.R"))
source(file.path(analysis_dir, "regression.R"))
source(file.path(analysis_dir, "feature_engineering.R"))
source(file.path(analysis_dir, "model_selection.R"))
source(file.path(analysis_dir, "metrics.R"))
source(file.path(analysis_dir, "data_preprocessing.R"))

# Helper Functions

#' Load data from uploaded file
#' @param file The uploaded file object
#' @return Dataframe containing the loaded data
load_file_data <- function(file) {
    ext <- tools::file_ext(file$datapath)
    
    if (ext == "csv") {
        data <- read.csv(file$datapath, header = TRUE, stringsAsFactors = FALSE)
    } else if (ext %in% c("xls", "xlsx")) {
        data <- readxl::read_excel(file$datapath)
    } else {
        stop("Unsupported file format. Please upload a CSV or Excel file.")
    }
    
    return(data)
}

server <- function(input, output, session) {
    # Reactive values for storing state
    values <- reactiveValues(
        data = NULL,
        model = NULL,
        results = NULL
    )
    
    # Data Input Handler
    observeEvent(input$file, {
        req(input$file)
        tryCatch({
            values$data <- load_file_data(input$file)
            
            # Update column selection dropdowns
            updateSelectInput(session, "time_col", 
                choices = names(values$data))
            updateSelectInput(session, "target_col", 
                choices = names(values$data))
            updateSelectizeInput(session, "multivar_cols", 
                choices = names(values$data))
            
            # Update regressor selection if regression is enabled
            if (input$runRegression) {
                updateSelectizeInput(session, "regressors",
                    choices = names(values$data))
            }
            
            showNotification("Data loaded successfully", type = "message")
        }, error = function(e) {
            showNotification(paste("Error loading data:", e$message), type = "error")
        })
    })
    
    # Time Series Analysis Handler
    observeEvent(input$run_ts_analysis, {
        req(values$data, input$ts_method, input$horizon)
        tryCatch({
            ts_data <- ts(values$data[[input$target_col]])
            values$results <- GeneralAnalyser::ts_forecast(ts_data, input$horizon, input$ts_method)
            
            output$ts_plot <- renderPlot({
                plot(values$results) +
                    theme_minimal() +
                    labs(title = "Time Series Forecast")
            })
        }, error = function(e) {
            showNotification(paste("Error in analysis:", e$message), type = "error")
        })
    })
    
    # Data Preview
    output$data_preview <- DT::renderDataTable({
        req(values$data)
        DT::datatable(values$data,
            options = list(pageLength = 10, scrollX = TRUE)
        )
    })
    
    # Model Summary
    output$model_summary <- renderPrint({
        req(values$results)
        summary(values$results)
    })
    
    # Download Handler
    output$download_results <- downloadHandler(
        filename = function() {
            paste("analysis-results-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(values$results$mean, file, row.names = FALSE)
        }
    )

    # Trend Plot 
    output$trendPlot <- renderPlotly({
        req(values$data, input$time_col)
        df <- values$data
        df_long <- reshape2::melt(df[sapply(df, is.numeric)], 
                                id.vars = input$time_col)
        
        p <- ggplot(df_long, aes_string(x = input$time_col, y = "value")) +
            geom_line(aes_string(color = "variable")) +
            facet_wrap(~.data$variable, scales = "free_y") +
            labs(title = "Trends for Numeric Variables",
                 x = "Time",
                 y = "Value") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(p)
    })

    # Correlation Heatmap
    output$corPlot <- renderPlotly({
        req(values$data)
        df <- values$data[sapply(values$data, is.numeric)]
        if(ncol(df) < 2) return(NULL)
        
        cor_mat <- cor(df, use = "complete.obs")
        
        corColors <- switch(input$cor_color_scheme,
            "Blue-Red" = list(low = "blue", high = "red"),
            "Purple-Green" = list(low = "purple", high = "green"),
            "Orange-Blue" = list(low = "orange", high = "blue"),
            list(low = "blue", high = "red"))
        
        plot_ly(
            x = colnames(cor_mat),
            y = rownames(cor_mat),
            z = cor_mat,
            type = "heatmap",
            colors = colorRamp(c(corColors$low, "white", corColors$high))
        ) %>% layout(title = "Correlation Heatmap")
    })

    # Model Results Handler
    output$modelResults <- renderPlot({
        req(values$results)
        autoplot(values$results) +
            theme_minimal() +
            labs(title = "Model Results",
                 x = "Time",
                 y = "Value")
    })

    # Best Model Selection
    output$bestModel <- renderUI({
        req(values$results)
        if(is.null(values$results$error_metrics)) return(NULL)
        
        metrics <- values$results$error_metrics
        best_idx <- which.min(metrics$RMSE)
        best_model <- metrics$Model[best_idx]
        best_rmse <- round(metrics$RMSE[best_idx], 2)
        best_mape <- round(metrics$MAPE[best_idx], 2)
        
        div(
            h4("Best Performing Model"),
            tags$ul(
                tags$li(sprintf("Model: %s", best_model)),
                tags$li(sprintf("RMSE: %.2f", best_rmse)),
                tags$li(sprintf("MAPE: %.2f%%", best_mape))
            )
        )
    })

    # Anomaly Detection Plot
    output$anomalyPlot <- renderPlotly({
        req(values$data, input$target_col)
        
        data <- values$data[[input$target_col]]
        ts_data <- ts(data)
        result <- GeneralAnalyser::detect_anomalies(ts_data, method = "zscore", threshold = 0.95)
        
        df_plot <- data.frame(
            Time = seq_along(data),
            Value = data,
            IsAnomaly = result$anomalies
        )
        
        plot_ly() %>%
            add_lines(data = df_plot, x = ~Time, y = ~Value, name = "Data") %>%
            add_markers(data = subset(df_plot, .data$IsAnomaly), 
                       x = ~Time, y = ~Value,
                       marker = list(color = "red", size = 8),
                       name = "Anomalies") %>%
            layout(title = "Anomaly Detection Results",
                   xaxis = list(title = "Time"),
                   yaxis = list(title = "Value"))
    })

    # Forecasting Analysis
    forecast_analysis <- eventReactive(input$runAnalysis, {
        req(values$data, input$time_col, input$target_col)
        
        withProgress(message = 'Running Analysis', value = 0, {
            # Data preparation
            df <- values$data
            df[[input$time_col]] <- as.POSIXct(df[[input$time_col]])
            if(any(is.na(df[[input$time_col]]))) {
                stop("Time column could not be parsed correctly.")
            }
            df <- df[order(df[[input$time_col]]), ]
            
            # Split data
            n <- nrow(df)
            train_size <- floor(input$split_ratio * n)
            train_df <- df[1:train_size, ]
            test_df <- df[(train_size + 1):n, ]
            
            model_preds <- list()
            error_metrics <- data.frame(Model = character(),
                                      RMSE = numeric(),
                                      MAPE = numeric(),
                                      stringsAsFactors = FALSE)
            
            # Run selected algorithms
            for(algo in input$algorithms) {
                incProgress(1/length(input$algorithms),
                          detail = paste("Running", algo))
                
                tryCatch({
                    result <- switch(algo,
                        "ARIMA" = fit_arima(train_df, test_df, input$target_col),
                        "ETS" = fit_ets(train_df, test_df, input$target_col),
                        "NNETAR" = fit_nnetar(train_df, test_df, input$target_col),
                        "VAR" = fit_var(train_df, test_df, input$target_col, input$multivar_cols),
                        "SVAR" = fit_svar(train_df, test_df, input$target_col, input$multivar_cols),
                        "Random Forest" = fit_rf(train_df, test_df, input$target_col)
                    )
                    
                    model_preds[[paste0(algo, "_test")]] <- result$predictions
                    error_metrics <- rbind(error_metrics,
                                         data.frame(Model = algo,
                                                  RMSE = result$rmse,
                                                  MAPE = result$mape))
                }, error = function(e) {
                    showNotification(paste("Error in", algo, ":", e$message),
                                   type = "warning")
                })
            }
            
            list(
                train_df = train_df,
                test_df = test_df,
                model_preds = model_preds,
                error_metrics = error_metrics
            )
        })
    })

    # Model Fitting Helper Functions
    fit_arima <- function(train, test, target_col) {
        ts_data <- ts(train[[target_col]])
        model <- auto.arima(ts_data)
        preds <- forecast(model, h = nrow(test))$mean
        
        list(
            predictions = preds,
            rmse = rmse(test[[target_col]], preds),
            mape = mape(test[[target_col]], preds)
        )
    }

    fit_ets <- function(train, test, target_col) {
        ts_data <- ts(train[[target_col]])
        model <- ets(ts_data)
        preds <- forecast(model, h = nrow(test))$mean
        
        list(
            predictions = preds,
            rmse = rmse(test[[target_col]], preds),
            mape = mape(test[[target_col]], preds)
        )
    }

    fit_nnetar <- function(train, test, target_col) {
        ts_data <- ts(train[[target_col]])
        model <- nnetar(ts_data)
        preds <- forecast(model, h = nrow(test))$mean
        
        list(
            predictions = preds,
            rmse = rmse(test[[target_col]], preds),
            mape = mape(test[[target_col]], preds)
        )
    }

    # VAR/SVAR Model Fitting
    fit_var <- function(train, test, target_col, multivar_cols) {
        if(length(multivar_cols) < 1) {
            stop("VAR requires additional variables")
        }
        
        mv_data <- train[, c(target_col, multivar_cols)]
        mv_data <- as.data.frame(lapply(mv_data, function(x) as.numeric(as.character(x))))
        mv_data <- na.omit(mv_data)
        
        model <- VAR(mv_data, p = 1, type = "const")
        future <- predict(model, n.ahead = nrow(test))
        preds <- future$fcst[[target_col]][, "fcst"]
        
        list(
            predictions = preds,
            rmse = rmse(test[[target_col]], preds),
            mape = mape(test[[target_col]], preds)
        )
    }

    fit_svar <- function(train, test, target_col, multivar_cols) {
        if(length(multivar_cols) < 1) {
            stop("SVAR requires additional variables")
        }
        
        mv_data <- train[, c(target_col, multivar_cols)]
        mv_data <- as.data.frame(lapply(mv_data, function(x) as.numeric(as.character(x))))
        mv_data <- na.omit(mv_data)
        
        var_model <- VAR(mv_data, p = 1, type = "const")
        svar_model <- svars::svar(var_model, estmethod = "direct")
        future <- predict(svar_model, n.ahead = nrow(test))
        preds <- future$fcst[[target_col]][, "fcst"]
        
        list(
            predictions = preds,
            rmse = rmse(test[[target_col]], preds),
            mape = mape(test[[target_col]], preds)
        )
    }

    # Add Random Forest implementation
    fit_rf <- function(train, test, target_col) {
        max_lag <- 3
        create_lagged_df <- function(series, max_lag) {
            df_lag <- data.frame(Target = series)
            for(i in 1:max_lag) {
                df_lag[[paste0("lag", i)]] <- dplyr::lag(series, n = i)
            }
            df_lag
        }
        
        # Create lagged features for training
        train_lagged <- create_lagged_df(train[[target_col]], max_lag)
        train_lagged <- na.omit(train_lagged)
        
        # Train RF model
        rf_model <- randomForest(Target ~ ., data = train_lagged,
                               ntree = 500, importance = TRUE)
        
        # Create lagged features for test set
        last_values <- tail(train[[target_col]], max_lag)
        test_preds <- numeric(nrow(test))
        
        for(i in seq_along(test_preds)) {
            new_data <- data.frame(matrix(rev(last_values), nrow = 1))
            colnames(new_data) <- paste0("lag", 1:max_lag)
            test_preds[i] <- predict(rf_model, newdata = new_data)
            last_values <- c(test_preds[i], last_values[-length(last_values)])
        }
        
        list(
            predictions = test_preds,
            rmse = rmse(test[[target_col]], test_preds),
            mape = mape(test[[target_col]], test_preds)
        )
    }

    # Combined Forecast Plot
    output$combinedForecastPlot <- renderPlotly({
        req(forecast_analysis())
        res <- forecast_analysis()
        
        plot_df <- data.frame(
            Time = res$test_df[[input$time_col]],
            Actual = res$test_df[[input$target_col]]
        )
        
        for(model in names(res$model_preds)) {
            plot_df[[model]] <- res$model_preds[[model]]
        }
        
        plot_df_long <- reshape2::melt(plot_df, 
                                      id.vars = "Time",
                                      variable.name = "Model",
                                      value.name = "Value")
        
        p <- ggplot(plot_df_long, aes_string(x = "Time", y = "Value", color = "Model")) +
            geom_line() +
            labs(title = "Combined Forecast Results") +
            theme_minimal()
        
        ggplotly(p)
    })

    # Regression Analysis
    output$regressionSummary <- renderUI({
        req(forecast_analysis()$regression)
        reg_list <- forecast_analysis()$regression
        
        summaries <- lapply(names(reg_list), function(algo) {
            mod <- reg_list[[algo]]$model
            txt <- if(inherits(mod, "cv.glmnet")) {
                paste(capture.output(print(mod)), collapse = "\n")
            } else {
                paste(capture.output(summary(mod)), collapse = "\n")
            }
            
            box(
                title = algo,
                width = 12,
                collapsible = TRUE,
                solidHeader = TRUE,
                pre(txt)
            )
        })
        
        do.call(tagList, summaries)
    })

    # Regression Plots
    output$regressionPlot <- renderUI({
        req(forecast_analysis()$regression)
        reg_list <- forecast_analysis()$regression
        
        tabs <- lapply(names(reg_list), function(algo) {
            tabPanel(
                algo,
                plotlyOutput(outputId = paste0("reg_plot_", gsub(" ", "_", algo)))
            )
        })
        
        do.call(tabsetPanel, tabs)
    })

    # Observe regression plots
    observe({
        req(forecast_analysis()$regression)
        reg_list <- forecast_analysis()$regression
        
        for(algo in names(reg_list)) {
            local({
                a <- algo
                output[[paste0("reg_plot_", gsub(" ", "_", a))]] <- renderPlotly({
                    df_plot <- reg_list[[a]]$data
                    df_plot$Predicted <- reg_list[[a]]$predictions
                    
                    p <- ggplot(df_plot, aes_string(x = input$target_col, y = "Predicted")) +
                        geom_point() +
                        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
                        labs(title = paste("Actual vs Predicted -", a),
                             x = "Actual",
                             y = "Predicted") +
                        theme_minimal()
                    
                    ggplotly(p)
                })
            })
        }
    })
}