#' Time Series Analysis Functions
#' 
#' This module contains functions for time series analysis including
#' forecasting, decomposition, and seasonal adjustments.

#' @import forecast
#' @import stats
#' @export
NULL

#' Perform time series forecast
#' @param data Time series data
#' @param horizon Forecast horizon
#' @param method Forecasting method (auto.arima, ets, etc.)
#' @return Forecast object
#' @export
ts_forecast <- function(data, horizon = 12, method = "auto.arima") {
    ts_obj <- ts(data)
    
    model <- switch(method,
        auto.arima = forecast::auto.arima(ts_obj),
        ets = forecast::ets(ts_obj),
        nnetar = forecast::nnetar(ts_obj),
        stop("Unsupported forecasting method")
    )
    
    forecast::forecast(model, h = horizon)
}

#' Decompose time series
#' @param data Time series data
#' @param type Decomposition type (additive or multiplicative)
#' @return Decomposition object
#' @export
ts_decompose <- function(data, type = "additive") {
    ts_obj <- ts(data)
    decompose(ts_obj, type = type)
}

#' Check for seasonality
#' @param data Time series data
#' @return List containing seasonality tests
#' @export
check_seasonality <- function(data) {
    ts_obj <- ts(data)
    list(
        seasonal = forecast::findfrequency(ts_obj),
        acf = stats::acf(ts_obj, plot = FALSE)
    )
}

#' Perform time series forecasting with multiple algorithms
#' @param data Data frame containing time series data
#' @param time_col Time column name
#' @param target_col Target column name
#' @param algorithms Vector of algorithm names
#' @param split_ratio Training/test split ratio
#' @return List containing forecasts and metrics
#' @export
performTimeSeriesAnalysis <- function(data, time_col, target_col, multivar_cols = NULL,
                                    algorithms, split_ratio = 0.8, future_periods = 10) {
    # Data preparation
    df <- data[order(data[[time_col]]), ]
    target <- as.numeric(df[[target_col]])
    n <- length(target)
    train_size <- floor(split_ratio * n)
    
    train_target <- target[1:train_size]
    test_target <- target[(train_size + 1):n]
    
    model_preds <- list()
    error_metrics <- data.frame(
        Model = character(),
        RMSE = numeric(),
        MAPE = numeric(),
        stringsAsFactors = FALSE
    )
    
    # ARIMA
    if("ARIMA" %in% algorithms) {
        arima_model <- forecast::auto.arima(train_target)
        fc_test <- forecast::forecast(arima_model, h = length(test_target))
        preds_test <- as.numeric(fc_test$mean)
        fc_future <- forecast::forecast(arima_model, h = future_periods)
        
        model_preds[["ARIMA_test"]] <- preds_test
        model_preds[["ARIMA_future"]] <- as.numeric(fc_future$mean)
        model_preds[["ARIMA_fc"]] <- fc_test
        
        error_metrics <- rbind(error_metrics, data.frame(
            Model = "ARIMA",
            RMSE = Metrics::rmse(test_target, preds_test),
            MAPE = Metrics::mape(test_target, preds_test) * 100
        ))
    }
    
    # ETS
    if("ETS" %in% algorithms) {
        ets_model <- forecast::ets(train_target)
        fc_test <- forecast::forecast(ets_model, h = length(test_target))
        preds_test <- as.numeric(fc_test$mean)
        fc_future <- forecast::forecast(ets_model, h = future_periods)
        
        model_preds[["ETS_test"]] <- preds_test
        model_preds[["ETS_future"]] <- as.numeric(fc_future$mean)
        model_preds[["ETS_fc"]] <- fc_test
        
        error_metrics <- rbind(error_metrics, data.frame(
            Model = "ETS",
            RMSE = Metrics::rmse(test_target, preds_test),
            MAPE = Metrics::mape(test_target, preds_test) * 100
        ))
    }
    
    # NNETAR
    if("NNETAR" %in% algorithms) {
        nnet_model <- forecast::nnetar(train_target)
        fc_test <- forecast::forecast(nnet_model, h = length(test_target))
        preds_test <- as.numeric(fc_test$mean)
        fc_future <- forecast::forecast(nnet_model, h = future_periods)
        
        model_preds[["NNETAR_test"]] <- preds_test
        model_preds[["NNETAR_future"]] <- as.numeric(fc_future$mean)
        model_preds[["NNETAR_fc"]] <- fc_test
        
        error_metrics <- rbind(error_metrics, data.frame(
            Model = "NNETAR",
            RMSE = Metrics::rmse(test_target, preds_test),
            MAPE = Metrics::mape(test_target, preds_test) * 100
        ))
    }
    
    # VAR/SVAR for multivariate analysis
    if(any(c("VAR", "SVAR") %in% algorithms) && length(multivar_cols) > 0) {
        mv_cols <- c(target_col, multivar_cols)
        mv_data <- df[, mv_cols, drop = FALSE]
        mv_data <- as.data.frame(lapply(mv_data, as.numeric))
        train_mv <- mv_data[1:train_size, ]
        test_mv <- mv_data[(train_size + 1):n, ]
        
        if("VAR" %in% algorithms) {
            var_model <- vars::VAR(train_mv, p = 1, type = "const")
            fc_test <- predict(var_model, n.ahead = nrow(test_mv))
            preds_test <- fc_test$fcst[[target_col]][, "fcst"]
            fc_future <- predict(var_model, n.ahead = future_periods)
            future_preds <- fc_future$fcst[[target_col]][, "fcst"]
            
            model_preds[["VAR_test"]] <- preds_test
            model_preds[["VAR_future"]] <- future_preds
            
            error_metrics <- rbind(error_metrics, data.frame(
                Model = "VAR",
                RMSE = Metrics::rmse(test_mv[[target_col]], preds_test),
                MAPE = Metrics::mape(test_mv[[target_col]], preds_test) * 100
            ))
        }
        
        if("SVAR" %in% algorithms) {
            tryCatch({
                var_model <- vars::VAR(train_mv, p = 1, type = "const")
                svar_model <- svars::svar(var_model, estmethod = "direct")
                fc_test <- predict(svar_model, n.ahead = nrow(test_mv))
                preds_test <- fc_test$fcst[[target_col]][, "fcst"]
                fc_future <- predict(svar_model, n.ahead = future_periods)
                future_preds <- fc_future$fcst[[target_col]][, "fcst"]
                
                model_preds[["SVAR_test"]] <- preds_test
                model_preds[["SVAR_future"]] <- future_preds
                
                error_metrics <- rbind(error_metrics, data.frame(
                    Model = "SVAR",
                    RMSE = Metrics::rmse(test_mv[[target_col]], preds_test),
                    MAPE = Metrics::mape(test_mv[[target_col]], preds_test) * 100
                ))
            }, error = function(e) {
                warning("SVAR model failed to converge")
            })
        }
    }
    
    # Random Forest
    if("Random Forest" %in% algorithms) {
        # Create lagged features
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
        
        rf_model <- randomForest::randomForest(Target ~ ., data = rf_train)
        preds_test <- predict(rf_model, newdata = rf_test)
        
        # Generate future predictions
        rf_future <- numeric(future_periods)
        last_values <- tail(full_series, max_lag)
        for(i in 1:future_periods) {
            new_data <- as.data.frame(t(last_values[1:max_lag]))
            colnames(new_data) <- paste0("lag", 1:max_lag)
            pred <- predict(rf_model, newdata = new_data)
            rf_future[i] <- pred
            last_values <- c(pred, last_values[1:(max_lag - 1)])
        }
        
        model_preds[["RF_test"]] <- preds_test
        model_preds[["RF_future"]] <- rf_future
        
        error_metrics <- rbind(error_metrics, data.frame(
            Model = "Random Forest",
            RMSE = Metrics::rmse(rf_test$Target, preds_test),
            MAPE = Metrics::mape(rf_test$Target, preds_test) * 100
        ))
    }
    
    list(
        train_df = df[1:train_size, ],
        test_df = df[(train_size + 1):n, ],
        target_actual_test = test_target,
        model_preds = model_preds,
        error_metrics = error_metrics
    )
}

#' Plot combined forecast results
#' @param results Results from performTimeSeriesAnalysis
#' @return plotly object
plotCombinedForecast <- function(results) {
    df <- results$test_df
    time_vals <- df[[1]]  # Assuming first column is time
    actual <- results$target_actual_test
    
    plot_df <- data.frame(Time = time_vals, Actual = actual)
    
    # Add predictions for each model
    for(model in names(results$model_preds)) {
        if(endsWith(model, "_test")) {
            model_name <- sub("_test$", "", model)
            plot_df[[model_name]] <- results$model_preds[[model]]
        }
    }
    
    p <- plot_ly(data = plot_df, x = ~Time) %>%
        add_lines(y = ~Actual, name = "Actual", line = list(color = "black", width = 2))
    
    colors <- RColorBrewer::brewer.pal(length(names(results$model_preds)) / 2, "Set2")
    i <- 1
    
    for(model in unique(sub("_test$", "", names(results$model_preds)))) {
        if(model %in% colnames(plot_df)) {
            p <- p %>% add_lines(y = as.formula(paste0("~", model)),
                               name = model,
                               line = list(color = colors[i], dash = "dash"))
            i <- i + 1
        }
    }
    
    p %>% layout(title = "Forecast Comparison",
                xaxis = list(title = "Time"),
                yaxis = list(title = "Value"))
}

#' Plot residuals for all models
#' @param results Results from performTimeSeriesAnalysis
#' @return plotly object
plotResiduals <- function(results) {
    df <- results$test_df
    time_vals <- df[[1]]  # Assuming first column is time
    actual <- results$target_actual_test
    
    resid_df <- data.frame(Time = time_vals)
    
    for(model in names(results$model_preds)) {
        if(endsWith(model, "_test")) {
            model_name <- sub("_test$", "", model)
            resid_df[[model_name]] <- actual - results$model_preds[[model]]
        }
    }
    
    plot_ly(data = resid_df, x = ~Time) %>%
        add_traces(y = as.formula(paste0("~", names(resid_df)[2:ncol(resid_df)])),
                  type = "scatter", mode = "lines") %>%
        layout(title = "Model Residuals",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Residual"))
}