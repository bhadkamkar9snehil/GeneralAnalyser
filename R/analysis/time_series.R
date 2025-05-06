#' Time Series Analysis Module
#' Basic time series analysis and forecasting functionality

#' Create time series object from data
#' @param data Numeric vector of observations
#' @param frequency Frequency of the time series (default: 12 for monthly)
#' @return ts object
create_ts <- function(data, frequency = 12) {
  ts(data, frequency = frequency)
}

#' Fit ARIMA model to time series data
#' @param ts_data Time series object
#' @return ARIMA model object
fit_arima <- function(ts_data) {
  tryCatch({
    forecast::auto.arima(ts_data)
  }, error = function(e) {
    warning("Error fitting ARIMA model: ", e$message)
    NULL
  })
}

#' Generate forecasts from a fitted model
#' @param model Fitted time series model
#' @param h Number of periods to forecast
#' @return Forecast object
generate_forecast <- function(model, h = 12) {
  if(is.null(model)) {
    stop("No valid model provided")
  }
  
  tryCatch({
    forecast::forecast(model, h = h)
  }, error = function(e) {
    warning("Error generating forecast: ", e$message)
    NULL
  })
}

#' Calculate basic time series metrics
#' @param actual Actual values
#' @param predicted Predicted values
#' @return List of accuracy metrics
calculate_metrics <- function(actual, predicted) {
  if(length(actual) != length(predicted)) {
    stop("Length of actual and predicted values must match")
  }
  
  # Calculate basic metrics
  mae <- mean(abs(actual - predicted))
  rmse <- sqrt(mean((actual - predicted)^2))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  
  list(
    MAE = mae,
    RMSE = rmse,
    MAPE = mape
  )
}