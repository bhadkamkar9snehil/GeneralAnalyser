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