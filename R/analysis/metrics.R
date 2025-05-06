#' Model evaluation metrics
#' @export

calculate_metrics <- function(actual, predicted) {
    data.frame(
        RMSE = Metrics::rmse(actual, predicted),
        MAE = Metrics::mae(actual, predicted),
        MAPE = Metrics::mape(actual, predicted) * 100,
        R2 = cor(actual, predicted)^2
    )
}

calculate_forecast_accuracy <- function(forecast_obj) {
    accuracy_metrics <- forecast::accuracy(forecast_obj)
    as.data.frame(accuracy_metrics)
}