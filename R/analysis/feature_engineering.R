#' Feature engineering functions
#' @export

create_lags <- function(series, n_lags) {
    lags <- lapply(1:n_lags, function(i) dplyr::lag(series, i))
    names(lags) <- paste0("lag", 1:n_lags)
    as.data.frame(lags)
}

create_rolling_stats <- function(series, window_size) {
    data.frame(
        rolling_mean = zoo::rollmean(series, k = window_size, fill = NA, align = "right"),
        rolling_sd = zoo::rollapply(series, width = window_size, FUN = sd, fill = NA, align = "right"),
        rolling_min = zoo::rollmin(series, k = window_size, fill = NA, align = "right"),
        rolling_max = zoo::rollmax(series, k = window_size, fill = NA, align = "right")
    )
}