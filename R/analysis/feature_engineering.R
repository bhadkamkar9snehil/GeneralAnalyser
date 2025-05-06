# Feature Engineering Functions
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

#' Create time-based features
#' @param data Data frame containing timestamp column
#' @param timestamp_col Name of timestamp column
#' @return Data frame with additional time-based features
createTimeFeatures <- function(data, timestamp_col) {
    timestamps <- as.POSIXct(data[[timestamp_col]])
    
    # Extract basic time components
    result <- data.frame(
        Hour = lubridate::hour(timestamps),
        DayOfWeek = lubridate::wday(timestamps),
        DayOfMonth = lubridate::day(timestamps),
        Month = lubridate::month(timestamps),
        Quarter = lubridate::quarter(timestamps),
        Year = lubridate::year(timestamps),
        IsWeekend = lubridate::wday(timestamps) %in% c(1, 7),
        IsDayTime = lubridate::hour(timestamps) >= 6 & lubridate::hour(timestamps) <= 18
    )
    
    # Add holiday indicators if timeDate package is available
    if(requireNamespace("timeDate", quietly = TRUE)) {
        holidays <- timeDate::holiday(class(timestamps), timestamps)
        result$IsHoliday <- !is.na(holidays)
    }
    
    result
}

#' Create lagged features
#' @param series Numeric vector or time series
#' @param lags Vector of lag values
#' @return Data frame with lagged features
createLagFeatures <- function(series, lags = 1:3) {
    result <- sapply(lags, function(lag) {
        dplyr::lag(series, lag)
    })
    colnames(result) <- paste0("lag_", lags)
    as.data.frame(result)
}

#' Create rolling window statistics
#' @param series Numeric vector or time series
#' @param windows Vector of window sizes
#' @param functions List of functions to apply
#' @return Data frame with rolling statistics
createRollingFeatures <- function(series, windows = c(3, 7, 14, 30), 
                                functions = list(mean = mean, sd = sd, min = min, max = max)) {
    result <- data.frame(row.names = seq_along(series))
    
    for(window in windows) {
        for(fname in names(functions)) {
            fn <- functions[[fname]]
            col_name <- paste0(fname, "_", window)
            result[[col_name]] <- zoo::rollapply(
                series, 
                width = window, 
                FUN = function(x) fn(x, na.rm = TRUE),
                fill = NA,
                align = "right"
            )
        }
    }
    
    result
}

#' Create interaction features
#' @param data Data frame
#' @param feature_cols Columns to create interactions for
#' @param degree Maximum degree of interactions
#' @return Data frame with interaction features
createInteractionFeatures <- function(data, feature_cols, degree = 2) {
    if(length(feature_cols) < 2) return(data.frame())
    
    combinations <- utils::combn(feature_cols, degree, simplify = FALSE)
    result <- data.frame(row.names = seq_len(nrow(data)))
    
    for(cols in combinations) {
        col_name <- paste(cols, collapse = "_x_")
        result[[col_name]] <- Reduce(`*`, lapply(cols, function(col) data[[col]]))
    }
    
    result
}

#' Create polynomial features
#' @param data Data frame
#' @param feature_cols Columns to create polynomials for
#' @param degree Maximum polynomial degree
#' @return Data frame with polynomial features
createPolynomialFeatures <- function(data, feature_cols, degree = 2) {
    result <- data.frame(row.names = seq_len(nrow(data)))
    
    for(col in feature_cols) {
        for(d in 2:degree) {
            col_name <- paste0(col, "_pow", d)
            result[[col_name]] <- data[[col]]^d
        }
    }
    
    result
}

#' Create frequency domain features
#' @param series Numeric vector or time series
#' @return Data frame with frequency domain features
createFrequencyFeatures <- function(series) {
    # Perform FFT
    n <- length(series)
    fft_result <- fft(series)
    magnitude <- Mod(fft_result)[1:(n/2 + 1)]
    frequency <- seq(0, 1, length.out = length(magnitude))
    
    # Extract frequency domain features
    data.frame(
        DominantFreq = frequency[which.max(magnitude)],
        MaxMagnitude = max(magnitude),
        MeanMagnitude = mean(magnitude),
        MedianMagnitude = median(magnitude),
        FrequencySD = sd(magnitude)
    )
}

#' Create statistical features
#' @param series Numeric vector
#' @return Data frame with statistical features
createStatisticalFeatures <- function(series) {
    data.frame(
        Mean = mean(series, na.rm = TRUE),
        SD = sd(series, na.rm = TRUE),
        Median = median(series, na.rm = TRUE),
        IQR = IQR(series, na.rm = TRUE),
        Skewness = moments::skewness(series, na.rm = TRUE),
        Kurtosis = moments::kurtosis(series, na.rm = TRUE),
        RootMeanSquare = sqrt(mean(series^2, na.rm = TRUE))
    )
}

#' Detect and handle outliers
#' @param data Data frame
#' @param columns Columns to process
#' @param method Outlier detection method
#' @param treatment Treatment method for outliers
#' @return Data frame with treated outliers
handleOutliers <- function(data, columns, method = "iqr", treatment = "clip") {
    result <- data
    
    for(col in columns) {
        values <- data[[col]]
        
        # Detect outliers
        outliers <- switch(method,
            "iqr" = {
                q1 <- quantile(values, 0.25, na.rm = TRUE)
                q3 <- quantile(values, 0.75, na.rm = TRUE)
                iqr <- q3 - q1
                lower <- q1 - 1.5 * iqr
                upper <- q3 + 1.5 * iqr
                values < lower | values > upper
            },
            "zscore" = {
                z <- abs(scale(values))
                z > 3
            },
            stop("Unsupported outlier detection method")
        )
        
        # Treat outliers
        if(any(outliers, na.rm = TRUE)) {
            result[[col]] <- switch(treatment,
                "clip" = {
                    pmin(pmax(values, quantile(values, 0.01, na.rm = TRUE)),
                                   quantile(values, 0.99, na.rm = TRUE))
                },
                "median" = {
                    values[outliers] <- median(values[!outliers], na.rm = TRUE)
                    values
                },
                stop("Unsupported outlier treatment method")
            )
        }
    }
    
    result
}

#' Perform feature selection
#' @param data Data frame
#' @param target Target variable
#' @param feature_cols Feature columns
#' @param method Feature selection method
#' @param k Number of features to select
#' @return Selected feature names
selectFeatures <- function(data, target, feature_cols, method = "correlation", k = NULL) {
    if(is.null(k)) k <- length(feature_cols)
    
    selected <- switch(method,
        "correlation" = {
            cors <- sapply(feature_cols, function(col) {
                cor(data[[col]], data[[target]], use = "complete.obs")
            })
            names(sort(abs(cors), decreasing = TRUE)[1:k])
        },
        "variance" = {
            vars <- sapply(feature_cols, function(col) {
                var(data[[col]], na.rm = TRUE)
            })
            names(sort(vars, decreasing = TRUE)[1:k])
        },
        "lasso" = {
            x <- as.matrix(data[, feature_cols])
            y <- data[[target]]
            cv_fit <- glmnet::cv.glmnet(x, y, alpha = 1)
            coef_fit <- coef(cv_fit, s = "lambda.1se")
            selected_idx <- which(abs(coef_fit[-1]) > 0)
            feature_cols[selected_idx]
        },
        stop("Unsupported feature selection method")
    )
    
    selected
}