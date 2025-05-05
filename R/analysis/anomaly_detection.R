#' Anomaly Detection Functions
#' 
#' This module contains functions for detecting anomalies using
#' various statistical and machine learning methods.

#' @import anomalize
#' @import forecast
NULL

#' Detect anomalies in time series data
#' @param data Time series data
#' @param method Detection method (iqr, zscore, isolation_forest)
#' @param threshold Threshold for anomaly detection
#' @return List containing anomaly indicators and scores
detect_anomalies <- function(data, method = "iqr", threshold = 0.95) {
    if (is.data.frame(data)) {
        data <- as.numeric(data[[1]])
    }
    
    result <- switch(method,
        iqr = {
            q1 <- quantile(data, 0.25)
            q3 <- quantile(data, 0.75)
            iqr <- q3 - q1
            lower <- q1 - 1.5 * iqr
            upper <- q3 + 1.5 * iqr
            list(
                anomalies = data < lower | data > upper,
                scores = abs(scale(data)),
                bounds = c(lower = lower, upper = upper)
            )
        },
        zscore = {
            scores <- abs(scale(data))
            list(
                anomalies = scores > qnorm(threshold),
                scores = scores
            )
        },
        isolation_forest = {
            if (!requireNamespace("isotree", quietly = TRUE)) {
                stop("Package 'isotree' needed for isolation forest method")
            }
            model <- isotree::isolation.forest(matrix(data, ncol = 1))
            scores <- predict(model, matrix(data, ncol = 1))
            list(
                anomalies = scores > threshold,
                scores = scores
            )
        },
        stop("Unsupported anomaly detection method")
    )
    
    return(result)
}

#' Detect seasonal anomalies
#' @param data Time series data
#' @param frequency Seasonal frequency
#' @return List containing seasonal anomalies
detect_seasonal_anomalies <- function(data, frequency = NULL) {
    if (is.null(frequency)) {
        frequency <- forecast::findfrequency(data)
    }
    
    # Decompose the time series
    decomp <- decompose(ts(data, frequency = frequency))
    
    # Detect anomalies in the seasonal component
    seasonal_anomalies <- detect_anomalies(decomp$seasonal, method = "zscore")
    
    # Detect anomalies in the remainder
    remainder_anomalies <- detect_anomalies(decomp$random, method = "zscore")
    
    list(
        seasonal = seasonal_anomalies,
        remainder = remainder_anomalies,
        decomposition = decomp
    )
}

#' Detect contextual anomalies
#' @param data Data frame with multiple variables
#' @param target Target variable for anomaly detection
#' @param context Context variables
#' @return List containing contextual anomalies
detect_contextual_anomalies <- function(data, target, context) {
    if (!all(c(target, context) %in% names(data))) {
        stop("Target or context variables not found in data")
    }
    
    # Fit a model using context variables
    formula <- as.formula(paste(target, "~", paste(context, collapse = "+")))
    model <- lm(formula, data = data)
    
    # Get residuals and detect anomalies in them
    residuals <- resid(model)
    residual_anomalies <- detect_anomalies(residuals, method = "zscore")
    
    list(
        anomalies = residual_anomalies$anomalies,
        scores = residual_anomalies$scores,
        model = model,
        residuals = residuals
    )
}

#' Generate anomaly report
#' @param anomaly_result Result from anomaly detection
#' @param data Original data
#' @return List containing summary statistics and visualizations
generate_anomaly_report <- function(anomaly_result, data) {
    n_anomalies <- sum(anomaly_result$anomalies)
    anomaly_rate <- n_anomalies / length(anomaly_result$anomalies)
    
    # Basic statistics
    stats <- list(
        total_observations = length(anomaly_result$anomalies),
        num_anomalies = n_anomalies,
        anomaly_rate = anomaly_rate,
        mean_score = mean(anomaly_result$scores),
        max_score = max(anomaly_result$scores)
    )
    
    # If bounds are available, include them
    if (!is.null(anomaly_result$bounds)) {
        stats$bounds <- anomaly_result$bounds
    }
    
    return(stats)
}