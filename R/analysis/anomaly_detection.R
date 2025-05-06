#' Anomaly Detection Functions
#' 
#' This module contains functions for detecting anomalies in time series data
#' using various statistical methods.

#' @import stats
#' @export
NULL

#' Detect anomalies in time series data
#' @param data Time series data
#' @param method Detection method (zscore, iqr)
#' @param threshold Threshold for anomaly detection
#' @return List containing anomalies and related statistics
#' @export
detect_anomalies <- function(data, method = "zscore", threshold = 0.95) {
    if (method == "zscore") {
        # Z-score method (3-sigma rule)
        mean_val <- mean(data, na.rm = TRUE)
        sd_val <- sd(data, na.rm = TRUE)
        z_scores <- abs((data - mean_val) / sd_val)
        anomalies <- z_scores > 3
        
        list(
            anomalies = anomalies,
            mean = mean_val,
            sd = sd_val,
            z_scores = z_scores,
            threshold = 3
        )
    } else if (method == "iqr") {
        # IQR method
        q1 <- quantile(data, 0.25, na.rm = TRUE)
        q3 <- quantile(data, 0.75, na.rm = TRUE)
        iqr <- q3 - q1
        lower_bound <- q1 - 1.5 * iqr
        upper_bound <- q3 + 1.5 * iqr
        anomalies <- data < lower_bound | data > upper_bound
        
        list(
            anomalies = anomalies,
            q1 = q1,
            q3 = q3,
            iqr = iqr,
            lower_bound = lower_bound,
            upper_bound = upper_bound
        )
    } else {
        stop("Unsupported anomaly detection method")
    }
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