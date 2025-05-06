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

#' Detect anomalies in data
#' @param data Data frame containing the target variable
#' @param target_col Target column name
#' @param method Detection method (Statistical (3-sigma), IQR, Isolation Forest)
#' @return List containing anomalies and metrics
detectAnomalies <- function(data, target_col, method = "Statistical (3-sigma)") {
    # Data preparation
    values <- data[[target_col]]
    if(!is.numeric(values)) {
        stop("Target column must be numeric")
    }
    
    # Detect anomalies using specified method
    result <- switch(method,
        "Statistical (3-sigma)" = detect_statistical(values),
        "IQR" = detect_iqr(values),
        "Isolation Forest" = detect_isolation_forest(data, target_col),
        stop("Unsupported anomaly detection method")
    )
    
    result$method <- method
    result$target_col <- target_col
    result
}

# Helper functions for different detection methods
detect_statistical <- function(values) {
    # Calculate mean and standard deviation
    mean_val <- mean(values, na.rm = TRUE)
    sd_val <- sd(values, na.rm = TRUE)
    
    # Detect anomalies (values outside 3 standard deviations)
    threshold <- 3
    upper_bound <- mean_val + threshold * sd_val
    lower_bound <- mean_val - threshold * sd_val
    
    anomalies <- values < lower_bound | values > upper_bound
    
    list(
        anomalies = anomalies,
        indices = which(anomalies),
        scores = abs((values - mean_val) / sd_val),
        threshold = threshold,
        upper_bound = upper_bound,
        lower_bound = lower_bound,
        mean = mean_val,
        sd = sd_val
    )
}

detect_iqr <- function(values) {
    # Calculate quartiles and IQR
    q1 <- quantile(values, 0.25, na.rm = TRUE)
    q3 <- quantile(values, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    
    # Define bounds (1.5 * IQR rule)
    threshold <- 1.5
    upper_bound <- q3 + threshold * iqr
    lower_bound <- q1 - threshold * iqr
    
    anomalies <- values < lower_bound | values > upper_bound
    
    list(
        anomalies = anomalies,
        indices = which(anomalies),
        scores = pmax(
            (values - upper_bound) / iqr,
            (lower_bound - values) / iqr
        ),
        threshold = threshold,
        upper_bound = upper_bound,
        lower_bound = lower_bound,
        q1 = q1,
        q3 = q3,
        iqr = iqr
    )
}

detect_isolation_forest <- function(data, target_col, contamination = 0.1) {
    # Use all numeric columns as features
    numeric_cols <- sapply(data, is.numeric)
    features <- data[, numeric_cols, drop = FALSE]
    
    # Standardize features
    features_scaled <- scale(features)
    
    # Train isolation forest
    model <- isotree::isolation.forest(
        features_scaled,
        ndim = ncol(features),
        ntrees = 100,
        sample_size = min(256, nrow(features)),
        contamination = contamination
    )
    
    # Get anomaly scores
    scores <- predict(model, features_scaled)
    
    # Determine threshold based on contamination
    threshold <- quantile(scores, 1 - contamination)
    anomalies <- scores > threshold
    
    list(
        anomalies = anomalies,
        indices = which(anomalies),
        scores = scores,
        threshold = threshold,
        model = model,
        contamination = contamination
    )
}

#' Plot anomaly detection results
#' @param results Results from detectAnomalies
#' @param data Original data frame
#' @return plotly object
plotAnomalies <- function(results, data) {
    values <- data[[results$target_col]]
    
    # Create basic scatter plot
    plot_data <- data.frame(
        Index = seq_along(values),
        Value = values,
        Anomaly = as.factor(results$anomalies)
    )
    
    p <- plot_ly(data = plot_data) %>%
        add_markers(x = ~Index, y = ~Value,
                   color = ~Anomaly,
                   colors = c("blue", "red"),
                   marker = list(size = 8),
                   name = c("Normal", "Anomaly")) %>%
        layout(title = paste(results$method, "Anomaly Detection"),
               xaxis = list(title = "Index"),
               yaxis = list(title = "Value"))
    
    # Add threshold lines for statistical and IQR methods
    if(results$method %in% c("Statistical (3-sigma)", "IQR")) {
        p <- p %>%
            add_lines(y = results$upper_bound, name = "Upper Bound",
                     line = list(dash = "dash", color = "gray")) %>%
            add_lines(y = results$lower_bound, name = "Lower Bound",
                     line = list(dash = "dash", color = "gray"))
    }
    
    p
}

#' Generate anomaly detection report
#' @param results Results from detectAnomalies
#' @param data Original data frame
#' @return List containing summary statistics and visualizations
generateAnomalyReport <- function(results, data) {
    n_total <- length(results$anomalies)
    n_anomalies <- sum(results$anomalies)
    
    # Basic statistics
    stats <- list(
        total_points = n_total,
        anomalies = n_anomalies,
        anomaly_rate = n_anomalies / n_total,
        method = results$method
    )
    
    # Method-specific statistics
    if(results$method == "Statistical (3-sigma)") {
        stats$mean <- results$mean
        stats$sd <- results$sd
        stats$threshold_sigmas <- results$threshold
    } else if(results$method == "IQR") {
        stats$q1 <- results$q1
        stats$q3 <- results$q3
        stats$iqr <- results$iqr
        stats$threshold_multiplier <- results$threshold
    } else if(results$method == "Isolation Forest") {
        stats$contamination <- results$contamination
        stats$threshold_score <- results$threshold
    }
    
    # Create anomaly table
    anomaly_indices <- results$indices
    anomaly_values <- data[[results$target_col]][anomaly_indices]
    anomaly_scores <- results$scores[anomaly_indices]
    
    anomaly_table <- data.frame(
        Index = anomaly_indices,
        Value = anomaly_values,
        Score = anomaly_scores
    )
    
    list(
        statistics = stats,
        anomalies = anomaly_table,
        plot = plotAnomalies(results, data)
    )
}