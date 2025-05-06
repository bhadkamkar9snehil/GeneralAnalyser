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

# Metrics Functions

#' Calculate regression metrics
#' @param actual Actual values
#' @param predicted Predicted values
#' @return List of regression metrics
calculateRegressionMetrics <- function(actual, predicted) {
    residuals <- actual - predicted
    n <- length(actual)
    
    # Basic metrics
    mse <- mean(residuals^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(residuals))
    mape <- mean(abs(residuals / actual)) * 100
    
    # R-squared calculation
    ss_tot <- sum((actual - mean(actual))^2)
    ss_res <- sum(residuals^2)
    r_squared <- 1 - (ss_res / ss_tot)
    
    # Adjusted R-squared (if predictors provided)
    adj_r_squared <- NULL
    if(!is.null(attr(predicted, "df"))) {
        p <- attr(predicted, "df")
        adj_r_squared <- 1 - ((1 - r_squared) * (n - 1) / (n - p - 1))
    }
    
    # Additional metrics
    rmsle <- sqrt(mean((log(actual + 1) - log(predicted + 1))^2))
    nrmse <- rmse / (max(actual) - min(actual))
    
    list(
        mse = mse,
        rmse = rmse,
        mae = mae,
        mape = mape,
        r_squared = r_squared,
        adj_r_squared = adj_r_squared,
        rmsle = rmsle,
        nrmse = nrmse
    )
}

#' Calculate classification metrics
#' @param actual Actual classes
#' @param predicted Predicted classes
#' @param probs Prediction probabilities (optional)
#' @return List of classification metrics
calculateClassificationMetrics <- function(actual, predicted, probs = NULL) {
    # Convert factors to numeric if needed
    if(is.factor(actual)) actual <- as.numeric(actual)
    if(is.factor(predicted)) predicted <- as.numeric(predicted)
    
    # Confusion matrix
    classes <- sort(unique(c(actual, predicted)))
    conf_matrix <- table(Predicted = predicted, Actual = actual)
    
    # Basic metrics
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    # For binary classification
    if(length(classes) == 2) {
        tp <- conf_matrix[2,2]
        fp <- conf_matrix[2,1]
        tn <- conf_matrix[1,1]
        fn <- conf_matrix[1,2]
        
        precision <- tp / (tp + fp)
        recall <- tp / (tp + fn)
        f1_score <- 2 * (precision * recall) / (precision + recall)
        specificity <- tn / (tn + fp)
        
        # ROC and AUC if probabilities provided
        roc_auc <- NULL
        if(!is.null(probs)) {
            roc <- calculateROC(actual == classes[2], probs[,2])
            roc_auc <- roc$auc
        }
        
        metrics <- list(
            accuracy = accuracy,
            precision = precision,
            recall = recall,
            f1_score = f1_score,
            specificity = specificity,
            confusion_matrix = conf_matrix,
            roc_auc = roc_auc
        )
    } else {
        # Multi-class metrics
        precision <- diag(conf_matrix) / colSums(conf_matrix)
        recall <- diag(conf_matrix) / rowSums(conf_matrix)
        f1_score <- 2 * (precision * recall) / (precision + recall)
        
        metrics <- list(
            accuracy = accuracy,
            macro_precision = mean(precision, na.rm = TRUE),
            macro_recall = mean(recall, na.rm = TRUE),
            macro_f1 = mean(f1_score, na.rm = TRUE),
            per_class = data.frame(
                Class = classes,
                Precision = precision,
                Recall = recall,
                F1 = f1_score
            ),
            confusion_matrix = conf_matrix
        )
    }
    
    metrics
}

#' Calculate clustering metrics
#' @param data Original data
#' @param clusters Cluster assignments
#' @param centers Cluster centers (optional)
#' @return List of clustering metrics
calculateClusteringMetrics <- function(data, clusters, centers = NULL) {
    n <- nrow(data)
    k <- length(unique(clusters))
    
    # Calculate silhouette score
    sil <- cluster::silhouette(clusters, dist(data))
    avg_silhouette <- mean(sil[,3])
    
    # Calculate Davies-Bouldin index
    if(!is.null(centers)) {
        db_index <- calculateDaviesBouldin(data, clusters, centers)
    } else {
        db_index <- NULL
    }
    
    # Calculate cluster sizes and proportions
    sizes <- table(clusters)
    proportions <- sizes / n
    
    # Calculate within-cluster sum of squares
    wss <- sapply(1:k, function(i) {
        cluster_points <- data[clusters == i,, drop = FALSE]
        if(nrow(cluster_points) > 0) {
            sum((scale(cluster_points, scale = FALSE))^2)
        } else {
            0
        }
    })
    
    list(
        n_clusters = k,
        sizes = sizes,
        proportions = proportions,
        total_wss = sum(wss),
        avg_silhouette = avg_silhouette,
        db_index = db_index
    )
}

#' Calculate time series metrics
#' @param actual Actual values
#' @param predicted Predicted values
#' @param frequency Time series frequency
#' @return List of time series metrics
calculateTimeSeriesMetrics <- function(actual, predicted, frequency = NULL) {
    # Basic error metrics
    basic_metrics <- calculateRegressionMetrics(actual, predicted)
    
    # Time series specific metrics
    residuals <- actual - predicted
    n <- length(actual)
    
    # ACF of residuals
    acf_resid <- acf(residuals, plot = FALSE)
    ljung_box <- Box.test(residuals, lag = min(10, n-1), type = "Ljung-Box")
    
    # Directional accuracy
    direction_actual <- diff(actual) > 0
    direction_pred <- diff(predicted) > 0
    directional_accuracy <- mean(direction_actual == direction_pred)
    
    # Seasonality test if frequency provided
    seasonal_test <- NULL
    if(!is.null(frequency)) {
        ts_data <- ts(actual, frequency = frequency)
        decomp <- decompose(ts_data)
        seasonal_strength <- var(decomp$seasonal, na.rm = TRUE) / 
                           (var(decomp$seasonal, na.rm = TRUE) + var(decomp$random, na.rm = TRUE))
    }
    
    c(basic_metrics,
      list(
          acf_residuals = acf_resid$acf,
          ljung_box_test = ljung_box$p.value,
          directional_accuracy = directional_accuracy,
          seasonal_strength = if(!is.null(frequency)) seasonal_strength else NULL
      ))
}

#' Calculate anomaly detection metrics
#' @param actual Actual anomaly labels
#' @param predicted Predicted anomaly labels
#' @param scores Anomaly scores
#' @return List of anomaly detection metrics
calculateAnomalyMetrics <- function(actual, predicted, scores = NULL) {
    # Convert to binary format
    actual_binary <- as.numeric(actual)
    predicted_binary <- as.numeric(predicted)
    
    # Basic classification metrics
    class_metrics <- calculateClassificationMetrics(actual_binary, predicted_binary)
    
    # Precision@k and Recall@k if scores provided
    prec_k <- rec_k <- NULL
    if(!is.null(scores)) {
        k <- sum(actual_binary)  # Number of true anomalies
        ordered_idx <- order(scores, decreasing = TRUE)[1:k]
        prec_k <- mean(actual_binary[ordered_idx])
        rec_k <- sum(actual_binary[ordered_idx]) / sum(actual_binary)
    }
    
    # Average precision score
    avg_precision <- NULL
    if(!is.null(scores)) {
        avg_precision <- calculateAveragePrecision(actual_binary, scores)
    }
    
    c(class_metrics,
      list(
          precision_at_k = prec_k,
          recall_at_k = rec_k,
          average_precision = avg_precision
      ))
}

# Helper functions

#' Calculate ROC curve and AUC
#' @param actual Binary actual values
#' @param scores Prediction scores
#' @return List with ROC curve points and AUC
calculateROC <- function(actual, scores) {
    # Sort by descending scores
    ord <- order(scores, decreasing = TRUE)
    actual <- actual[ord]
    scores <- scores[ord]
    
    # Calculate TPR and FPR
    tpr <- cumsum(actual) / sum(actual)
    fpr <- cumsum(!actual) / sum(!actual)
    
    # Calculate AUC using trapezoidal rule
    auc <- sum(diff(fpr) * (tpr[-1] + tpr[-length(tpr)]) / 2)
    
    list(
        tpr = tpr,
        fpr = fpr,
        auc = auc
    )
}

#' Calculate Davies-Bouldin index
#' @param data Data points
#' @param clusters Cluster assignments
#' @param centers Cluster centers
#' @return Davies-Bouldin index
calculateDaviesBouldin <- function(data, clusters, centers) {
    k <- nrow(centers)
    
    # Calculate average distances within clusters
    avg_dist <- numeric(k)
    for(i in 1:k) {
        cluster_points <- data[clusters == i,, drop = FALSE]
        if(nrow(cluster_points) > 0) {
            avg_dist[i] <- mean(sqrt(rowSums((cluster_points - centers[i,])^2)))
        }
    }
    
    # Calculate Davies-Bouldin index
    max_ratio <- numeric(k)
    for(i in 1:k) {
        ratios <- numeric(k)
        for(j in 1:k) {
            if(i != j) {
                center_dist <- sqrt(sum((centers[i,] - centers[j,])^2))
                ratios[j] <- (avg_dist[i] + avg_dist[j]) / center_dist
            }
        }
        max_ratio[i] <- max(ratios)
    }
    
    mean(max_ratio)
}

#' Calculate average precision score
#' @param actual Binary actual values
#' @param scores Prediction scores
#' @return Average precision score
calculateAveragePrecision <- function(actual, scores) {
    ord <- order(scores, decreasing = TRUE)
    actual <- actual[ord]
    
    precision <- cumsum(actual) / seq_along(actual)
    sum(precision * actual) / sum(actual)
}