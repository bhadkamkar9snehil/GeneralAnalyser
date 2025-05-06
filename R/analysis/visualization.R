#' Visualization functions for GeneralAnalyser
#' @export

plot_time_series <- function(data, time_col, value_col, title = "Time Series Plot") {
    ggplot(data, aes_string(x = time_col, y = value_col)) +
        geom_line() +
        labs(title = title) +
        theme_minimal()
}

plot_correlation_matrix <- function(data, method = "pearson") {
    cor_matrix <- cor(data, method = method)
    melted_cor <- reshape2::melt(cor_matrix)
    ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Visualization Functions

#' Create time series plots
#' @param data Time series data
#' @param type Type of plot
#' @return plotly object
plotTimeSeries <- function(data, type = "line") {
    plot_data <- if(is.data.frame(data)) {
        data
    } else {
        data.frame(
            Time = seq_along(data),
            Value = as.numeric(data)
        )
    }
    
    p <- switch(type,
        "line" = {
            plot_ly(data = plot_data) %>%
                add_lines(x = ~Time, y = ~Value) %>%
                layout(title = "Time Series Plot",
                       xaxis = list(title = "Time"),
                       yaxis = list(title = "Value"))
        },
        "decomposition" = {
            ts_obj <- ts(plot_data$Value)
            decomp <- decompose(ts_obj)
            plot_data <- data.frame(
                Time = rep(plot_data$Time, 4),
                Value = c(plot_data$Value, decomp$trend,
                         decomp$seasonal, decomp$random),
                Component = factor(rep(c("Original", "Trend",
                                       "Seasonal", "Random"),
                                     each = length(plot_data$Time)))
            )
            
            plot_ly(data = plot_data) %>%
                add_lines(x = ~Time, y = ~Value, color = ~Component) %>%
                layout(title = "Time Series Decomposition",
                       xaxis = list(title = "Time"),
                       yaxis = list(title = "Value"))
        },
        stop("Unsupported plot type")
    )
    
    p
}

#' Create forecast plots
#' @param actual Actual values
#' @param forecast Forecast object
#' @param confidence_level Confidence level for prediction intervals
#' @return plotly object
plotForecast <- function(actual, forecast, confidence_level = 0.95) {
    # Extract forecast components
    point_forecast <- as.numeric(forecast$mean)
    lower <- as.numeric(forecast$lower[, 1])
    upper <- as.numeric(forecast$upper[, 1])
    
    # Create plot data
    plot_data <- data.frame(
        Time = c(seq_along(actual), seq_along(point_forecast) + length(actual)),
        Value = c(actual, point_forecast),
        Type = factor(c(rep("Actual", length(actual)),
                       rep("Forecast", length(point_forecast))))
    )
    
    # Create confidence interval data
    ci_data <- data.frame(
        Time = seq_along(point_forecast) + length(actual),
        Lower = lower,
        Upper = upper
    )
    
    # Create plot
    p <- plot_ly() %>%
        add_lines(data = plot_data, x = ~Time, y = ~Value,
                 color = ~Type, colors = c("black", "blue")) %>%
        add_ribbons(data = ci_data, x = ~Time,
                   ymin = ~Lower, ymax = ~Upper,
                   fillcolor = "rgba(0,0,255,0.2)",
                   line = list(color = "transparent"),
                   showlegend = FALSE,
                   name = paste0(confidence_level * 100, "% CI")) %>%
        layout(title = "Forecast Plot",
               xaxis = list(title = "Time"),
               yaxis = list(title = "Value"))
    
    p
}

#' Create regression diagnostic plots
#' @param model Regression model object
#' @return List of plotly objects
plotRegressionDiagnostics <- function(model) {
    # Extract required components
    fitted_vals <- fitted(model)
    residuals <- resid(model)
    std_residuals <- rstandard(model)
    
    # Residuals vs Fitted
    p1 <- plot_ly() %>%
        add_markers(x = fitted_vals, y = residuals) %>%
        add_lines(x = c(min(fitted_vals), max(fitted_vals)),
                 y = c(0, 0), line = list(dash = "dash")) %>%
        layout(title = "Residuals vs Fitted",
               xaxis = list(title = "Fitted values"),
               yaxis = list(title = "Residuals"))
    
    # Normal Q-Q
    qq <- qqnorm(std_residuals, plot.it = FALSE)
    p2 <- plot_ly() %>%
        add_markers(x = qq$x, y = qq$y) %>%
        add_lines(x = range(qq$x), y = range(qq$x),
                 line = list(dash = "dash")) %>%
        layout(title = "Normal Q-Q",
               xaxis = list(title = "Theoretical Quantiles"),
               yaxis = list(title = "Standardized Residuals"))
    
    # Scale-Location
    p3 <- plot_ly() %>%
        add_markers(x = fitted_vals,
                   y = sqrt(abs(std_residuals))) %>%
        layout(title = "Scale-Location",
               xaxis = list(title = "Fitted values"),
               yaxis = list(title = "√|Standardized residuals|"))
    
    # Leverage
    leverage <- hatvalues(model)
    p4 <- plot_ly() %>%
        add_markers(x = leverage, y = std_residuals) %>%
        layout(title = "Residuals vs Leverage",
               xaxis = list(title = "Leverage"),
               yaxis = list(title = "Standardized Residuals"))
    
    list(
        residuals_fitted = p1,
        qq = p2,
        scale_location = p3,
        residuals_leverage = p4
    )
}

#' Create confusion matrix plot
#' @param conf_matrix Confusion matrix
#' @return plotly object
plotConfusionMatrix <- function(conf_matrix) {
    # Convert to data frame
    df <- as.data.frame(as.table(conf_matrix))
    names(df) <- c("Predicted", "Actual", "Count")
    
    # Calculate percentages
    total <- sum(df$Count)
    df$Percentage <- df$Count / total * 100
    
    # Create text labels
    df$Text <- sprintf("%d\n(%.1f%%)", df$Count, df$Percentage)
    
    # Create heatmap
    plot_ly(data = df,
            x = ~Predicted,
            y = ~Actual,
            z = ~Count,
            text = ~Text,
            type = "heatmap",
            hoverongaps = FALSE,
            hovertemplate = paste(
                "<b>Actual:</b> %{y}<br>",
                "<b>Predicted:</b> %{x}<br>",
                "<b>Count:</b> %{z}<br>",
                "<b>Percentage:</b> %{text}<br>",
                "<extra></extra>"
            )) %>%
        layout(title = "Confusion Matrix",
               xaxis = list(title = "Predicted Class"),
               yaxis = list(title = "Actual Class"))
}

#' Create ROC curve plot
#' @param roc_data ROC curve data
#' @return plotly object
plotROC <- function(roc_data) {
    plot_ly() %>%
        add_lines(x = roc_data$fpr, y = roc_data$tpr,
                 name = sprintf("ROC (AUC = %.3f)", roc_data$auc)) %>%
        add_lines(x = c(0,1), y = c(0,1),
                 line = list(dash = "dash"),
                 name = "Random") %>%
        layout(title = "ROC Curve",
               xaxis = list(title = "False Positive Rate"),
               yaxis = list(title = "True Positive Rate"),
               showlegend = TRUE)
}

#' Create clustering plot
#' @param data Original data
#' @param clusters Cluster assignments
#' @param centers Cluster centers (optional)
#' @return plotly object
plotClustering <- function(data, clusters, centers = NULL) {
    if(ncol(data) > 2) {
        # Use PCA for dimensionality reduction if more than 2 dimensions
        pca <- prcomp(data, scale. = TRUE)
        plot_data <- as.data.frame(pca$x[, 1:2])
        names(plot_data) <- c("PC1", "PC2")
        if(!is.null(centers)) {
            centers_pca <- predict(pca, centers)[, 1:2]
        }
    } else {
        plot_data <- as.data.frame(data)
        names(plot_data) <- c("PC1", "PC2")
        centers_pca <- centers
    }
    
    plot_data$Cluster <- as.factor(clusters)
    
    p <- plot_ly() %>%
        add_markers(data = plot_data,
                   x = ~PC1, y = ~PC2,
                   color = ~Cluster,
                   showlegend = TRUE)
    
    if(!is.null(centers)) {
        p <- p %>%
            add_markers(data = as.data.frame(centers_pca),
                       x = ~V1, y = ~V2,
                       symbol = I("x"),
                       size = I(10),
                       color = I("black"),
                       name = "Centroids")
    }
    
    p %>% layout(title = "Cluster Plot",
                xaxis = list(title = "Principal Component 1"),
                yaxis = list(title = "Principal Component 2"))
}

#' Create feature importance plot
#' @param importance Feature importance scores
#' @param max_features Maximum number of features to show
#' @return plotly object
plotFeatureImportance <- function(importance, max_features = 20) {
    # Sort importance scores
    df <- data.frame(
        Feature = names(importance),
        Importance = as.numeric(importance)
    )
    df <- df[order(-df$Importance), ]
    
    # Limit number of features
    if(nrow(df) > max_features) {
        df <- df[1:max_features, ]
    }
    
    # Create bar plot
    plot_ly(data = df,
            x = ~reorder(Feature, Importance),
            y = ~Importance,
            type = "bar") %>%
        layout(title = "Feature Importance",
               xaxis = list(title = "Feature",
                          tickangle = 45),
               yaxis = list(title = "Importance Score"))
}

#' Create correlation plot
#' @param data Data frame
#' @param method Correlation method
#' @return plotly object
plotCorrelation <- function(data, method = "pearson") {
    # Calculate correlation matrix
    cor_matrix <- cor(data, method = method)
    
    # Create correlation plot
    plot_ly(
        x = colnames(cor_matrix),
        y = colnames(cor_matrix),
        z = cor_matrix,
        type = "heatmap",
        colors = colorRamp(c("#4575b4", "#white", "#d73027")),
        zmin = -1,
        zmax = 1
    ) %>%
        layout(
            title = "Correlation Matrix",
            xaxis = list(title = ""),
            yaxis = list(title = "")
        )
}

#' Create anomaly detection plot
#' @param data Original data
#' @param anomalies Anomaly indicators
#' @param scores Anomaly scores (optional)
#' @return plotly object
plotAnomalies <- function(data, anomalies, scores = NULL) {
    plot_data <- data.frame(
        Index = seq_along(data),
        Value = data,
        Anomaly = factor(anomalies, levels = c(FALSE, TRUE),
                        labels = c("Normal", "Anomaly"))
    )
    
    p <- plot_ly() %>%
        add_markers(data = plot_data,
                   x = ~Index, y = ~Value,
                   color = ~Anomaly,
                   colors = c("blue", "red")) %>%
        layout(title = "Anomaly Detection",
               xaxis = list(title = "Index"),
               yaxis = list(title = "Value"))
    
    if(!is.null(scores)) {
        # Add scores as a secondary y-axis
        plot_data$Score <- scores
        p <- p %>%
            add_lines(data = plot_data,
                     x = ~Index, y = ~Score,
                     name = "Anomaly Score",
                     yaxis = "y2") %>%
            layout(
                yaxis2 = list(
                    title = "Anomaly Score",
                    overlaying = "y",
                    side = "right"
                )
            )
    }
    
    p
}