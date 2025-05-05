#' Visualization Functions
#' 
#' This module contains functions for creating various types of visualizations
#' including charts, plots, and dashboards.

#' @import ggplot2
#' @import plotly
#' @import DT
NULL

#' Create time series plot
#' @param data Time series data
#' @param title Plot title
#' @param interactive Whether to create interactive plot
#' @return Plot object
create_ts_plot <- function(data, title = "Time Series Plot", interactive = TRUE) {
    p <- ggplot(data, aes(x = time, y = value)) +
        geom_line(color = "#3498DB") +
        geom_point(color = "#2C3E50", size = 2, alpha = 0.6) +
        theme_minimal() +
        labs(title = title,
             x = "Time",
             y = "Value") +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)
        )
    
    if (interactive) {
        return(ggplotly(p))
    }
    return(p)
}

#' Create regression plot
#' @param data Data frame containing x and y variables
#' @param x_var X variable name
#' @param y_var Y variable name
#' @param add_ci Add confidence interval
#' @return Plot object
create_regression_plot <- function(data, x_var, y_var, add_ci = TRUE) {
    p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
        geom_point(color = "#3498DB", alpha = 0.6) +
        geom_smooth(method = "lm", color = "#2C3E50", 
                   se = add_ci, fill = "#3498DB", alpha = 0.2) +
        theme_minimal() +
        labs(title = "Regression Analysis",
             x = x_var,
             y = y_var)
    
    return(ggplotly(p))
}

#' Create classification plot
#' @param data Data frame with features and class labels
#' @param x_var First feature
#' @param y_var Second feature
#' @param class_var Class variable
#' @return Plot object
create_classification_plot <- function(data, x_var, y_var, class_var) {
    p <- ggplot(data, aes_string(x = x_var, y = y_var, color = class_var)) +
        geom_point(size = 3, alpha = 0.6) +
        scale_color_brewer(palette = "Set1") +
        theme_minimal() +
        labs(title = "Classification Results",
             x = x_var,
             y = y_var,
             color = "Class")
    
    return(ggplotly(p))
}

#' Create clustering plot
#' @param data Data frame of features
#' @param clusters Cluster assignments
#' @param x_var First feature
#' @param y_var Second feature
#' @return Plot object
create_clustering_plot <- function(data, clusters, x_var, y_var) {
    plot_data <- cbind(data, Cluster = as.factor(clusters))
    
    p <- ggplot(plot_data, aes_string(x = x_var, y = y_var, color = "Cluster")) +
        geom_point(size = 3, alpha = 0.6) +
        scale_color_brewer(palette = "Set2") +
        theme_minimal() +
        labs(title = "Clustering Results",
             x = x_var,
             y = y_var)
    
    return(ggplotly(p))
}

#' Create anomaly detection plot
#' @param data Time series data
#' @param anomalies Logical vector indicating anomalies
#' @return Plot object
create_anomaly_plot <- function(data, anomalies) {
    plot_data <- data.frame(
        time = seq_along(data),
        value = data,
        anomaly = anomalies
    )
    
    p <- ggplot(plot_data, aes(x = time, y = value)) +
        geom_line(color = "#3498DB") +
        geom_point(data = subset(plot_data, !anomaly),
                  color = "#2C3E50", size = 2, alpha = 0.6) +
        geom_point(data = subset(plot_data, anomaly),
                  color = "#E74C3C", size = 3) +
        theme_minimal() +
        labs(title = "Anomaly Detection Results",
             x = "Time",
             y = "Value")
    
    return(ggplotly(p))
}

#' Create interactive data table
#' @param data Data frame to display
#' @param options List of DataTable options
#' @return DataTable object
create_data_table <- function(data, options = list()) {
    default_options <- list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel", "pdf"),
        searchHighlight = TRUE
    )
    
    options <- modifyList(default_options, options)
    
    datatable(data,
             options = options,
             rownames = FALSE,
             filter = "top",
             extensions = c("Buttons", "SearchHighlight"))
}

#' Create dashboard metrics
#' @param metrics List of metric values
#' @return HTML widget for displaying metrics
create_metrics_dashboard <- function(metrics) {
    validate_metrics(metrics)
    
    html_output <- tags$div(
        class = "metrics-dashboard",
        style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1rem;",
        
        Map(function(name, value) {
            tags$div(
                class = "metric-card",
                style = "padding: 1rem; background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                tags$h3(style = "margin: 0; color: #2C3E50;", name),
                tags$p(style = "font-size: 24px; font-weight: bold; margin: 8px 0; color: #3498DB;",
                      format_metric_value(value)),
                tags$p(style = "margin: 0; color: #7F8C8D;",
                      get_metric_description(name))
            )
        }, names(metrics), metrics)
    )
    
    return(html_output)
}

# Helper functions

#' Validate metrics input
#' @param metrics List of metrics to validate
#' @return Logical indicating if metrics are valid
validate_metrics <- function(metrics) {
    if (!is.list(metrics) || length(metrics) == 0) {
        stop("Metrics must be a non-empty list")
    }
    
    if (!all(sapply(metrics, is.numeric))) {
        stop("All metric values must be numeric")
    }
    
    return(TRUE)
}

#' Format metric value for display
#' @param value Numeric value to format
#' @return Formatted string
format_metric_value <- function(value) {
    if (abs(value) >= 1000000) {
        return(paste0(round(value/1000000, 1), "M"))
    } else if (abs(value) >= 1000) {
        return(paste0(round(value/1000, 1), "K"))
    } else if (is.integer(value)) {
        return(as.character(value))
    } else {
        return(format(round(value, 2), nsmall = 2))
    }
}

#' Get metric description
#' @param name Name of the metric
#' @return Description string
get_metric_description <- function(name) {
    descriptions <- list(
        accuracy = "Model prediction accuracy",
        rmse = "Root mean square error",
        mae = "Mean absolute error",
        r2 = "R-squared value",
        num_clusters = "Number of clusters",
        silhouette = "Silhouette score",
        num_anomalies = "Number of anomalies detected"
    )
    
    return(descriptions[[name]] %||% "Metric value")