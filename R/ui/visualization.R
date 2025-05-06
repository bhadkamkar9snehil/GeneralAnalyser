#' Visualization Module
#' Core plotting functions for time series and regression analysis

library(ggplot2)
library(plotly)

#' Create time series plot
#' @param data Time series data
#' @param forecast Optional forecast object
#' @return plotly object
plot_time_series <- function(data, forecast = NULL) {
  p <- ggplot() +
    geom_line(data = data.frame(
      time = time(data),
      value = as.numeric(data)
    ), aes(x = time, y = value, color = "Actual")) +
    labs(title = "Time Series Analysis",
         x = "Time",
         y = "Value") +
    theme_minimal()
  
  if(!is.null(forecast)) {
    # Add forecast line and confidence intervals
    forecast_df <- data.frame(
      time = time(forecast$mean),
      value = as.numeric(forecast$mean),
      lower = as.numeric(forecast$lower[,"80%"]),
      upper = as.numeric(forecast$upper[,"80%"])
    )
    
    p <- p +
      geom_line(data = forecast_df, 
                aes(x = time, y = value, color = "Forecast")) +
      geom_ribbon(data = forecast_df,
                  aes(x = time, ymin = lower, ymax = upper),
                  alpha = 0.2) +
      scale_color_manual(values = c("Actual" = "#2c3e50", "Forecast" = "#e74c3c"))
  }
  
  ggplotly(p)
}

#' Create regression scatter plot
#' @param data Data frame
#' @param x Name of x variable
#' @param y Name of y variable
#' @param fitted Optional fitted values
#' @return plotly object
plot_regression <- function(data, x, y, fitted = NULL) {
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(color = "#2c3e50", alpha = 0.6) +
    labs(title = "Regression Analysis") +
    theme_minimal()
  
  if(!is.null(fitted)) {
    p <- p + 
      geom_line(aes(y = fitted),
                color = "#e74c3c",
                size = 1)
  }
  
  ggplotly(p)
}

#' Create residuals plot
#' @param model Fitted model object
#' @return plotly object
plot_residuals <- function(model) {
  df <- data.frame(
    fitted = fitted(model),
    residuals = residuals(model)
  )
  
  p <- ggplot(df, aes(x = fitted, y = residuals)) +
    geom_point(color = "#2c3e50", alpha = 0.6) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#e74c3c") +
    labs(title = "Residuals vs Fitted",
         x = "Fitted values",
         y = "Residuals") +
    theme_minimal()
  
  ggplotly(p)
}

#' Create normal Q-Q plot
#' @param model Fitted model object
#' @return plotly object
plot_qq <- function(model) {
  qq_data <- qqnorm(residuals(model), plot.it = FALSE)
  df <- data.frame(
    theoretical = qq_data$x,
    sample = qq_data$y
  )
  
  p <- ggplot(df, aes(x = theoretical, y = sample)) +
    geom_point(color = "#2c3e50", alpha = 0.6) +
    geom_abline(intercept = 0, slope = 1, 
                linetype = "dashed", color = "#e74c3c") +
    labs(title = "Normal Q-Q Plot",
         x = "Theoretical Quantiles",
         y = "Sample Quantiles") +
    theme_minimal()
  
  ggplotly(p)
}