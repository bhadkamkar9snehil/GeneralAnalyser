#' Regression Analysis Module
#' Basic regression analysis functionality

#' Fit linear regression model
#' @param data Input data.frame
#' @param dependent Name of dependent variable
#' @param independents Vector of independent variable names
#' @return Fitted lm object
fit_linear_model <- function(data, dependent, independents) {
  # Create formula
  formula <- as.formula(paste(dependent, "~", paste(independents, collapse = " + ")))
  
  tryCatch({
    lm(formula, data = data)
  }, error = function(e) {
    warning("Error fitting linear model: ", e$message)
    NULL
  })
}

#' Generate predictions from regression model
#' @param model Fitted regression model
#' @param newdata Data frame with prediction inputs
#' @return Vector of predictions
predict_regression <- function(model, newdata) {
  if(is.null(model)) {
    stop("No valid model provided")
  }
  
  tryCatch({
    predict(model, newdata = newdata)
  }, error = function(e) {
    warning("Error generating predictions: ", e$message)
    NULL
  })
}

#' Calculate regression metrics
#' @param model Fitted regression model
#' @return List of model metrics
calculate_regression_metrics <- function(model) {
  if(is.null(model)) {
    stop("No valid model provided")
  }
  
  summary_stats <- summary(model)
  
  list(
    R_squared = summary_stats$r.squared,
    Adj_R_squared = summary_stats$adj.r.squared,
    F_statistic = summary_stats$fstatistic[1],
    p_value = pf(
      summary_stats$fstatistic[1],
      summary_stats$fstatistic[2],
      summary_stats$fstatistic[3],
      lower.tail = FALSE
    )
  )
}

#' Generate regression diagnostics plots
#' @param model Fitted regression model
#' @return List of ggplot objects
generate_diagnostic_plots <- function(model) {
  if(is.null(model)) {
    stop("No valid model provided")
  }
  
  # Residuals vs Fitted
  p1 <- ggplot2::ggplot(data.frame(
    fitted = fitted(model),
    residuals = residuals(model)
  ), ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "Residuals vs Fitted",
                  x = "Fitted values",
                  y = "Residuals")
  
  # QQ Plot
  p2 <- ggplot2::ggplot(data.frame(
    theoretical = qqnorm(residuals(model), plot.it = FALSE)$x,
    sample = qqnorm(residuals(model), plot.it = FALSE)$y
  ), ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    ggplot2::labs(title = "Normal Q-Q Plot",
                  x = "Theoretical Quantiles",
                  y = "Sample Quantiles")
  
  list(
    residuals_vs_fitted = p1,
    qq_plot = p2
  )
}