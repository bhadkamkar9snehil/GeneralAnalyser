#' Utility Functions
#' 
#' Helper functions for data preprocessing, model management,
#' and common operations.

#' @import caret
NULL

#' Preprocess data for analysis
#' @param data Input data frame
#' @param scale Whether to scale numeric variables
#' @param na.action How to handle NA values
#' @return Preprocessed data frame
preprocess_data <- function(data, scale = TRUE, na.action = "omit") {
    # Handle missing values
    if (na.action == "omit") {
        data <- na.omit(data)
    } else if (na.action == "impute") {
        # Implement imputation logic
    }
    
    # Scale numeric columns if requested
    if (scale) {
        numeric_cols <- sapply(data, is.numeric)
        data[numeric_cols] <- scale(data[numeric_cols])
    }
    
    return(data)
}

#' Save model to disk
#' @param model Model object to save
#' @param name Model name
#' @param path Path to save model
#' @return Path to saved model
save_model <- function(model, name, path = "models") {
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
    
    filename <- file.path(path, paste0(name, ".rds"))
    saveRDS(model, filename)
    return(filename)
}

#' Load model from disk
#' @param name Model name
#' @param path Path to load model from
#' @return Loaded model object
load_model <- function(name, path = "models") {
    filename <- file.path(path, paste0(name, ".rds"))
    if (!file.exists(filename)) {
        stop("Model file not found")
    }
    readRDS(filename)
}

#' Calculate model performance metrics
#' @param actual Actual values
#' @param predicted Predicted values
#' @param type Type of model (regression/classification)
#' @return List of performance metrics
calculate_metrics <- function(actual, predicted, type = "regression") {
    if (type == "regression") {
        list(
            RMSE = sqrt(mean((actual - predicted)^2)),
            MAE = mean(abs(actual - predicted)),
            R2 = cor(actual, predicted)^2
        )
    } else if (type == "classification") {
        # Add classification metrics
        conf_matrix <- table(actual, predicted)
        list(
            accuracy = sum(diag(conf_matrix)) / sum(conf_matrix),
            confusion_matrix = conf_matrix
        )
    }
}

# Helper functions for GeneralAnalyser
# Core utility functions for data handling and error management

#' Check if input is numeric and finite
#' @param x Input vector to check
#' @return Logical indicating if all values are valid numeric
is_valid_numeric <- function(x) {
  if(!is.numeric(x)) return(FALSE)
  all(is.finite(x))
}

#' Basic data validation
#' @param data Data frame to validate
#' @param required_cols Vector of required column names
#' @return List with validation status and any error messages
validate_data <- function(data, required_cols = NULL) {
  errors <- character()
  
  # Check if data is data frame
  if(!is.data.frame(data)) {
    errors <- c(errors, "Input must be a data frame")
    return(list(valid = FALSE, errors = errors))
  }
  
  # Check for required columns if specified
  if(!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(data))
    if(length(missing_cols) > 0) {
      errors <- c(errors, 
                 sprintf("Missing required columns: %s", 
                        paste(missing_cols, collapse = ", ")))
    }
  }
  
  list(
    valid = length(errors) == 0,
    errors = errors
  )
}

#' Format error message for UI display
#' @param error Error message or object
#' @return Formatted error string
format_error <- function(error) {
  if(is.character(error)) {
    return(error)
  }
  if(inherits(error, "error")) {
    return(error$message)
  }
  "An unknown error occurred"
}

#' Simple progress logging
#' @param msg Message to log
#' @param level Log level (INFO, WARNING, ERROR)
log_message <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  sprintf("[%s] [%s] %s", timestamp, level, msg)
}

#' Generate unique identifier
#' @return Character string with unique ID
generate_id <- function() {
  format(Sys.time(), "%Y%m%d_%H%M%S")
}