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