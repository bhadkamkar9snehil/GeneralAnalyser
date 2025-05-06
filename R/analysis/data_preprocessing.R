# Data Preprocessing Functions

#' Clean and preprocess data
#' @param data Data frame to process
#' @param config Preprocessing configuration
#' @return Preprocessed data frame
preprocessData <- function(data, config = list()) {
    # Set default configuration
    default_config <- list(
        remove_duplicates = TRUE,
        handle_missing = "impute",
        handle_outliers = TRUE,
        standardize = TRUE,
        categorical_encoding = "onehot",
        date_handling = "features"
    )
    config <- modifyList(default_config, config)
    
    result <- data
    
    # Remove duplicates
    if(config$remove_duplicates) {
        result <- removeDuplicates(result)
    }
    
    # Handle missing values
    if(!is.null(config$handle_missing)) {
        result <- handleMissingValues(result, method = config$handle_missing)
    }
    
    # Handle outliers
    if(config$handle_outliers) {
        numeric_cols <- sapply(result, is.numeric)
        result <- handleOutliers(result, names(numeric_cols)[numeric_cols])
    }
    
    # Standardize numeric columns
    if(config$standardize) {
        result <- standardizeFeatures(result)
    }
    
    # Encode categorical variables
    if(!is.null(config$categorical_encoding)) {
        result <- encodeCategoricalVariables(result, method = config$categorical_encoding)
    }
    
    # Handle date/time columns
    if(!is.null(config$date_handling)) {
        result <- handleDateColumns(result, method = config$date_handling)
    }
    
    attr(result, "preprocessing_config") <- config
    result
}

#' Remove duplicate rows
#' @param data Data frame
#' @return Data frame without duplicates
removeDuplicates <- function(data) {
    unique(data)
}

#' Handle missing values
#' @param data Data frame
#' @param method Method to handle missing values
#' @return Data frame with handled missing values
handleMissingValues <- function(data, method = "impute") {
    result <- data
    
    # Process each column
    for(col in names(data)) {
        if(any(is.na(data[[col]]))) {
            result[[col]] <- switch(method,
                "remove" = {
                    result <- result[!is.na(result[[col]]), ]
                    result[[col]]
                },
                "impute" = {
                    if(is.numeric(data[[col]])) {
                        # For numeric columns, use median imputation
                        replace_val <- median(data[[col]], na.rm = TRUE)
                    } else if(is.factor(data[[col]]) || is.character(data[[col]])) {
                        # For categorical columns, use mode imputation
                        replace_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
                    } else {
                        # For other types, keep NA
                        next
                    }
                    replace(data[[col]], is.na(data[[col]]), replace_val)
                },
                "indicator" = {
                    # Create missing value indicator
                    indicator_name <- paste0(col, "_is_missing")
                    result[[indicator_name]] <- is.na(data[[col]])
                    
                    # Then impute the original column
                    if(is.numeric(data[[col]])) {
                        result[[col]] <- replace(data[[col]], is.na(data[[col]]),
                                               median(data[[col]], na.rm = TRUE))
                    } else if(is.factor(data[[col]]) || is.character(data[[col]])) {
                        mode_val <- names(sort(table(data[[col]]), decreasing = TRUE))[1]
                        result[[col]] <- replace(data[[col]], is.na(data[[col]]), mode_val)
                    }
                    result[[col]]
                },
                stop("Unsupported missing value handling method")
            )
        }
    }
    
    result
}

#' Standardize numeric features
#' @param data Data frame
#' @param exclude Columns to exclude from standardization
#' @return Data frame with standardized features
standardizeFeatures <- function(data, exclude = NULL) {
    result <- data
    
    # Identify numeric columns to standardize
    numeric_cols <- sapply(data, is.numeric)
    if(!is.null(exclude)) {
        numeric_cols[exclude] <- FALSE
    }
    
    # Standardize each numeric column
    for(col in names(numeric_cols)[numeric_cols]) {
        values <- data[[col]]
        if(var(values, na.rm = TRUE) > 0) {  # Only standardize if variance > 0
            result[[col]] <- scale(values)
        }
    }
    
    result
}

#' Encode categorical variables
#' @param data Data frame
#' @param method Encoding method
#' @return Data frame with encoded categories
encodeCategoricalVariables <- function(data, method = "onehot") {
    result <- data
    
    # Identify categorical columns
    categorical_cols <- sapply(data, function(x) is.factor(x) || is.character(x))
    
    if(any(categorical_cols)) {
        result <- switch(method,
            "onehot" = {
                # One-hot encoding
                model_matrix <- stats::model.matrix(
                    ~ . - 1,
                    data = data[, categorical_cols, drop = FALSE]
                )
                # Combine with non-categorical columns
                cbind(
                    data[, !categorical_cols, drop = FALSE],
                    as.data.frame(model_matrix)
                )
            },
            "label" = {
                # Label encoding
                for(col in names(categorical_cols)[categorical_cols]) {
                    result[[col]] <- as.numeric(as.factor(data[[col]]))
                }
                result
            },
            "frequency" = {
                # Frequency encoding
                for(col in names(categorical_cols)[categorical_cols]) {
                    freq_table <- table(data[[col]]) / nrow(data)
                    result[[col]] <- freq_table[match(data[[col]], names(freq_table))]
                }
                result
            },
            stop("Unsupported categorical encoding method")
        )
    }
    
    result
}

#' Handle date/time columns
#' @param data Data frame
#' @param method Method to handle date columns
#' @return Data frame with processed date features
handleDateColumns <- function(data, method = "features") {
    result <- data
    
    # Identify date columns
    date_cols <- sapply(data, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")))
    
    if(any(date_cols)) {
        for(col in names(date_cols)[date_cols]) {
            dates <- data[[col]]
            
            result <- switch(method,
                "features" = {
                    # Extract date components as features
                    cbind(
                        result[, !date_cols, drop = FALSE],
                        createTimeFeatures(data.frame(date = dates), "date")
                    )
                },
                "numeric" = {
                    # Convert to numeric (seconds since epoch)
                    result[[col]] <- as.numeric(dates)
                    result
                },
                "keep" = {
                    # Keep original date format
                    result
                },
                stop("Unsupported date handling method")
            )
        }
    }
    
    result
}

#' Validate data quality
#' @param data Data frame
#' @param rules List of validation rules
#' @return List of validation results
validateData <- function(data, rules = NULL) {
    if(is.null(rules)) {
        rules <- list(
            missing_values = TRUE,
            duplicates = TRUE,
            outliers = TRUE,
            type_consistency = TRUE
        )
    }
    
    results <- list()
    
    # Check missing values
    if(rules$missing_values) {
        missing_summary <- sapply(data, function(x) sum(is.na(x)))
        results$missing_values <- list(
            counts = missing_summary,
            total = sum(missing_summary),
            rate = sum(missing_summary) / (nrow(data) * ncol(data))
        )
    }
    
    # Check duplicates
    if(rules$duplicates) {
        dup_rows <- duplicated(data)
        results$duplicates <- list(
            count = sum(dup_rows),
            rate = sum(dup_rows) / nrow(data)
        )
    }
    
    # Check outliers in numeric columns
    if(rules$outliers) {
        numeric_cols <- sapply(data, is.numeric)
        outlier_summary <- lapply(data[numeric_cols], function(x) {
            q1 <- quantile(x, 0.25, na.rm = TRUE)
            q3 <- quantile(x, 0.75, na.rm = TRUE)
            iqr <- q3 - q1
            outliers <- x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr)
            sum(outliers, na.rm = TRUE)
        })
        results$outliers <- outlier_summary
    }
    
    # Check type consistency
    if(rules$type_consistency) {
        type_summary <- sapply(data, class)
        results$type_consistency <- type_summary
    }
    
    results
}