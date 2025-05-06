#' Data preprocessing functions
#' @export

preprocess_data <- function(data, numeric_cols = NULL) {
    # Handle missing values
    data <- na.omit(data)
    
    # Convert numeric columns if specified
    if (!is.null(numeric_cols)) {
        data[numeric_cols] <- lapply(data[numeric_cols], as.numeric)
    }
    
    # Scale numeric columns
    numeric_data <- data[sapply(data, is.numeric)]
    if (ncol(numeric_data) > 0) {
        data[names(numeric_data)] <- scale(numeric_data)
    }
    
    return(data)
}

handle_outliers <- function(data, cols, method = "zscore", threshold = 3) {
    for (col in cols) {
        if (method == "zscore") {
            z_scores <- scale(data[[col]])
            data[[col]][abs(z_scores) > threshold] <- NA
        }
    }
    data
}