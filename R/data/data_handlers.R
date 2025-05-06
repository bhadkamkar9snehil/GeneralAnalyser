# Data Handlers for GeneralAnalyser
# Basic data loading and preprocessing functions

#' Load data from a file
#' @param file_path Path to the data file
#' @param has_header Boolean indicating if file has headers
#' @return data.frame with loaded data
load_data <- function(file_path, has_header = TRUE) {
  ext <- tools::file_ext(file_path)
  
  data <- switch(ext,
    "csv" = read.csv(file_path, header = has_header),
    "xlsx" = readxl::read_excel(file_path),
    "xls" = readxl::read_excel(file_path),
    stop("Unsupported file format")
  )
  
  return(as.data.frame(data))
}

#' Basic data cleaning
#' @param data Input data.frame
#' @return Cleaned data.frame
clean_data <- function(data) {
  # Remove rows with all NA
  data <- data[rowSums(is.na(data)) != ncol(data), ]
  
  # Basic type conversion
  for(col in names(data)) {
    # Convert numeric strings to numeric
    if(is.character(data[[col]]) && !any(is.na(suppressWarnings(as.numeric(data[[col]]))))) {
      data[[col]] <- as.numeric(data[[col]])
    }
    
    # Convert date strings to Date
    if(is.character(data[[col]])) {
      date_format <- guess_date_format(data[[col]])
      if(!is.null(date_format)) {
        data[[col]] <- as.Date(data[[col]], format = date_format)
      }
    }
  }
  
  return(data)
}

#' Guess date format from string vector
#' @param x Character vector of potential dates
#' @return Date format string or NULL if not a date
guess_date_format <- function(x) {
  # Common date formats to try
  formats <- c(
    "%Y-%m-%d", "%Y/%m/%d",
    "%d-%m-%Y", "%d/%m/%Y",
    "%m-%d-%Y", "%m/%d/%Y"
  )
  
  # Try each format
  for(fmt in formats) {
    if(!any(is.na(suppressWarnings(as.Date(x[1], format = fmt))))) {
      return(fmt)
    }
  }
  
  return(NULL)
}