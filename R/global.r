# global.R
# Load required packages and set global options

# Function to check package installation status
check_and_install_packages <- function(pkgs) {
  for(pkg in pkgs) {
    tryCatch({
      if(!requireNamespace(pkg, quietly = TRUE)) {
        message(sprintf("Installing package: %s", pkg))
        install.packages(pkg, repos = "http://cran.us.r-project.org", quiet = TRUE)
      }
      library(pkg, character.only = TRUE)
      packageVersion(pkg) # Check version
      message(sprintf("Successfully loaded %s version %s", pkg, packageVersion(pkg)))
    }, error = function(e) {
      stop(sprintf("Error loading package %s: %s", pkg, e$message))
    })
  }
}

packages <- c("shiny", "shinydashboardPlus", "shinydashboard", "ggplot2", 
              "dplyr", "readxl", "forecast", "randomForest", "DT", 
              "reshape2", "vars", "Metrics", "ggthemes", "plotly", "RColorBrewer", 
              "svars", "glmnet", "e1071", "rpart")

# Load all required packages
check_and_install_packages(packages)

# Source data handling functions
source("data/data_handlers.R")

# Global helper functions
impute_values <- function(x) {
  if(!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  for(i in seq_along(x)) {
    if(is.na(x[i])) {
      x[i] <- if(i == 1) 0 else x[i-1]
    }
  }
  x
}

logMsg <- function(msg) {
  message(sprintf("[%s] - %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
}
