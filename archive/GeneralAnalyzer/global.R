# global.R
# Load required packages and set global options
packages <- c("shiny", "shinydashboardPlus", "shinydashboard", "ggplot2", 
              "dplyr", "readxl", "forecast", "randomForest", "DT", 
              "reshape2", "vars", "Metrics", "ggthemes", "plotly", "RColorBrewer", "svars","glmnet","e1071","rpart")
for(pkg in packages) {
  if(!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "http://cran.us.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

# Global helper functions
impute_values <- function(x) {
  for(i in seq_along(x)) {
    if(is.na(x[i])) {
      x[i] <- if(i == 1) 0 else x[i-1]
    }
  }
  x
}

logMsg <- function(msg) {
  message(paste(Sys.time(), "-", msg))
}
