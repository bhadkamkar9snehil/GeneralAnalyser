# global.R
# Load required packages and set global options

# Package dependencies
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  shiny,
  shinydashboard,
  tidyverse,
  forecast,
  DT,
  plotly,
  readxl
)

# Global settings
options(shiny.maxRequestSize = 30*1024^2)  # Set max file upload size to 30MB
options(warn = -1)  # Suppress warnings in production

# Theme settings
theme_generalanalyser <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      text = element_text(color = "#2c3e50")
    )
}

# Set default theme for all plots
theme_set(theme_generalanalyser())

# Source helper modules
source("data/data_handlers.R", local = TRUE)
source("analysis/time_series.R", local = TRUE)
source("analysis/regression.R", local = TRUE)
source("analysis/classification.R", local = TRUE)
source("analysis/clustering.R", local = TRUE)
source("analysis/feature_engineering.R", local = TRUE)
source("utils/helpers.R", local = TRUE)

# Global helper functions
impute_values <- function(x) {
  if(!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  for(i in seq_along(x)) {
    if(is.na(x[i])) {
      x[i] <- if(i == 1) median(x, na.rm = TRUE) else x[i-1]
    }
  }
  x
}

# Improved logging function
logMsg <- function(msg, level = "INFO") {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message(sprintf("[%s] [%s] - %s", timestamp, level, msg))
}
