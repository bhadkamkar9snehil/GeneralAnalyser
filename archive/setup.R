# Install required packages with error handling
packages <- c(
    "shiny",
    "shinydashboardPlus",
    "shinydashboard",
    "ggplot2",
    "forecast",
    "randomForest",
    "reshape2",
    "vars",
    "Metrics",
    "ggthemes",
    "plotly",
    "RColorBrewer",
    "svars",
    "glmnet",
    "e1071",
    "rpart",
    "DT",
    "readxl",
    "dplyr",
    "shinyWidgets",
    "bslib"
)

install_packages <- function() {
    for(pkg in packages) {
        tryCatch({
            if(!requireNamespace(pkg, quietly = TRUE)) {
                message(sprintf("Installing package: %s", pkg))
                install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
            }
            library(pkg, character.only = TRUE)
            packageVersion(pkg)
            message(sprintf("Successfully loaded %s version %s", pkg, packageVersion(pkg)))
        }, error = function(e) {
            message(sprintf("Error installing/loading package %s: %s", pkg, e$message))
        })
    }
}

install_packages()
