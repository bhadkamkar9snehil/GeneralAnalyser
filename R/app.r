# app.R
# This is the starting point

# Create analysis directory if it doesn't exist
dir.create("analysis", showWarnings = FALSE)

# Set working directory to the script's location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load packages and source files
source("global.r")  # This will handle package loading
source("clustering.R")  # Move clustering.R to R directory
source("server.r")
source("ui.r")

# Initialize the Shiny app
shinyApp(ui = ui, server = server)
