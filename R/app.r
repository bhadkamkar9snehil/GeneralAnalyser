# app.R
# This is the starting point
# Load packages and source files
source("global.r")  # This will handle package loading
source("server.r")
source("ui.r")

# Initialize the Shiny app
shinyApp(ui = ui, server = server)
