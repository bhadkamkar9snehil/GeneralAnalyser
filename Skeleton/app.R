# app.R
# This file orchestrates the Shiny app by sourcing global, ui, and server files.

# Source global settings
source("global.R")

# Load UI
ui <- source("ui.R")$value

# Load server logic
server <- source("server.R")$value

# Launch the Shiny app
shinyApp(ui = ui, server = server)
