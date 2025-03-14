# ui.R

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(plotly)

ui <- shinydashboardPlus::dashboardPage(
  
  # Header: Simple title as specified in the design doc.
  header = dashboardHeader(title = "General Analytics Platform"),
  
  # Sidebar: Navigation based entirely on the design document.
  # Each primary menu item is split into child items corresponding to individual components.
  sidebar = dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Landing", tabName = "landing", icon = icon("home")),
      
      menuItem("Data Hub", icon = icon("database"),
               menuSubItem("File Manager", tabName = "fileManager"),
               menuSubItem("API Builder", tabName = "apiBuilder"),
               menuSubItem("Database Connect", tabName = "dbConnect")
      ),
      
      menuItem("Analysis Center", icon = icon("chart-line"),
               menuSubItem("Data Explorer", tabName = "dataExplorer"),
               menuSubItem("Algorithm Lab", tabName = "algorithmLab"),
               menuSubItem("Results View", tabName = "resultsView")
      ),
      
      menuItem("Model Studio", icon = icon("cogs"),
               menuSubItem("Model Training", tabName = "modelTraining"),
               menuSubItem("Deployment", tabName = "deployment"),
               menuSubItem("Monitoring", tabName = "monitoring")
      ),
      
      menuItem("Live Dashboard", icon = icon("tachometer-alt"),
               menuSubItem("Live Metrics", tabName = "liveMetrics"),
               menuSubItem("Alerts & Notifications", tabName = "alertsNotifications")
      ),
      
      menuItem("Settings", icon = icon("sliders-h"),
               menuSubItem("General Settings", tabName = "generalSettings"),
               menuSubItem("User Profile", tabName = "userProfile")
      )
    )
  ),
  
  # Main body: Each tabItem corresponds to a child item from the sidebar.
  body = dashboardBody(
    tabItems(
      # Landing Page: Welcome / Authentication overview.
      tabItem(tabName = "landing",
              fluidRow(
                box(
                  title = "Welcome",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  "Welcome to the General Analytics Platform. Please sign in or explore our features."
                )
              )
      ),
      
      # Data Hub components.
      tabItem(tabName = "fileManager",
              fluidRow(
                box(
                  title = "File Manager",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  fileInput("fileManagerInput", "Upload File:", accept = c(".csv", ".xls", ".xlsx")),
                  actionButton("browseFiles", "Browse Files"),
                  verbatimTextOutput("fileManagerHistory")
                )
              )
      ),
      
      tabItem(tabName = "apiBuilder",
              fluidRow(
                box(
                  title = "API Builder",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  textInput("apiEndpoint", "API Endpoint:", value = ""),
                  actionButton("testAPI", "Test API")
                )
              )
      ),
      
      tabItem(tabName = "dbConnect",
              fluidRow(
                box(
                  title = "Database Connect",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  textInput("dbConnection", "Database Connection String:", value = ""),
                  actionButton("connectDB", "Connect")
                )
              )
      ),
      
      # Analysis Center components.
      tabItem(tabName = "dataExplorer",
              fluidRow(
                box(
                  title = "Data Explorer",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  DT::dataTableOutput("dataExplorerTable"),
                  actionButton("transformData", "Transform Data")
                )
              )
      ),
      
      tabItem(tabName = "algorithmLab",
              fluidRow(
                box(
                  title = "Algorithm Lab",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  selectInput("algorithmSelect", "Select Algorithm:",
                              choices = c("ARIMA", "ETS", "NNETAR", "VAR", "Random Forest")),
                  numericInput("param1", "Parameter 1:", value = 10, min = 1),
                  actionButton("runAlgorithm", "Execute Algorithm")
                )
              )
      ),
      
      tabItem(tabName = "resultsView",
              fluidRow(
                box(
                  title = "Results View",
                  width = 12,
                  status = "danger",
                  solidHeader = TRUE,
                  plotlyOutput("resultsPlot", height = "300px"),
                  downloadButton("exportResults", "Export Results")
                )
              )
      ),
      
      # Model Studio components.
      tabItem(tabName = "modelTraining",
              fluidRow(
                box(
                  title = "Model Training",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  textInput("modelName", "Model Name:", value = ""),
                  actionButton("trainModel", "Train Model")
                )
              )
      ),
      
      tabItem(tabName = "deployment",
              fluidRow(
                box(
                  title = "Deployment",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  actionButton("deployModel", "Deploy Model"),
                  verbatimTextOutput("deployStatus")
                )
              )
      ),
      
      tabItem(tabName = "monitoring",
              fluidRow(
                box(
                  title = "Monitoring",
                  width = 12,
                  status = "warning",
                  solidHeader = TRUE,
                  plotlyOutput("monitorPlot", height = "300px"),
                  verbatimTextOutput("monitorStatus")
                )
              )
      ),
      
      # Live Dashboard components.
      tabItem(tabName = "liveMetrics",
              fluidRow(
                box(
                  title = "Live Metrics",
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  plotlyOutput("liveMetricsPlot", height = "300px")
                )
              )
      ),
      
      tabItem(tabName = "alertsNotifications",
              fluidRow(
                box(
                  title = "Alerts & Notifications",
                  width = 12,
                  status = "danger",
                  solidHeader = TRUE,
                  verbatimTextOutput("alertsOutput")
                )
              )
      ),
      
      # Settings components.
      tabItem(tabName = "generalSettings",
              fluidRow(
                box(
                  title = "General Settings",
                  width = 6,
                  status = "primary",
                  solidHeader = TRUE,
                  textInput("settingText", "Text Input:", value = ""),
                  selectInput("settingSelect", "Select Option:", choices = c("Option 1", "Option 2", "Option 3")),
                  dateInput("settingDate", "Select Date:", value = Sys.Date()),
                  fileInput("settingFile", "Upload File:", accept = c(".csv", ".xls", ".xlsx"))
                )
              )
      ),
      
      tabItem(tabName = "userProfile",
              fluidRow(
                box(
                  title = "User Profile",
                  width = 6,
                  status = "info",
                  solidHeader = TRUE,
                  textInput("userName", "User Name:", value = ""),
                  passwordInput("userPassword", "Password:"),
                  actionButton("updateProfile", "Update Profile")
                )
              )
      )
    )
  ),
  
  # Control Bar: Now contains only the appearance control.
  controlbar = dashboardControlbar(
    id = "controlbar",
    disable = FALSE,
    width = 300,
    collapsed = TRUE,
    overlay = TRUE,
    skin = "light",
    .list = list(
      controlbarItem(
        title = "Appearance",
        skinSelector()
      )
    )
  ),
  
  title = "General Analytics Platform",
  skin = "black-light"
)

shinyUI(ui)
