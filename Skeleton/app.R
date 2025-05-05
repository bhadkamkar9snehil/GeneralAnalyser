# app.R

# Load required packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(plotly)
library(readxl)
library(dplyr)
library(forecast)

# Define UI --------------------------------------------------------------------
ui <- shinydashboardPlus::dashboardPage(
  header = dashboardHeader(title = "General Analytics Platform"),
  
  # Sidebar: Navigation based solely on the design document.
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
  
  # Main body: Each tabItem corresponds to a child navigation item.
  body = dashboardBody(
    tabItems(
      # Landing Page
      tabItem(tabName = "landing",
              fluidRow(
                box(
                  id = "landingBox",
                  title = "Welcome",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  closable = TRUE,
                  width = 12,
                  "Welcome to the General Analytics Platform. Please sign in or explore our features.",
                  sidebar = boxSidebar(
                    id = "landingSidebar",
                    width = 25,
                    sliderInput("welcomeSlider", "Welcome Slider:", min = 0, max = 100, value = 50)
                  )
                )
              )
      ),
      
      # Data Hub: File Manager
      tabItem(tabName = "fileManager",
              fluidRow(
                box(
                  id = "fileManagerBox",
                  title = "File Manager",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  fileInput("fileManagerInput", "Upload File (CSV/Excel):", 
                            accept = c(".csv", ".xls", ".xlsx")),
                  verbatimTextOutput("fileInfo"),
                  DT::dataTableOutput("filePreview")
                )
              )
      ),
      
      # Data Hub: API Builder (placeholder)
      tabItem(tabName = "apiBuilder",
              fluidRow(
                box(
                  id = "apiBuilderBox",
                  title = "API Builder",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  textInput("apiEndpoint", "API Endpoint:", value = ""),
                  actionButton("testAPI", "Test API")
                )
              )
      ),
      
      # Data Hub: Database Connect (placeholder)
      tabItem(tabName = "dbConnect",
              fluidRow(
                box(
                  id = "dbConnectBox",
                  title = "Database Connect",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  textInput("dbConnection", "Database Connection String:", value = ""),
                  actionButton("connectDB", "Connect")
                )
              )
      ),
      
      # Analysis Center: Data Explorer
      tabItem(tabName = "dataExplorer",
              fluidRow(
                box(
                  id = "dataExplorerBox",
                  title = "Data Explorer",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  DT::dataTableOutput("dataExplorerTable"),
                  actionButton("refreshExplorer", "Refresh Data")
                )
              )
      ),
      
      # Analysis Center: Algorithm Lab
      tabItem(tabName = "algorithmLab",
              fluidRow(
                box(
                  id = "algorithmLabBox",
                  title = "Algorithm Lab",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  selectInput("algorithmSelect", "Select Algorithm:",
                              choices = c("ARIMA", "ETS", "NNETAR", "VAR", "Random Forest")),
                  numericInput("param1", "Parameter 1:", value = 10, min = 1),
                  actionButton("runAlgorithm", "Execute Algorithm")
                )
              )
      ),
      
      # Analysis Center: Results View
      tabItem(tabName = "resultsView",
              fluidRow(
                box(
                  id = "resultsViewBox",
                  title = "Results View",
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  plotlyOutput("resultsPlot", height = "300px"),
                  downloadButton("exportResults", "Export Results")
                )
              ),
              fluidRow(
                box(
                  id = "statsBox",
                  title = "Basic Statistics (Mean & SD)",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotlyOutput("statsPlot", height = "300px")
                ),
                box(
                  id = "trendBox",
                  title = "Trends for Numeric Columns",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  plotlyOutput("trendPlot", height = "300px")
                )
              ),
              fluidRow(
                box(
                  id = "correlationBox",
                  title = "Correlation Heatmap",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  plotlyOutput("corPlot", height = "300px")
                )
              )
      ),
      
      # Model Studio: Model Training
      tabItem(tabName = "modelTraining",
              fluidRow(
                box(
                  id = "modelTrainingBox",
                  title = "Model Training",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  textInput("modelName", "Model Name:", value = ""),
                  actionButton("trainModel", "Train Model")
                )
              )
      ),
      
      # Model Studio: Deployment
      tabItem(tabName = "deployment",
              fluidRow(
                box(
                  id = "deploymentBox",
                  title = "Deployment",
                  status = "success",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  actionButton("deployModel", "Deploy Model"),
                  verbatimTextOutput("deployStatus")
                )
              )
      ),
      
      # Model Studio: Monitoring
      tabItem(tabName = "monitoring",
              fluidRow(
                box(
                  id = "monitoringBox",
                  title = "Monitoring",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  plotlyOutput("monitorPlot", height = "300px"),
                  verbatimTextOutput("monitorStatus")
                )
              )
      ),
      
      # Live Dashboard: Live Metrics
      tabItem(tabName = "liveMetrics",
              fluidRow(
                box(
                  id = "liveMetricsBox",
                  title = "Live Metrics",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  plotlyOutput("liveMetricsPlot", height = "300px")
                )
              )
      ),
      
      # Live Dashboard: Alerts & Notifications
      tabItem(tabName = "alertsNotifications",
              fluidRow(
                box(
                  id = "alertsBox",
                  title = "Alerts & Notifications",
                  status = "danger",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  verbatimTextOutput("alertsOutput")
                )
              )
      ),
      
      # Settings: General Settings
      tabItem(tabName = "generalSettings",
              fluidRow(
                box(
                  id = "generalSettingsBox",
                  title = "General Settings",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  textInput("settingText", "Text Input:", value = ""),
                  selectInput("settingSelect", "Select Option:", 
                              choices = c("Option 1", "Option 2", "Option 3")),
                  dateInput("settingDate", "Select Date:", value = Sys.Date()),
                  fileInput("settingFile", "Upload File:", 
                            accept = c(".csv", ".xls", ".xlsx"))
                ),
                box(
                  id = "advancedSettingsBox",
                  title = "Advanced Form Components",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  textAreaInput("richText", "Rich Text Editor:", 
                                value = "Enter formatted text here...", rows = 4),
                  textAreaInput("codeEditor", "Code Editor:", 
                                value = "# Write your code here", rows = 4),
                  textAreaInput("jsonBuilder", "JSON Builder:", 
                                value = '{"key": "value"}', rows = 3)
                )
              ),
              fluidRow(
                box(
                  id = "interactiveElementsBox",
                  title = "Interactive Elements",
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 12,
                  sliderInput("sliderInput", "Slider Input:", min = 0, max = 100, value = 50),
                  sliderInput("rangeSelector", "Range Selector:", min = 0, max = 100, value = c(25,75)),
                  textInput("colorPicker", "Color Picker (hex code):", value = "#FF0000")
                )
              )
      ),
      
      # Settings: User Profile
      tabItem(tabName = "userProfile",
              fluidRow(
                box(
                  id = "userProfileBox",
                  title = "User Profile",
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  width = 6,
                  textInput("userName", "User Name:", value = ""),
                  passwordInput("userPassword", "Password:"),
                  actionButton("updateProfile", "Update Profile")
                )
              )
      )
    )
  ),
  
  # Control Bar: Contains only appearance controls.
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

# Define Server Logic ----------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive value to store uploaded data
  uploadedData <- reactiveVal(NULL)
  
  # Handle file upload from File Manager
  observeEvent(input$fileManagerInput, {
    req(input$fileManagerInput)
    file <- input$fileManagerInput
    ext <- tools::file_ext(file$name)
    if(ext %in% c("csv")) {
      data <- read.csv(file$datapath, stringsAsFactors = FALSE)
    } else if(ext %in% c("xls", "xlsx")) {
      data <- read_excel(file$datapath)
    } else {
      showNotification("Invalid file type. Please upload CSV or Excel.", type = "error")
      return(NULL)
    }
    uploadedData(data)
  })
  
  # File Manager outputs: Display file info and preview (first 20 rows)
  output$fileInfo <- renderPrint({
    req(input$fileManagerInput)
    file <- input$fileManagerInput
    cat("File Name:", file$name, "\nFile Size:", file$size, "bytes")
  })
  
  output$filePreview <- DT::renderDataTable({
    req(uploadedData())
    head(uploadedData(), 20)
  })
  
  # Data Explorer: Display full dataset as a datatable
  output$dataExplorerTable <- DT::renderDataTable({
    req(uploadedData())
    DT::datatable(uploadedData(), options = list(pageLength = 10))
  })
  
  # Basic Statistics: Compute mean and SD for numeric columns and display as grouped bar chart
  output$statsPlot <- renderPlotly({
    req(uploadedData())
    data <- uploadedData()
    numericData <- data %>% select_if(is.numeric)
    if(ncol(numericData) == 0) return(NULL)
    stats <- data.frame(
      Column = names(numericData),
      Mean = sapply(numericData, mean, na.rm = TRUE),
      SD = sapply(numericData, sd, na.rm = TRUE)
    )
    p <- plot_ly(stats, x = ~Column) %>%
      add_bars(y = ~Mean, name = "Mean", marker = list(color = 'blue')) %>%
      add_bars(y = ~SD, name = "SD", marker = list(color = 'red')) %>%
      layout(barmode = "group", title = "Basic Statistics: Mean & SD")
    p
  })
  
  # Trends: Plot trends for each numeric column (using row index as x-axis)
  output$trendPlot <- renderPlotly({
    req(uploadedData())
    data <- uploadedData()
    numericData <- data %>% select_if(is.numeric)
    if(ncol(numericData) == 0) return(NULL)
    plotList <- lapply(names(numericData), function(col) {
      plot_ly(x = ~1:nrow(numericData), y = numericData[[col]], type = 'scatter', mode = 'lines', name = col)
    })
    subplot(plotList, nrows = ceiling(length(plotList)/2), shareX = TRUE) %>%
      layout(title = "Trends for Numeric Columns")
  })
  
  # Correlation Heatmap: Compute correlation matrix among numeric columns
  output$corPlot <- renderPlotly({
    req(uploadedData())
    data <- uploadedData()
    numericData <- data %>% select_if(is.numeric)
    if(ncol(numericData) < 2) return(NULL)
    corrMat <- cor(numericData, use = "pairwise.complete.obs")
    p <- plot_ly(
      x = colnames(corrMat),
      y = rownames(corrMat),
      z = corrMat,
      type = "heatmap",
      colorscale = "RdBu",
      reversescale = TRUE
    ) %>% layout(title = "Correlation Heatmap")
    p
  })
  
  # Algorithm Lab: Execute forecasting algorithm if ARIMA is selected.
  forecastResult <- reactiveVal(NULL)
  
  observeEvent(input$runAlgorithm, {
    req(uploadedData())
    data <- uploadedData()
    numericData <- data %>% select_if(is.numeric)
    if(ncol(numericData) == 0) {
      showNotification("No numeric columns available for forecasting.", type = "error")
      return(NULL)
    }
    alg <- input$algorithmSelect
    if("ARIMA" %in% alg) {
      tsData <- ts(numericData[[1]])
      fit <- auto.arima(tsData)
      fc <- forecast(fit, h = 10)
      forecastResult(fc)
      showNotification("ARIMA forecast executed.", type = "message")
    } else {
      forecastResult(NULL)
      showNotification("Selected algorithm not implemented. (Placeholder)", type = "warning")
    }
  })
  
  # Results View: Display forecast if available; otherwise, show default plot.
  output$resultsPlot <- renderPlotly({
    fc <- forecastResult()
    if(!is.null(fc)) {
      p <- autoplot(fc)
      ggplotly(p)
    } else {
      req(uploadedData())
      data <- uploadedData()
      numericData <- data %>% select_if(is.numeric)
      if(ncol(numericData) == 0) return(NULL)
      p <- plot_ly(x = ~1:nrow(numericData), y = numericData[[1]], type = "scatter", mode = "lines", name = names(numericData)[1]) %>%
        layout(title = "Default Results Plot")
      p
    }
  })
  
  # Model Studio, Live Dashboard, and Settings placeholders
  output$deployStatus <- renderPrint({ "Model not yet deployed. (Phase 1 placeholder)" })
  output$monitorStatus <- renderPrint({ "No monitoring data available. (Phase 1 placeholder)" })
  
  output$monitorPlot <- renderPlotly({
    plot_ly(x = ~1:10, y = rnorm(10), type = 'scatter', mode = 'lines') %>%
      layout(title = "Monitoring Sample Plot")
  })
  
  output$liveMetricsPlot <- renderPlotly({
    plot_ly(x = ~1:10, y = rnorm(10), type = 'scatter', mode = 'lines') %>%
      layout(title = "Live Metrics Sample Plot")
  })
  
  output$alertsOutput <- renderPrint({ "No alerts. (Phase 1 placeholder)" })
  
  # User Profile update: Notify when profile is updated
  observeEvent(input$updateProfile, {
    showNotification(paste("Profile updated for", input$userName), type = "message")
  })
  
  # Refresh Data Explorer
  observeEvent(input$refreshExplorer, {
    uploadedData(uploadedData())
    showNotification("Data refreshed.", type = "message")
  })
  
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
