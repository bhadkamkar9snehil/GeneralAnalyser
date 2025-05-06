library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(plotly)
library(readxl)
library(dplyr)
library(forecast)

server <- function(input, output, session) {
  
  # Reactive value to store uploaded data
  uploadedData <- reactiveVal(NULL)
  
  # Handle file upload from File Manager tab
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
  
  # File Manager outputs: show file info and preview first 20 rows
  output$fileInfo <- renderPrint({
    req(input$fileManagerInput)
    file <- input$fileManagerInput
    cat("File Name:", file$name, "\nFile Size:", file$size, "bytes")
  })
  
  output$filePreview <- DT::renderDataTable({
    req(uploadedData())
    head(uploadedData(), 20)
  })
  
  # Data Explorer: show full dataset as a datatable
  output$dataExplorerTable <- DT::renderDataTable({
    req(uploadedData())
    DT::datatable(uploadedData(), options = list(pageLength = 10))
  })
  
  # Basic Statistics: compute mean and SD for numeric columns and plot as grouped bar chart
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
  
  # Trends: plot line charts for each numeric column (using row index as x-axis)
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
  
  # Correlation Heatmap: compute correlation matrix and display as heatmap
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
  
  # Algorithm Lab: Execute forecasting algorithm if ARIMA is selected
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
  
  # Results View: Display forecast if available; otherwise, show default plot
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
      p <- plot_ly(x = ~1:nrow(numericData), y = numericData[[1]], type = "scatter", mode = "lines",
                   name = names(numericData)[1]) %>%
        layout(title = "Default Results Plot")
      p
    }
  })
  
  # Placeholders for Model Studio, Live Dashboard, and Settings outputs
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
  
  # User Profile update notification
  observeEvent(input$updateProfile, {
    showNotification(paste("Profile updated for", input$userName), type = "message")
  })
  
  # Refresh Data Explorer: reassign data to trigger re-render
  observeEvent(input$refreshExplorer, {
    uploadedData(uploadedData())
    showNotification("Data refreshed.", type = "message")
  })
  
}

shinyApp(ui = ui, server = server)
