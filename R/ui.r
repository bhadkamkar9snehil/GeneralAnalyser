library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(
    title = "General Analyser"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Data Upload", tabName = "data_upload", icon = icon("upload")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Results", tabName = "results", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      # Data Upload Tab
      tabItem(tabName = "data_upload",
        fluidRow(
          box(
            title = "Upload Data",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            fileInput("file", "Choose CSV or Excel File",
                     accept = c(".csv", ".xlsx", ".xls")
            ),
            checkboxInput("header", "File has header", TRUE)
          )
        ),
        fluidRow(
          box(
            title = "Data Preview",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            DT::dataTableOutput("dataPreview")
          )
        )
      ),
      
      # Analysis Tab
      tabItem(tabName = "analysis",
        fluidRow(
          box(
            title = "Analysis Settings",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            selectInput("analysisType", "Select Analysis Type",
                       choices = c(
                         "Time Series Analysis" = "timeseries",
                         "Regression Analysis" = "regression"
                       )
            ),
            uiOutput("analysisOptions"),
            hr(),
            div(
              class = "text-center",
              actionButton("runAnalysis", "Run Analysis",
                         class = "btn-lg btn-primary",
                         icon = icon("play"))
            )
          )
        )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
        fluidRow(
          box(
            title = "Analysis Results",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            plotlyOutput("resultPlot"),
            verbatimTextOutput("resultSummary")
          )
        ),
        fluidRow(
          box(
            title = "Model Metrics",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            DT::dataTableOutput("modelMetrics")
          )
        )
      )
    )
  )
)