library(shiny)
library(ggplot2)
library(forecast)
library(randomForest)
library(reshape2)
library(vars)
library(Metrics)
library(ggthemes)
library(plotly)
library(RColorBrewer)
library(svars)
library(glmnet)
library(e1071)
library(rpart)

#' Main Server Logic
#' 
#' This file contains the main server logic for the GeneralAnalyser application.
#' It coordinates between different modules and handles the application state.

#' @import shiny
#' @import dplyr
NULL

server <- function(input, output, session) {
    # Reactive values for storing state
    values <- reactiveValues(
        data = NULL,
        model = NULL,
        results = NULL
    )
    
    # Data Input Handler
    observeEvent(input$file, {
        req(input$file)
        tryCatch({
            values$data <- load_file_data(input$file)
            showNotification("Data loaded successfully", type = "message")
        }, error = function(e) {
            showNotification(paste("Error loading data:", e$message), type = "error")
        })
    })
    
    # Time Series Analysis Handler
    observeEvent(input$run_ts_analysis, {
        req(values$data, input$ts_method, input$horizon)
        tryCatch({
            values$results <- ts_forecast(
                data = values$data[[input$ts_column]],
                horizon = input$horizon,
                method = input$ts_method
            )
            output$ts_plot <- renderPlot({
                plot(values$results)
            })
        }, error = function(e) {
            showNotification(paste("Error in analysis:", e$message), type = "error")
        })
    })
    
    # Data Preview
    output$data_preview <- DT::renderDataTable({
        req(values$data)
        DT::datatable(values$data,
            options = list(pageLength = 10, scrollX = TRUE)
        )
    })
    
    # Model Summary
    output$model_summary <- renderPrint({
        req(values$results)
        summary(values$results)
    })
    
    # Download Handler
    output$download_results <- downloadHandler(
        filename = function() {
            paste("analysis-results-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(values$results$mean, file, row.names = FALSE)
        }
    )
}