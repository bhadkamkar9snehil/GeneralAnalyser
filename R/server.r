library(shiny)
library(tidyverse)
library(forecast)
library(DT)
library(plotly)

#' Main Server Logic for GeneralAnalyser
#' 
#' This file contains the core server-side functionality for the GeneralAnalyser application.
#' It handles basic data loading, analysis, and visualization.
#'
#' Features:
#' - Time series forecasting with basic algorithms (ARIMA, ETS)
#' - Basic interactive visualizations
#' - Support for CSV and Excel file formats
#'
#' @import shiny
#' @import dplyr
#' @import ggplot2
#' @import forecast
#' @importFrom stats ts na.omit
#' @importFrom utils tail
NULL

server <- function(input, output, session) {
  # Reactive data storage
  data <- reactiveVal(NULL)
  model <- reactiveVal(NULL)
  
  # Data Upload Handler
  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    df <- if(ext == "csv") {
      read.csv(input$file$datapath, header = input$header)
    } else {
      readxl::read_excel(input$file$datapath)
    }
    
    data(df)
  })
  
  # Data Preview
  output$dataPreview <- DT::renderDataTable({
    req(data())
    DT::datatable(head(data(), 100), options = list(scrollX = TRUE))
  })
  
  # Dynamic Analysis Options
  output$analysisOptions <- renderUI({
    req(data())
    
    if(input$analysisType == "timeseries") {
      tagList(
        selectInput("timeCol", "Time Column", choices = names(data())),
        selectInput("valueCol", "Value Column", choices = names(data())),
        numericInput("forecastPeriods", "Forecast Periods", value = 12, min = 1)
      )
    } else if(input$analysisType == "regression") {
      tagList(
        selectInput("dependent", "Dependent Variable", choices = names(data())),
        selectInput("independent", "Independent Variables", 
                   choices = names(data()), multiple = TRUE)
      )
    }
  })
  
  # Run Analysis Handler
  observeEvent(input$runAnalysis, {
    req(data())
    
    # Show progress
    withProgress(message = 'Running analysis...', {
      if(input$analysisType == "timeseries") {
        req(input$timeCol, input$valueCol)
        
        incProgress(0.3, detail = "Creating time series")
        # Convert time column to proper format if it's datetime
        time_data <- data()[[input$timeCol]]
        if(inherits(time_data, "POSIXct") || inherits(time_data, "POSIXlt")) {
          time_data <- as.numeric(time_data)
        }
        
        # Ensure value column is numeric
        value_data <- as.numeric(data()[[input$valueCol]])
        if(any(is.na(value_data))) {
          showNotification("Warning: Some values were not numeric and were converted to NA", 
                         type = "warning")
        }
        
        ts_data <- ts(value_data, frequency = 12)
        
        incProgress(0.3, detail = "Fitting model")
        fit <- tryCatch({
          auto.arima(ts_data)
        }, error = function(e) {
          showNotification(paste("Error fitting model:", e$message), type = "error")
          NULL
        })
        
        if(!is.null(fit)) {
          incProgress(0.2, detail = "Generating forecast")
          forecast_result <- forecast(fit, h = input$forecastPeriods)
          
          model(list(
            type = "timeseries",
            fit = fit,
            forecast = forecast_result,
            actual = ts_data,
            time = time_data
          ))
        }
        
      } else if(input$analysisType == "regression") {
        req(input$dependent, input$independent)
        
        incProgress(0.3, detail = "Preparing data")
        # Ensure numeric data for regression
        dep_data <- as.numeric(data()[[input$dependent]])
        if(any(is.na(dep_data))) {
          showNotification("Warning: Some dependent values were not numeric", type = "warning")
        }
        
        # Clean column names for formula
        clean_names <- make.names(c(input$dependent, input$independent))
        temp_data <- data()
        names(temp_data)[match(c(input$dependent, input$independent), names(temp_data))] <- clean_names
        
        # Convert all independent variables to numeric
        for(var in clean_names[-1]) {
          temp_data[[var]] <- as.numeric(temp_data[[var]])
        }
        
        formula <- as.formula(paste(
          clean_names[1], "~",
          paste(clean_names[-1], collapse = " + ")
        ))
        
        incProgress(0.4, detail = "Fitting model")
        fit <- tryCatch({
          lm(formula, data = temp_data)
        }, error = function(e) {
          showNotification(paste("Error fitting model:", e$message), type = "error")
          NULL
        })
        
        if(!is.null(fit)) {
          model(list(
            type = "regression",
            fit = fit,
            actual = dep_data,
            predicted = fitted(fit)
          ))
        }
      }
      
      # Switch to results tab after analysis
      updateTabItems(session, "tabs", selected = "results")
    })
  })
  
  # Results Plot
  output$resultPlot <- renderPlotly({
    req(model())
    
    if(model()$type == "timeseries") {
      if(!is.null(model()$forecast)) {
        autoplot(model()$forecast) %>% ggplotly()
      }
    } else if(model()$type == "regression") {
      if(!is.null(model()$fit)) {
        plot_ly(data = data()) %>%
          add_trace(
            x = data()[[input$independent[1]]],
            y = model()$actual,
            type = "scatter",
            mode = "markers",
            name = "Actual"
          ) %>%
          add_trace(
            x = data()[[input$independent[1]]],
            y = model()$predicted,
            type = "scatter",
            mode = "lines",
            name = "Predicted"
          )
      }
    }
  })
  
  # Results Summary
  output$resultSummary <- renderPrint({
    req(model())
    if(!is.null(model()$fit)) {
      summary(model()$fit)
    }
  })
  
  # Model Metrics
  output$modelMetrics <- DT::renderDataTable({
    req(model())
    
    if(model()$type == "timeseries") {
      if(!is.null(model()$forecast)) {
        # Get the fitted values (in-sample predictions)
        fitted_values <- fitted(model()$fit)
        actual_values <- as.numeric(model()$actual)
        
        # Calculate accuracy metrics manually
        metrics <- data.frame(
          Metric = c("RMSE", "MAE", "MAPE"),
          Value = c(
            sqrt(mean((actual_values - fitted_values)^2)),  # RMSE
            mean(abs(actual_values - fitted_values)),       # MAE
            mean(abs((actual_values - fitted_values) / actual_values)) * 100  # MAPE
          )
        )
        
        DT::datatable(
          metrics,
          options = list(
            pageLength = 5,
            dom = 't'
          )
        )
      }
    } else if(model()$type == "regression") {
      # Calculate regression metrics safely
      actual <- as.numeric(model()$actual)
      predicted <- as.numeric(model()$predicted)
      
      # Remove any NA values
      valid_idx <- !is.na(actual) & !is.na(predicted)
      actual <- actual[valid_idx]
      predicted <- predicted[valid_idx]
      
      # Calculate metrics
      metrics <- data.frame(
        Metric = c(
          "R-squared",
          "Adj R-squared",
          "RMSE",
          "MAE",
          "MAPE"
        ),
        Value = c(
          summary(model()$fit)$r.squared,
          summary(model()$fit)$adj.r.squared,
          sqrt(mean((actual - predicted)^2)),
          mean(abs(actual - predicted)),
          mean(abs((actual - predicted) / actual)) * 100
        )
      )
      
      DT::datatable(
        metrics,
        options = list(
          pageLength = 5,
          dom = 't'
        )
      )
    }
  })
}