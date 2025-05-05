#' UI Components Module
#' 
#' This module contains reusable UI components for the dashboard interface.

#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
NULL

#' Create sidebar menu with analysis options
#' @param current_tab Currently selected tab
#' @return Sidebar menu UI element
create_sidebar_menu <- function(current_tab = NULL) {
    sidebarMenu(
        id = "main_menu",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Data", tabName = "data", icon = icon("database"),
                menuSubItem("Import", tabName = "data_import"),
                menuSubItem("Explore", tabName = "data_explore"),
                menuSubItem("Preprocess", tabName = "data_preprocess")
        ),
        menuItem("Analysis", tabName = "analysis", icon = icon("chart-line"),
                menuSubItem("Time Series", tabName = "time_series"),
                menuSubItem("Regression", tabName = "regression"),
                menuSubItem("Classification", tabName = "classification"),
                menuSubItem("Clustering", tabName = "clustering"),
                menuSubItem("Anomaly Detection", tabName = "anomaly")
        ),
        menuItem("Results", tabName = "results", icon = icon("chart-bar")),
        menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
}

#' Create data input UI panel
#' @param id Namespace ID
#' @return UI element for data input
create_data_input_ui <- function(id) {
    ns <- NS(id)
    
    box(
        title = "Data Input",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        
        fileInput(ns("file"), "Choose File",
                 multiple = FALSE,
                 accept = c(
                     "text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv"
                 )),
        
        checkboxInput(ns("header"), "Header", TRUE),
        
        radioButtons(ns("sep"), "Separator",
                    choices = c(
                        Comma = ",",
                        Semicolon = ";",
                        Tab = "\t"
                    ),
                    selected = ","),
        
        radioButtons(ns("quote"), "Quote",
                    choices = c(
                        None = "",
                        "Double Quote" = '"',
                        "Single Quote" = "'"
                    ),
                    selected = '"')
    )
}

#' Create analysis options panel
#' @param id Namespace ID
#' @param analysis_type Type of analysis
#' @return UI element for analysis options
create_analysis_options <- function(id, analysis_type) {
    ns <- NS(id)
    
    options <- switch(analysis_type,
        time_series = list(
            selectInput(ns("ts_method"), "Method",
                       choices = c("Auto ARIMA", "ETS", "Prophet")),
            numericInput(ns("horizon"), "Forecast Horizon", 12, min = 1),
            checkboxInput(ns("include_ci"), "Include Confidence Intervals", TRUE)
        ),
        regression = list(
            selectInput(ns("reg_method"), "Method",
                       choices = c("Linear", "Polynomial", "GAM")),
            selectInput(ns("target"), "Target Variable", choices = NULL),
            selectInput(ns("features"), "Features",
                       choices = NULL, multiple = TRUE)
        ),
        classification = list(
            selectInput(ns("clf_method"), "Method",
                       choices = c("Random Forest", "SVM", "Decision Tree")),
            selectInput(ns("target"), "Target Variable", choices = NULL),
            selectInput(ns("features"), "Features",
                       choices = NULL, multiple = TRUE),
            numericInput(ns("train_ratio"), "Training Ratio",
                        0.8, min = 0.5, max = 0.9)
        ),
        clustering = list(
            selectInput(ns("clust_method"), "Method",
                       choices = c("K-means", "Hierarchical", "DBSCAN")),
            conditionalPanel(
                condition = sprintf("input['%s'] == 'K-means'", ns("clust_method")),
                numericInput(ns("n_clusters"), "Number of Clusters",
                           3, min = 2)
            )
        ),
        anomaly = list(
            selectInput(ns("anom_method"), "Method",
                       choices = c("IQR", "Z-score", "Isolation Forest")),
            numericInput(ns("threshold"), "Threshold",
                        0.95, min = 0.8, max = 0.99)
        )
    )
    
    box(
        title = "Analysis Options",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        options
    )
}

#' Create analysis results panel
#' @param id Namespace ID
#' @param results Analysis results list
#' @return UI element for displaying results
create_results_panel <- function(id, results = NULL) {
    ns <- NS(id)
    
    tabBox(
        title = "Analysis Results",
        width = NULL,
        tabPanel("Plot",
                 plotlyOutput(ns("result_plot")),
                 downloadButton(ns("download_plot"), "Download Plot")),
        tabPanel("Metrics",
                 create_metrics_dashboard(results$metrics)),
        tabPanel("Table",
                 DTOutput(ns("result_table")),
                 downloadButton(ns("download_data"), "Download Data"))
    )
}

#' Create notification toast
#' @param type Notification type (success, warning, error)
#' @param message Notification message
#' @return UI element for notification
create_notification <- function(type, message) {
    showNotification(
        message,
        type = type,
        duration = ifelse(type == "error", NULL, 5)
    )
}

#' Create progress tracking UI
#' @param id Namespace ID
#' @return UI element for progress tracking
create_progress_ui <- function(id) {
    ns <- NS(id)
    
    div(
        class = "progress-container",
        style = "position: relative; min-height: 100px;",
        
        # Progress bar
        progressBar(
            ns("progress"),
            value = 0,
            total = 100,
            title = "Analysis Progress",
            display_pct = TRUE
        ),
        
        # Status message
        textOutput(ns("status_message")),
        
        # Cancel button
        actionButton(ns("cancel"), "Cancel",
                    class = "btn-danger",
                    style = "position: absolute; right: 10px; top: 10px;")
    )
}

#' Create settings panel
#' @param id Namespace ID
#' @return UI element for settings
create_settings_panel <- function(id) {
    ns <- NS(id)
    
    box(
        title = "Settings",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        
        # Theme settings
        selectInput(ns("theme"), "Theme",
                   choices = c("Default", "Dark", "Light")),
        
        # Plot settings
        numericInput(ns("plot_height"), "Plot Height",
                    400, min = 200, max = 800),
        
        # Table settings
        numericInput(ns("page_length"), "Rows per Page",
                    10, min = 5, max = 100),
        
        # Export settings
        selectInput(ns("export_format"), "Export Format",
                   choices = c("CSV", "Excel", "PDF")),
        
        # Save settings button
        actionButton(ns("save_settings"), "Save Settings",
                    class = "btn-primary btn-block")
    )
}

#' Create help tooltip
#' @param id Element ID
#' @param text Help text
#' @return UI element for tooltip
create_help_tooltip <- function(id, text) {
    span(
        style = "margin-left: 5px;",
        icon("question-circle"),
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        title = text
    )
}

#' Create confirmation dialog
#' @param id Namespace ID
#' @param title Dialog title
#' @param message Dialog message
#' @return UI element for confirmation dialog
create_confirmation_dialog <- function(id, title, message) {
    ns <- NS(id)
    
    modalDialog(
        title = title,
        message,
        footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm"), "Confirm",
                        class = "btn-primary")
        ),
        easyClose = TRUE
    )
}

#' Create error message
#' @param message Error message
#' @param details Additional error details
#' @return UI element for error message
create_error_message <- function(message, details = NULL) {
    div(
        class = "error-message",
        style = "padding: 15px; margin-bottom: 20px; border: 1px solid #ebccd1; border-radius: 4px; background-color: #f2dede; color: #a94442;",
        
        h4(icon("exclamation-triangle"), message),
        if (!is.null(details)) {
            div(
                style = "margin-top: 10px; font-size: 0.9em;",
                details
            )
        }
    )
}