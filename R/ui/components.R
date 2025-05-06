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

# Create loading overlay for boxes
create_loading_overlay <- function(id) {
    div(
        class = "loading-overlay",
        id = ns(id),
        style = "display: none;",
        div(
            class = "loading-spinner",
            tags$i(class = "fa fa-spinner fa-spin"),
            "Loading..."
        )
    )
}

# Create error message for boxes
create_error_message <- function(id) {
    div(
        class = "error-message",
        id = ns(id),
        style = "display: none;",
        icon("exclamation-triangle"),
        tags$span(class = "error-text")
    )
}

# Enhanced box creation with loading and error states
create_analysis_box <- function(id, title, status = "primary", ...) {
    ns <- NS(id)
    
    box(
        title = title,
        status = status,
        solidHeader = TRUE,
        collapsible = TRUE,
        width = NULL,
        div(
            class = "box-content",
            create_loading_overlay(ns("loading")),
            create_error_message(ns("error")),
            ...
        )
    )
}

# UI Components

#' Create monitoring dashboard
#' @param ns Namespace function for module
monitoringDashboardUI <- function(ns) {
    fluidPage(
        fluidRow(
            column(3, 
                valueBoxOutput(ns("modelPerformance"), width = 12),
                valueBoxOutput(ns("modelHealth"), width = 12)
            ),
            column(3,
                valueBoxOutput(ns("modelAccuracy"), width = 12),
                valueBoxOutput(ns("modelLatency"), width = 12)
            ),
            column(6,
                box(
                    width = 12,
                    title = "Model Performance Over Time",
                    plotlyOutput(ns("performanceTrend"))
                )
            )
        ),
        fluidRow(
            column(6,
                box(
                    width = 12,
                    title = "Active Models",
                    DTOutput(ns("activeModels"))
                )
            ),
            column(6,
                box(
                    width = 12,
                    title = "Model Alerts",
                    DTOutput(ns("modelAlerts"))
                )
            )
        ),
        fluidRow(
            column(12,
                tabBox(
                    width = 12,
                    tabPanel("Performance Metrics",
                        plotlyOutput(ns("metricsPlot"))
                    ),
                    tabPanel("System Health",
                        plotlyOutput(ns("healthPlot"))
                    ),
                    tabPanel("Resource Usage",
                        plotlyOutput(ns("resourcePlot"))
                    )
                )
            )
        )
    )
}

#' Create analysis settings panel
#' @param ns Namespace function for module
analysisSettingsUI <- function(ns) {
    fluidRow(
        column(4,
            box(
                width = 12,
                title = "Data Settings",
                fileInput(ns("file"), "Upload Data File",
                         accept = c(".csv", ".xlsx")),
                selectInput(ns("time_col"), "Time Column", choices = NULL),
                selectInput(ns("target_col"), "Target Column", choices = NULL),
                numericInput(ns("split_ratio"), "Train/Test Split Ratio",
                           value = 0.8, min = 0.5, max = 0.9, step = 0.1)
            )
        ),
        column(4,
            box(
                width = 12,
                title = "Model Settings",
                selectInput(ns("analysis_type"), "Analysis Type",
                          choices = c("Time Series", "Regression",
                                    "Classification", "Clustering",
                                    "Anomaly Detection")),
                conditionalPanel(
                    condition = sprintf("input['%s'] == 'Time Series'", ns("analysis_type")),
                    checkboxGroupInput(ns("algorithms"), "Algorithms",
                                     choices = c("ARIMA", "ETS", "Prophet",
                                               "NNETAR", "VAR")),
                    numericInput(ns("future_periods"), "Forecast Periods",
                               value = 12, min = 1)
                ),
                conditionalPanel(
                    condition = sprintf("input['%s'] == 'Regression'", ns("analysis_type")),
                    selectInput(ns("reg_type"), "Regression Type",
                              choices = c("Linear", "Ridge", "Lasso",
                                        "Random Forest", "XGBoost")),
                    selectInput(ns("reg_target"), "Target Variable",
                              choices = NULL),
                    selectizeInput(ns("reg_features"), "Features",
                                 choices = NULL, multiple = TRUE)
                ),
                conditionalPanel(
                    condition = sprintf("input['%s'] == 'Classification'", ns("analysis_type")),
                    selectInput(ns("class_method"), "Classification Method",
                              choices = c("Random Forest", "SVM",
                                        "Decision Tree", "XGBoost")),
                    selectInput(ns("class_target"), "Target Variable",
                              choices = NULL),
                    selectizeInput(ns("class_features"), "Features",
                                 choices = NULL, multiple = TRUE)
                ),
                conditionalPanel(
                    condition = sprintf("input['%s'] == 'Clustering'", ns("analysis_type")),
                    selectInput(ns("cluster_method"), "Clustering Method",
                              choices = c("K-Means", "Hierarchical", "DBSCAN")),
                    conditionalPanel(
                        condition = sprintf("input['%s'] != 'DBSCAN'", ns("cluster_method")),
                        numericInput(ns("n_clusters"), "Number of Clusters",
                                   value = 3, min = 2)
                    ),
                    selectizeInput(ns("cluster_features"), "Features",
                                 choices = NULL, multiple = TRUE)
                ),
                conditionalPanel(
                    condition = sprintf("input['%s'] == 'Anomaly Detection'", ns("analysis_type")),
                    selectInput(ns("anom_method"), "Detection Method",
                              choices = c("Statistical (3-sigma)", "IQR",
                                        "Isolation Forest")),
                    selectInput(ns("anom_target"), "Target Variable",
                              choices = NULL)
                )
            )
        ),
        column(4,
            box(
                width = 12,
                title = "Advanced Settings",
                checkboxInput(ns("feature_engineering"), "Enable Feature Engineering",
                            value = TRUE),
                conditionalPanel(
                    condition = sprintf("input['%s'] == true", ns("feature_engineering")),
                    checkboxGroupInput(ns("feature_transformations"),
                                     "Feature Transformations",
                                     choices = c("Polynomial", "Interaction",
                                               "Lag", "Rolling Statistics"))
                ),
                checkboxInput(ns("handle_missing"), "Handle Missing Values",
                            value = TRUE),
                checkboxInput(ns("handle_outliers"), "Handle Outliers",
                            value = TRUE),
                selectInput(ns("validation_method"), "Validation Method",
                          choices = c("Cross-Validation", "Hold-out",
                                    "Time Series CV")),
                numericInput(ns("cv_folds"), "Number of CV Folds",
                           value = 5, min = 2)
            )
        )
    )
}

#' Create results display panel
#' @param ns Namespace function for module
resultsDisplayUI <- function(ns) {
    fluidRow(
        column(12,
            tabBox(
                width = 12,
                tabPanel("Model Performance",
                    fluidRow(
                        column(6,
                            plotlyOutput(ns("performancePlot"))
                        ),
                        column(6,
                            DTOutput(ns("metricsTable"))
                        )
                    )
                ),
                tabPanel("Model Diagnostics",
                    uiOutput(ns("diagnosticsUI"))
                ),
                tabPanel("Feature Importance",
                    plotlyOutput(ns("featureImportance"))
                ),
                tabPanel("Model Explanation",
                    verbatimTextOutput(ns("modelExplanation"))
                )
            )
        )
    )
}

#' Create model management panel
#' @param ns Namespace function for module
modelManagementUI <- function(ns) {
    fluidRow(
        column(12,
            tabBox(
                width = 12,
                tabPanel("Model Registry",
                    DTOutput(ns("modelsList"))
                ),
                tabPanel("Deployments",
                    DTOutput(ns("deploymentsList")),
                    actionButton(ns("deployModel"), "Deploy Selected Model",
                               class = "btn-primary")
                ),
                tabPanel("Model Monitoring",
                    monitoringDashboardUI(ns)
                )
            )
        )
    )
}

#' Create data quality report panel
#' @param ns Namespace function for module
dataQualityUI <- function(ns) {
    fluidRow(
        column(6,
            box(
                width = 12,
                title = "Missing Values Analysis",
                plotlyOutput(ns("missingPlot")),
                DTOutput(ns("missingTable"))
            )
        ),
        column(6,
            box(
                width = 12,
                title = "Outlier Analysis",
                plotlyOutput(ns("outlierPlot")),
                DTOutput(ns("outlierTable"))
            )
        ),
        column(12,
            box(
                width = 12,
                title = "Data Distribution",
                plotlyOutput(ns("distributionPlot"))
            )
        )
    )
}

#' Create feature engineering panel
#' @param ns Namespace function for module
featureEngineeringUI <- function(ns) {
    fluidRow(
        column(4,
            box(
                width = 12,
                title = "Feature Transformations",
                checkboxGroupInput(ns("transformations"), "Select Transformations",
                                 choices = c("Log", "Square Root", "Box-Cox",
                                           "Standardization", "Min-Max Scaling")),
                selectizeInput(ns("transform_cols"), "Columns to Transform",
                             choices = NULL, multiple = TRUE),
                actionButton(ns("applyTransform"), "Apply Transformations",
                           class = "btn-primary")
            )
        ),
        column(4,
            box(
                width = 12,
                title = "Feature Generation",
                checkboxGroupInput(ns("feature_types"), "Feature Types",
                                 choices = c("Polynomial", "Interaction",
                                           "Time-based", "Domain-specific")),
                numericInput(ns("poly_degree"), "Polynomial Degree",
                           value = 2, min = 2),
                actionButton(ns("generateFeatures"), "Generate Features",
                           class = "btn-primary")
            )
        ),
        column(4,
            box(
                width = 12,
                title = "Feature Selection",
                selectInput(ns("selection_method"), "Selection Method",
                          choices = c("Correlation", "Mutual Information",
                                    "LASSO", "Random Forest")),
                numericInput(ns("n_features"), "Number of Features",
                           value = 10, min = 1),
                actionButton(ns("selectFeatures"), "Select Features",
                           class = "btn-primary")
            )
        )
    )
}

#' Create download report panel
#' @param ns Namespace function for module
downloadReportUI <- function(ns) {
    fluidRow(
        column(12,
            box(
                width = 12,
                title = "Generate Report",
                checkboxGroupInput(ns("report_sections"), "Report Sections",
                                 choices = c("Data Quality", "Model Performance",
                                           "Feature Importance",
                                           "Model Diagnostics")),
                selectInput(ns("report_format"), "Report Format",
                          choices = c("HTML", "PDF", "Word")),
                downloadButton(ns("downloadReport"), "Generate Report",
                             class = "btn-primary")
            )
        )
    )
}