# General Forecasting Tool - R Implementation
# Based on requirements from provided documentation

# --------------------
# Required Libraries
# --------------------

# Install necessary packages if not already installed
required_packages <- c("shiny", "shinydashboard", "shinyWidgets", "shinythemes", 
                       "tidyverse", "forecast", "plotly", "DT", "readxl", "jsonlite",
                       "xml2", "httr", "openxlsx", "randomForest", "e1071", "dbscan",
                       "cluster", "AnomalyDetection", "caret", "bslib", "waiter", "rpart")

# Function to check and install missing packages
install_missing <- function(packages) {
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(missing_packages) > 0) {
    install.packages(missing_packages)
  }
}

# Install missing packages
install_missing(required_packages)

# Load libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(forecast)
library(plotly)
library(DT)
library(readxl)
library(jsonlite)
library(xml2)
library(httr)
library(openxlsx)
library(randomForest)
library(e1071)
library(dbscan)
library(cluster)
library(AnomalyDetection)
library(caret)
library(bslib)
library(waiter)
library(rpart)

# --------------------
# Helper Functions
# --------------------

# Function to read various file types
read_data <- function(file_path, file_type) {
  tryCatch({
    if (file_type == "csv") {
      data <- read.csv(file_path, stringsAsFactors = FALSE)
    } else if (file_type == "excel") {
      data <- read_excel(file_path)
      data <- as.data.frame(data)
    } else if (file_type == "json") {
      data <- fromJSON(file_path)
      if (is.list(data) && !is.data.frame(data)) {
        data <- as.data.frame(do.call(cbind, data))
      }
    } else if (file_type == "xml") {
      xml_data <- read_xml(file_path)
      data <- xml_to_df(xml_data)
    } else {
      data <- NULL
    }
    return(data)
  }, error = function(e) {
    return(NULL)
  })
}

# Function to convert XML to dataframe (simplified)
xml_to_df <- function(xml_data) {
  nodes <- xml_find_all(xml_data, "//record")
  if (length(nodes) == 0) {
    nodes <- xml_find_all(xml_data, "//row")
  }
  
  if (length(nodes) == 0) {
    return(data.frame())
  }
  
  results <- lapply(nodes, function(x) {
    child_nodes <- xml_children(x)
    values <- xml_text(child_nodes)
    names <- xml_name(child_nodes)
    result <- as.list(values)
    names(result) <- names
    return(result)
  })
  
  return(do.call(rbind.data.frame, results))
}

# Function to perform time series forecasting
perform_forecast <- function(data, target_col, date_col, forecast_horizon, method = "auto") {
  # Ensure data is properly prepared
  if (!date_col %in% colnames(data) || !target_col %in% colnames(data)) {
    return(list(success = FALSE, message = "Column names not found in dataset"))
  }
  
  # Convert date column if needed
  if (!inherits(data[[date_col]], "Date")) {
    data[[date_col]] <- as.Date(data[[date_col]])
  }
  
  # Sort data by date
  data <- data[order(data[[date_col]]), ]
  
  # Create time series object
  ts_data <- ts(data[[target_col]], frequency = determine_frequency(data[[date_col]]))
  
  # Apply forecasting based on method
  if (method == "auto") {
    model <- auto.arima(ts_data)
  } else if (method == "ets") {
    model <- ets(ts_data)
  } else if (method == "naive") {
    model <- naive(ts_data, h = forecast_horizon)
    return(list(
      success = TRUE,
      forecast = model,
      model_type = "naive",
      original_data = data.frame(date = data[[date_col]], value = data[[target_col]])
    ))
  }
  
  # Generate forecast
  forecast_result <- forecast(model, h = forecast_horizon)
  
  # Create date sequence for forecast period
  last_date <- max(data[[date_col]])
  freq <- determine_frequency(data[[date_col]])
  
  if (freq == 12) { # Monthly data
    forecast_dates <- seq.Date(last_date, by = "month", length.out = forecast_horizon + 1)[-1]
  } else if (freq == 4) { # Quarterly data
    forecast_dates <- seq.Date(last_date, by = "quarter", length.out = forecast_horizon + 1)[-1]
  } else if (freq == 52 || freq == 53) { # Weekly data
    forecast_dates <- seq.Date(last_date, by = "week", length.out = forecast_horizon + 1)[-1]
  } else { # Daily data or other
    forecast_dates <- seq.Date(last_date, by = "day", length.out = forecast_horizon + 1)[-1]
  }
  
  # Return results
  return(list(
    success = TRUE,
    forecast = forecast_result,
    model = model,
    model_type = if(method == "auto") "ARIMA" else method,
    original_data = data.frame(date = data[[date_col]], value = data[[target_col]]),
    forecast_dates = forecast_dates
  ))
}

# Function to determine the frequency of time series data
determine_frequency <- function(dates) {
  if (length(dates) <= 1) return(1)
  
  # Calculate differences between consecutive dates
  diffs <- diff(as.numeric(dates))
  avg_diff <- mean(diffs)
  
  # Determine frequency based on average difference
  if (avg_diff >= 28 && avg_diff <= 31) {
    return(12)  # Monthly
  } else if (avg_diff >= 89 && avg_diff <= 92) {
    return(4)   # Quarterly
  } else if (avg_diff >= 6 && avg_diff <= 8) {
    return(52)  # Weekly
  } else {
    return(365) # Daily
  }
}

# Function to perform regression analysis
perform_regression <- function(data, target_col, feature_cols, regression_type = "linear") {
  # Input validation
  if (!target_col %in% colnames(data)) {
    return(list(success = FALSE, message = "Target column not found"))
  }
  
  for (col in feature_cols) {
    if (!col %in% colnames(data)) {
      return(list(success = FALSE, message = paste("Feature column not found:", col)))
    }
  }
  
  # Prepare formula
  formula_str <- paste(target_col, "~", paste(feature_cols, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Create train/test split
  set.seed(123)
  sample_size <- floor(0.8 * nrow(data))
  train_indices <- sample(seq_len(nrow(data)), size = sample_size)
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Fit model based on regression type
  if (regression_type == "linear") {
    model <- lm(formula_obj, data = train_data)
    predictions <- predict(model, newdata = test_data)
    
    # Calculate metrics
    rmse <- sqrt(mean((predictions - test_data[[target_col]])^2))
    r_squared <- summary(model)$r.squared
    
    return(list(
      success = TRUE,
      model = model,
      model_type = "linear_regression",
      rmse = rmse,
      r_squared = r_squared,
      predictions = predictions,
      actual = test_data[[target_col]],
      formula = formula_str,
      coefficients = coef(model),
      summary = summary(model)
    ))
  } else if (regression_type == "polynomial") {
    # For simplicity, using polynomial with degree 2
    poly_formula <- as.formula(paste(target_col, "~ poly(", feature_cols[1], ", 2)"))
    model <- lm(poly_formula, data = train_data)
    predictions <- predict(model, newdata = test_data)
    
    # Calculate metrics
    rmse <- sqrt(mean((predictions - test_data[[target_col]])^2))
    r_squared <- summary(model)$r.squared
    
    return(list(
      success = TRUE,
      model = model,
      model_type = "polynomial_regression",
      rmse = rmse,
      r_squared = r_squared,
      predictions = predictions,
      actual = test_data[[target_col]],
      formula = poly_formula,
      summary = summary(model)
    ))
  } else if (regression_type == "logistic") {
    # Check if target is binary
    if (length(unique(data[[target_col]])) > 2) {
      return(list(success = FALSE, message = "Logistic regression requires a binary target variable"))
    }
    
    # Convert target to factor if needed
    train_data[[target_col]] <- as.factor(train_data[[target_col]])
    test_data[[target_col]] <- as.factor(test_data[[target_col]])
    
    model <- glm(formula_obj, data = train_data, family = "binomial")
    predictions_prob <- predict(model, newdata = test_data, type = "response")
    predictions <- ifelse(predictions_prob > 0.5, 1, 0)
    
    # Create confusion matrix
    conf_matrix <- table(Predicted = predictions, Actual = test_data[[target_col]])
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    return(list(
      success = TRUE,
      model = model,
      model_type = "logistic_regression",
      accuracy = accuracy,
      predictions = predictions,
      predictions_prob = predictions_prob,
      actual = test_data[[target_col]],
      formula = formula_str,
      confusion_matrix = conf_matrix,
      summary = summary(model)
    ))
  }
}

# Function to perform classification
perform_classification <- function(data, target_col, feature_cols, method = "random_forest") {
  # Input validation
  if (!target_col %in% colnames(data)) {
    return(list(success = FALSE, message = "Target column not found"))
  }
  
  for (col in feature_cols) {
    if (!col %in% colnames(data)) {
      return(list(success = FALSE, message = paste("Feature column not found:", col)))
    }
  }
  
  # Convert target to factor
  data[[target_col]] <- as.factor(data[[target_col]])
  
  # Create train/test split
  set.seed(123)
  sample_size <- floor(0.8 * nrow(data))
  train_indices <- sample(seq_len(nrow(data)), size = sample_size)
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  
  # Prepare formula
  formula_str <- paste(target_col, "~", paste(feature_cols, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Fit model based on classification method
  if (method == "random_forest") {
    model <- randomForest(formula_obj, data = train_data)
    predictions <- predict(model, newdata = test_data)
    
    # Calculate metrics
    conf_matrix <- table(Predicted = predictions, Actual = test_data[[target_col]])
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    return(list(
      success = TRUE,
      model = model,
      model_type = "random_forest",
      accuracy = accuracy,
      predictions = predictions,
      actual = test_data[[target_col]],
      formula = formula_str,
      confusion_matrix = conf_matrix,
      importance = importance(model)
    ))
  } else if (method == "svm") {
    model <- svm(formula_obj, data = train_data)
    predictions <- predict(model, newdata = test_data)
    
    # Calculate metrics
    conf_matrix <- table(Predicted = predictions, Actual = test_data[[target_col]])
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    return(list(
      success = TRUE,
      model = model,
      model_type = "svm",
      accuracy = accuracy,
      predictions = predictions,
      actual = test_data[[target_col]],
      formula = formula_str,
      confusion_matrix = conf_matrix
    ))
  } else if (method == "decision_tree") {
    model <- rpart::rpart(formula_obj, data = train_data)
    predictions <- predict(model, newdata = test_data, type = "class")
    
    # Calculate metrics
    conf_matrix <- table(Predicted = predictions, Actual = test_data[[target_col]])
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    return(list(
      success = TRUE,
      model = model,
      model_type = "decision_tree",
      accuracy = accuracy,
      predictions = predictions,
      actual = test_data[[target_col]],
      formula = formula_str,
      confusion_matrix = conf_matrix,
      variable_importance = model$variable.importance
    ))
  }
}

# Function to perform clustering
perform_clustering <- function(data, feature_cols, method = "kmeans", n_clusters = 3) {
  # Input validation
  for (col in feature_cols) {
    if (!col %in% colnames(data)) {
      return(list(success = FALSE, message = paste("Feature column not found:", col)))
    }
  }
  
  # Extract features
  features <- data[, feature_cols, drop = FALSE]
  
  # Scale features
  features_scaled <- scale(features)
  
  # Perform clustering based on method
  if (method == "kmeans") {
    # Determine optimal number of clusters using elbow method if not specified
    if (n_clusters <= 0) {
      wss <- sapply(1:10, function(k) {
        kmeans(features_scaled, k, nstart=10)$tot.withinss
      })
      n_clusters <- which(diff(diff(wss)) > 0)[1] + 1
      if (is.na(n_clusters) || n_clusters < 2) n_clusters <- 3
    }
    
    model_type = "kmeans",
      n_clusters = n_clusters,
      silhouette_score = avg_sil,
      results = results,
      cluster_centers = model$centers
    ))
  } else if (method == "hierarchical") {
    # Compute distance matrix
    dist_matrix <- dist(features_scaled)
    
    # Fit model
    model <- hclust(dist_matrix, method = "ward.D2")
    
    # Cut the tree to get cluster assignments
    clusters <- cutree(model, k = n_clusters)
    
    # Add cluster assignments to original data
    results <- data
    results$cluster <- as.factor(clusters)
    
    # Calculate silhouette score
    sil <- silhouette(clusters, dist_matrix)
    avg_sil <- mean(sil[,3])
    
    return(list(
      success = TRUE,
      model = model,
      model_type = "hierarchical",
      n_clusters = n_clusters,
      silhouette_score = avg_sil,
      results = results,
      dendrogram = model
    ))
  } else if (method == "dbscan") {
    # Fit model
    model <- dbscan::dbscan(features_scaled, eps = 0.5, minPts = 5)
    
    # Add cluster assignments to original data
    results <- data
    results$cluster <- as.factor(model$cluster)
    
    # Calculate silhouette score if there's more than one cluster
    if (length(unique(model$cluster)) > 1 && min(model$cluster) >= 0) {
      valid_points <- model$cluster > 0
      if (sum(valid_points) > 1) {
        sil <- silhouette(model$cluster[valid_points], dist(features_scaled[valid_points,]))
        avg_sil <- mean(sil[,3])
      } else {
        avg_sil <- NA
      }
    } else {
      avg_sil <- NA
    }
    
    return(list(
      success = TRUE,
      model = model,
      model_type = "dbscan",
      results = results,
      n_clusters = length(unique(model$cluster[model$cluster != 0])),
      silhouette_score = avg_sil,
      noise_points = sum(model$cluster == 0)
    ))
  }
}

# Function to perform anomaly detection
perform_anomaly_detection <- function(data, target_col, date_col = NULL, method = "statistical") {
  # Input validation
  if (!target_col %in% colnames(data)) {
    return(list(success = FALSE, message = "Target column not found"))
  }
  
  if (method == "statistical") {
    # Calculate mean and standard deviation
    mean_val <- mean(data[[target_col]], na.rm = TRUE)
    sd_val <- sd(data[[target_col]], na.rm = TRUE)
    
    # Identify anomalies (values outside 3 standard deviations)
    threshold <- 3
    upper_bound <- mean_val + threshold * sd_val
    lower_bound <- mean_val - threshold * sd_val
    
    anomalies <- data[[target_col]] > upper_bound | data[[target_col]] < lower_bound
    
    # Create result dataframe
    result <- data
    result$is_anomaly <- anomalies
    
    return(list(
      success = TRUE,
      model_type = "statistical",
      anomalies = result[anomalies, ],
      anomaly_indices = which(anomalies),
      anomaly_count = sum(anomalies),
      upper_bound = upper_bound,
      lower_bound = lower_bound,
      mean = mean_val,
      sd = sd_val,
      all_data = result
    ))
  } else if (method == "iqr") {
    # Calculate IQR
    q1 <- quantile(data[[target_col]], 0.25, na.rm = TRUE)
    q3 <- quantile(data[[target_col]], 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    
    # Identify anomalies (values outside 1.5 * IQR)
    threshold <- 1.5
    upper_bound <- q3 + threshold * iqr
    lower_bound <- q1 - threshold * iqr
    
    anomalies <- data[[target_col]] > upper_bound | data[[target_col]] < lower_bound
    
    # Create result dataframe
    result <- data
    result$is_anomaly <- anomalies
    
    return(list(
      success = TRUE,
      model_type = "iqr",
      anomalies = result[anomalies, ],
      anomaly_indices = which(anomalies),
      anomaly_count = sum(anomalies),
      upper_bound = upper_bound,
      lower_bound = lower_bound,
      q1 = q1,
      q3 = q3,
      iqr = iqr,
      all_data = result
    ))
  } else if (method == "twitter") {
    # Check if date column is available
    if (is.null(date_col) || !date_col %in% colnames(data)) {
      return(list(success = FALSE, message = "Date column required for Twitter anomaly detection method"))
    }
    
    # Prepare data for Twitter's anomaly detection
    ts_data <- data.frame(
      timestamp = data[[date_col]],
      value = data[[target_col]]
    )
    
    # Ensure timestamps are in proper format
    if (!inherits(ts_data$timestamp, "Date")) {
      ts_data$timestamp <- as.Date(ts_data$timestamp)
    }
    
    # Sort by timestamp
    ts_data <- ts_data[order(ts_data$timestamp), ]
    
    # Perform anomaly detection
    result <- AnomalyDetectionTs(ts_data, max_anoms = 0.05, direction = "both")
    
    # Mark anomalies in original data
    anomaly_dates <- result$anoms$timestamp
    result_data <- data
    result_data$is_anomaly <- data[[date_col]] %in% anomaly_dates
    
    return(list(
      success = TRUE,
      model_type = "twitter",
      anomalies = result$anoms,
      anomaly_count = nrow(result$anoms),
      all_data = result_data,
      plot_data = result$plot
    ))
  } else if (method == "isolation_forest") {
    # Isolation forest requires multiple columns for best results
    if (ncol(data) <= 1) {
      return(list(success = FALSE, message = "Isolation forest works best with multiple features"))
    }
    
    # Use all numeric columns as features
    numeric_cols <- sapply(data, is.numeric)
    if (sum(numeric_cols) <= 1) {
      return(list(success = FALSE, message = "Not enough numeric features for isolation forest"))
    }
    
    features <- data[, numeric_cols, drop = FALSE]
    
    # Remove NA values
    features <- na.omit(features)
    
    # Scale features
    features_scaled <- scale(features)
    
    # Create isolation forest model
    set.seed(123)
    # Note: R doesn't have a standard isolation forest package
    # Using a simplified approach based on randomForest
    # This is just a placeholder and not a true isolation forest implementation
    model <- randomForest(features_scaled, ntree = 100, mtry = ncol(features_scaled))
    
    # Calculate anomaly scores (using proximity as a proxy)
    prox <- proximity(model)
    anomaly_scores <- 1 - rowMeans(prox)
    
    # Identify anomalies (top 5%)
    threshold <- quantile(anomaly_scores, 0.95)
    anomalies <- anomaly_scores > threshold
    
    # Create result dataframe
    result <- data[rownames(features), ]
    result$anomaly_score <- anomaly_scores
    result$is_anomaly <- anomalies
    
    return(list(
      success = TRUE,
      model_type = "isolation_forest",
      anomalies = result[anomalies, ],
      anomaly_indices = which(anomalies),
      anomaly_count = sum(anomalies),
      threshold = threshold,
      all_data = result
    ))
  }
}

# --------------------
# UI Function
# --------------------

ui <- page_sidebar(
  title = "General Forecasting Tool",
  theme = bs_theme(bootswatch = "flatly"),
  
  # Use waiter for loading screens
  use_waiter(),
  
  # Sidebar panel
  sidebar = sidebar(
    accordion(
      accordion_panel(
        title = "Data Input",
        selectInput("data_source", "Data Source", choices = c("Upload File" = "file", "Sample Data" = "sample")),
        conditionalPanel(
          condition = "input.data_source == 'file'",
          fileInput("file_upload", "Upload Data File", 
                    accept = c(".csv", ".xls", ".xlsx", ".json", ".xml")),
          selectInput("file_type", "File Type", 
                      choices = c("CSV" = "csv", "Excel" = "excel", "JSON" = "json", "XML" = "xml"))
        ),
        conditionalPanel(
          condition = "input.data_source == 'sample'",
          selectInput("sample_dataset", "Sample Dataset", 
                      choices = c("AirPassengers" = "air", "Economics" = "economics", "Stocks" = "stocks"))
        )
      ),
      
      accordion_panel(
        title = "Analysis Type",
        selectInput("analysis_type", "Select Analysis Type", 
                    choices = c("Data Exploration" = "exploration",
                                "Time Series Forecasting" = "forecast",
                                "Regression Analysis" = "regression",
                                "Classification" = "classification",
                                "Clustering" = "clustering",
                                "Anomaly Detection" = "anomaly"))
      ),
      
      # Dynamic controls based on analysis type
      conditionalPanel(
        condition = "input.analysis_type == 'forecast'",
        selectInput("forecast_date_col", "Date Column", choices = NULL),
        selectInput("forecast_target_col", "Target Column", choices = NULL),
        numericInput("forecast_horizon", "Forecast Horizon", value = 12, min = 1, max = 100),
        selectInput("forecast_method", "Forecasting Method", 
                    choices = c("Auto ARIMA" = "auto", "ETS" = "ets", "Naive" = "naive"))
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'regression'",
        selectInput("regression_target_col", "Target Column", choices = NULL),
        selectizeInput("regression_features", "Select Features", choices = NULL, multiple = TRUE),
        selectInput("regression_type", "Regression Type", 
                    choices = c("Linear" = "linear", "Polynomial" = "polynomial", "Logistic" = "logistic"))
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'classification'",
        selectInput("classification_target_col", "Target Column", choices = NULL),
        selectizeInput("classification_features", "Select Features", choices = NULL, multiple = TRUE),
        selectInput("classification_method", "Classification Method", 
                    choices = c("Random Forest" = "random_forest", "SVM" = "svm", "Decision Tree" = "decision_tree"))
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'clustering'",
        selectizeInput("clustering_features", "Select Features", choices = NULL, multiple = TRUE),
        selectInput("clustering_method", "Clustering Method", 
                    choices = c("K-Means" = "kmeans", "Hierarchical" = "hierarchical", "DBSCAN" = "dbscan")),
        conditionalPanel(
          condition = "input.clustering_method != 'dbscan'",
          numericInput("n_clusters", "Number of Clusters", value = 3, min = 2, max = 20)
        )
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'anomaly'",
        selectInput("anomaly_target_col", "Target Column", choices = NULL),
        selectInput("anomaly_date_col", "Date Column (Optional)", choices = NULL),
        selectInput("anomaly_method", "Detection Method", 
                    choices = c("Statistical (3-sigma)" = "statistical", 
                                "IQR Method" = "iqr",
                                "Twitter AnomalyDetection" = "twitter",
                                "Isolation Forest" = "isolation_forest"))
      )
    ),
    
    conditionalPanel(
      condition = "input.analysis_type != 'exploration'",
      actionButton("run_analysis", "Run Analysis", class = "btn-primary btn-lg w-100")
    )
  ),
  
  # Main panel
  card(
    full_screen = TRUE,
    card_header("Results"),
    
    navset_card_tab(
      nav_panel("Data Preview", 
                dataTableOutput("data_preview")
      ),
      
      nav_panel("Data Summary", 
                verbatimTextOutput("data_summary"),
                plotOutput("data_distributions")
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'forecast'",
        nav_panel("Forecast Results",
                  plotlyOutput("forecast_plot"),
                  dataTableOutput("forecast_table"),
                  verbatimTextOutput("forecast_metrics")
        )
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'regression'",
        nav_panel("Regression Results",
                  plotlyOutput("regression_plot"),
                  verbatimTextOutput("regression_summary"),
                  dataTableOutput("regression_predictions")
        )
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'classification'",
        nav_panel("Classification Results",
                  plotOutput("classification_plot"),
                  verbatimTextOutput("classification_summary"),
                  plotOutput("confusion_matrix")
        )
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'clustering'",
        nav_panel("Clustering Results",
                  plotlyOutput("clustering_plot"),
                  verbatimTextOutput("clustering_summary"),
                  dataTableOutput("clustering_results")
        )
      ),
      
      conditionalPanel(
        condition = "input.analysis_type == 'anomaly'",
        nav_panel("Anomaly Detection",
                  plotlyOutput("anomaly_plot"),
                  verbatimTextOutput("anomaly_summary"),
                  dataTableOutput("anomaly_table")
        )
      ),
      
      nav_panel("Export", 
                selectInput("export_format", "Export Format", 
                            choices = c("CSV", "Excel", "JSON", "PDF")),
                downloadButton("download_results", "Download Results")
      )
    )
  )
)

# Server Function
server <- function(input, output, session) {
  # Data input handling
  data <- reactive({
    req(input$file_upload)
    file_type <- input$file_type
    read_data(input$file_upload$datapath, file_type)
  })

  # Data preview
  output$data_preview <- renderDataTable({
    data()
  })

  # Data summary
  output$data_summary <- renderPrint({  
    summary(data())
  })

  # Data distributions
  output$data_distributions <- renderPlot({
    data() %>%  
      pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
      ggplot(aes(x = value)) +
      geom_histogram(binwidth = 0.5) +
      facet_wrap(~ variable, scales = "free") +
      theme_minimal()
  })  

  # Forecast results
  output$forecast_plot <- renderPlotly({
    req(input$run_analysis)
    forecast_result <- perform_forecast(data(), input$forecast_target_col, input$forecast_date_col, input$forecast_horizon, input$forecast_method)
    if (forecast_result$success) {
      plot_ly(forecast_result$forecast, x = ~forecast_dates, y = ~forecast, type = "scatter", mode = "lines") %>%
        add_lines(x = ~forecast_dates, y = ~forecast, name = "Forecast") %>%
        add_lines(x = ~forecast_dates, y = ~data[[input$forecast_target_col]], name = "Actual") %>%
        layout(title = "Forecast vs Actual", xaxis = list(title = "Date"), yaxis = list(title = input$forecast_target_col))
    } else {    
      plot_ly() %>%
        layout(title = "Forecast Failed", xaxis = list(title = "Date"), yaxis = list(title = input$forecast_target_col))
    }
  })  
  
  # Forecast table
  output$forecast_table <- renderDataTable({
    req(input$run_analysis)
    forecast_result <- perform_forecast(data(), input$forecast_target_col, input$forecast_date_col, input$forecast_horizon, input$forecast_method)
    if (forecast_result$success) {
      forecast_result$forecast
    } else {
      data.frame()
    }
  })

  # Forecast metrics
  output$forecast_metrics <- renderPrint({
    req(input$run_analysis)
    forecast_result <- perform_forecast(data(), input$forecast_target_col, input$forecast_date_col, input$forecast_horizon, input$forecast_method)
    if (forecast_result$success) {
      cat("Forecasting Method:", forecast_result$model_type, "\n")
      cat("RMSE:", forecast_result$rmse, "\n")
      cat("R-squared:", forecast_result$r_squared, "\n")
    } else {
      cat("Forecasting failed\n")
    }
  })

  # Regression results
  output$regression_plot <- renderPlotly({
    req(input$run_analysis)
    regression_result <- perform_regression(data(), input$regression_target_col, input$regression_features, input$regression_type)
    if (regression_result$success) {
      plot_ly(regression_result$predictions, x = ~date, y = ~predictions, type = "scatter", mode = "lines") %>% 
        add_lines(x = ~date, y = ~predictions, name = "Predictions") %>%
        add_lines(x = ~date, y = ~actual, name = "Actual") %>%
        layout(title = "Regression Results", xaxis = list(title = "Date"), yaxis = list(title = input$regression_target_col))
    } else {
      plot_ly() %>%
        layout(title = "Regression Failed", xaxis = list(title = "Date"), yaxis = list(title = input$regression_target_col))
    }
  })

  # Regression summary  
  output$regression_summary <- renderPrint({
    req(input$run_analysis)
    regression_result <- perform_regression(data(), input$regression_target_col, input$regression_features, input$regression_type)
    if (regression_result$success) {
      cat("Regression Type:", regression_result$model_type, "\n") 
      cat("RMSE:", regression_result$rmse, "\n")
      cat("R-squared:", regression_result$r_squared, "\n")
    } else {
      cat("Regression failed\n")
    }
  })

  # Regression predictions
  output$regression_predictions <- renderDataTable({
    req(input$run_analysis)
    regression_result <- perform_regression(data(), input$regression_target_col, input$regression_features, input$regression_type)
    if (regression_result$success) {
      regression_result$predictions 
    } else {
      data.frame()
    }
  })

  # Classification results
  output$classification_plot <- renderPlot({
    req(input$run_analysis)
    classification_result <- perform_classification(data(), input$classification_target_col, input$classification_features, input$classification_method)
    if (classification_result$success) {
      plot(classification_result$model, data = data(), main = "Classification Results")
    } else {  
      plot(data.frame())
    }
  })

  # Classification summary
  output$classification_summary <- renderPrint({  
    req(input$run_analysis)
    classification_result <- perform_classification(data(), input$classification_target_col, input$classification_features, input$classification_method)
    if (classification_result$success) {
      cat("Classification Method:", classification_result$model_type, "\n")
      cat("Accuracy:", classification_result$accuracy, "\n")
    } else {
      cat("Classification failed\n")
    }
  })

  # Classification confusion matrix
  output$confusion_matrix <- renderPlot({ 
    req(input$run_analysis)
    classification_result <- perform_classification(data(), input$classification_target_col, input$classification_features, input$classification_method)
    if (classification_result$success) {
      plot(classification_result$confusion_matrix, main = "Confusion Matrix")
    } else {
      plot(data.frame())  
    }
  })

  # Clustering results
  output$clustering_plot <- renderPlotly({
    req(input$run_analysis) 
    clustering_result <- perform_clustering(data(), input$clustering_features, input$clustering_method, input$n_clusters)
    if (clustering_result$success) {
      plot_ly(clustering_result$all_data, x = ~feature1, y = ~feature2, color = ~cluster, type = "scatter") %>%
        layout(title = "Clustering Results")
    } else {
      plot_ly() %>%
        layout(title = "Clustering Failed")
    }
  })

  # Clustering summary
  output$clustering_summary <- renderPrint({  
    req(input$run_analysis)
    clustering_result <- perform_clustering(data(), input$clustering_features, input$clustering_method, input$n_clusters)
    if (clustering_result$success) {
      cat("Clustering Method:", clustering_result$model_type, "\n")
      cat("Number of Clusters:", clustering_result$n_clusters, "\n")
    } else {  
      cat("Clustering failed\n")
    }
  })

  # Anomaly detection results
  output$anomaly_plot <- renderPlotly({ 
    req(input$run_analysis)
    anomaly_result <- perform_anomaly_detection(data(), input$anomaly_target_col, input$anomaly_date_col, input$anomaly_method)
    if (anomaly_result$success) {
      plot_ly(anomaly_result$all_data, x = ~date, y = ~value, type = "scatter", mode = "lines") %>%
        add_lines(x = ~date, y = ~value, name = "Data") %>%
        add_lines(x = ~date, y = ~threshold, name = "Threshold") %>%  
        layout(title = "Anomaly Detection Results")
    } else {
      plot_ly() %>%
        layout(title = "Anomaly Detection Failed")
    }
  })  

  # Anomaly detection summary
  output$anomaly_summary <- renderPrint({
    req(input$run_analysis)
    anomaly_result <- perform_anomaly_detection(data(), input$anomaly_target_col, input$anomaly_date_col, input$anomaly_method)
    if (anomaly_result$success) {
      cat("Anomaly Detection Method:", anomaly_result$model_type, "\n") 
      cat("Number of Anomalies:", anomaly_result$n_anomalies, "\n")
    } else {
      cat("Anomaly detection failed\n")
    }
  })

  # Anomaly detection table 
  output$anomaly_table <- renderDataTable({
    req(input$run_analysis)
    anomaly_result <- perform_anomaly_detection(data(), input$anomaly_target_col, input$anomaly_date_col, input$anomaly_method)
    if (anomaly_result$success) {
      anomaly_result$all_data
    } else {  
      data.frame()
    }
  })

  # Export results
  output$download_results <- downloadHandler( 
    filename = function() {
      paste("results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    } 
  )
}

# Run the Shiny app
shinyApp(ui, server)









