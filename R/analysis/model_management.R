# Model Management Functions

#' Model Registry class for managing trained models
ModelRegistry <- R6::R6Class("ModelRegistry",
    public = list(
        models = list(),
        deployments = list(),
        
        initialize = function() {
            private$monitoring_data <- list()
        },
        
        #' Add a model to the registry
        #' @param model_id Unique identifier for the model
        #' @param model Model object
        #' @param metadata List of model metadata
        add_model = function(model_id, model, metadata) {
            if(model_id %in% names(self$models)) {
                stop("Model ID already exists")
            }
            
            self$models[[model_id]] <- list(
                model = model,
                metadata = metadata,
                created_at = Sys.time(),
                status = "trained",
                version = "1.0.0"
            )
            
            invisible(self)
        },
        
        #' Deploy a model
        #' @param model_id ID of model to deploy
        #' @param deployment_config Deployment configuration
        deploy_model = function(model_id, deployment_config = list()) {
            if(!model_id %in% names(self$models)) {
                stop("Model not found")
            }
            
            deployment_id <- paste0("deploy_", model_id, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
            
            self$deployments[[deployment_id]] <- list(
                model_id = model_id,
                config = deployment_config,
                deployed_at = Sys.time(),
                status = "active",
                metrics = list(
                    performance = 0,
                    health = 100,
                    accuracy = 0,
                    latency = 0
                )
            )
            
            self$models[[model_id]]$status <- "deployed"
            
            invisible(self)
        },
        
        #' Update deployment metrics
        #' @param deployment_id Deployment identifier
        #' @param metrics List of metric updates
        update_metrics = function(deployment_id, metrics) {
            if(!deployment_id %in% names(self$deployments)) {
                stop("Deployment not found")
            }
            
            for(metric_name in names(metrics)) {
                self$deployments[[deployment_id]]$metrics[[metric_name]] <- metrics[[metric_name]]
            }
            
            # Store historical metrics
            timestamp <- Sys.time()
            private$monitoring_data[[deployment_id]][[as.character(timestamp)]] <- metrics
            
            invisible(self)
        },
        
        #' Get model details
        #' @param model_id Model identifier
        get_model = function(model_id) {
            if(!model_id %in% names(self$models)) {
                stop("Model not found")
            }
            self$models[[model_id]]
        },
        
        #' Get deployment details
        #' @param deployment_id Deployment identifier
        get_deployment = function(deployment_id) {
            if(!deployment_id %in% names(self$deployments)) {
                stop("Deployment not found")
            }
            self$deployments[[deployment_id]]
        },
        
        #' List all models
        list_models = function() {
            model_df <- do.call(rbind, lapply(names(self$models), function(id) {
                model <- self$models[[id]]
                data.frame(
                    ModelID = id,
                    Type = model$metadata$type,
                    Method = model$metadata$method,
                    Created = format(model$created_at, "%Y-%m-%d %H:%M:%S"),
                    Status = model$status,
                    Version = model$version,
                    stringsAsFactors = FALSE
                )
            }))
            if(is.null(model_df)) return(data.frame())
            model_df
        },
        
        #' List all deployments
        list_deployments = function() {
            deploy_df <- do.call(rbind, lapply(names(self$deployments), function(id) {
                deploy <- self$deployments[[id]]
                data.frame(
                    DeploymentID = id,
                    ModelID = deploy$model_id,
                    Status = deploy$status,
                    DeployedAt = format(deploy$deployed_at, "%Y-%m-%d %H:%M:%S"),
                    Performance = deploy$metrics$performance,
                    Health = deploy$metrics$health,
                    stringsAsFactors = FALSE
                )
            }))
            if(is.null(deploy_df)) return(data.frame())
            deploy_df
        },
        
        #' Get monitoring data for a deployment
        #' @param deployment_id Deployment identifier
        #' @param metric_name Name of metric to retrieve
        #' @param window Time window in seconds (NULL for all data)
        get_monitoring_data = function(deployment_id, metric_name, window = NULL) {
            if(!deployment_id %in% names(private$monitoring_data)) {
                return(data.frame())
            }
            
            data <- private$monitoring_data[[deployment_id]]
            timestamps <- as.POSIXct(names(data))
            values <- sapply(data, function(x) x[[metric_name]])
            
            if(!is.null(window)) {
                cutoff <- Sys.time() - window
                keep <- timestamps >= cutoff
                timestamps <- timestamps[keep]
                values <- values[keep]
            }
            
            data.frame(
                Timestamp = timestamps,
                Value = values,
                stringsAsFactors = FALSE
            )
        }
    ),
    
    private = list(
        monitoring_data = list()
    )
)

#' Create monitoring plots
#' @param registry ModelRegistry instance
#' @param deployment_id Deployment identifier
#' @param metric_name Metric to plot
#' @param window Time window in seconds
#' @return plotly object
createMonitoringPlot <- function(registry, deployment_id, metric_name, window = 3600) {
    data <- registry$get_monitoring_data(deployment_id, metric_name, window)
    
    if(nrow(data) == 0) {
        return(NULL)
    }
    
    plot_ly(data = data, x = ~Timestamp, y = ~Value, type = "scatter", mode = "lines") %>%
        layout(title = paste("Model", metric_name, "Over Time"),
               xaxis = list(title = "Time"),
               yaxis = list(title = metric_name))
}

#' Calculate aggregate metrics
#' @param registry ModelRegistry instance
#' @return List of aggregate metrics
calculateAggregateMetrics <- function(registry) {
    deployments <- registry$deployments
    
    if(length(deployments) == 0) {
        return(list(
            performance = 0,
            health = 0,
            accuracy = 0,
            latency = 0
        ))
    }
    
    metrics <- lapply(deployments, function(d) d$metrics)
    
    list(
        performance = mean(sapply(metrics, function(m) m$performance)),
        health = mean(sapply(metrics, function(m) m$health)),
        accuracy = mean(sapply(metrics, function(m) m$accuracy)),
        latency = mean(sapply(metrics, function(m) m$latency))
    )
}

#' Generate model explanation
#' @param model Model object
#' @return List containing model explanation
generateModelExplanation <- function(model) {
    if(is.null(model) || is.null(model$results)) {
        return(NULL)
    }
    
    explanation <- list(
        type = model$type,
        method = if(!is.null(model$method)) model$method else paste(model$algorithms, collapse="/")
    )
    
    # Add method-specific explanations
    if(model$type == "time_series") {
        explanation$metrics <- model$results$error_metrics
    } else if(model$type == "regression") {
        if(!is.null(model$results$feature_importance)) {
            explanation$feature_importance <- model$results$feature_importance
        }
        explanation$metrics <- list(
            rmse = model$results$rmse,
            r_squared = model$results$r_squared
        )
    } else if(model$type == "classification") {
        explanation$metrics <- list(
            accuracy = model$results$accuracy,
            confusion_matrix = model$results$confusion_matrix
        )
        if(!is.null(model$results$feature_importance)) {
            explanation$feature_importance <- model$results$feature_importance
        }
    }
    
    explanation
}