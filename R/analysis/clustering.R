#' Clustering Analysis Functions
#' 
#' This module contains functions for various clustering methods
#' including k-means, hierarchical, and DBSCAN clustering.

#' @import cluster
#' @import factoextra
NULL

#' Perform clustering analysis
#' @param data Data frame of features to cluster
#' @param method Clustering method (kmeans, hierarchical, dbscan)
#' @param params List of method-specific parameters
#' @return List containing clustering results
perform_clustering <- function(data, method = "kmeans", params = list()) {
    # Ensure numeric data
    if (!all(sapply(data, is.numeric))) {
        stop("All features must be numeric for clustering")
    }
    
    # Scale data
    scaled_data <- scale(data)
    
    # Perform clustering based on method
    result <- switch(method,
        kmeans = {
            k <- params$k %||% 3
            kmeans(scaled_data, centers = k)
        },
        hierarchical = {
            dist_matrix <- dist(scaled_data)
            hclust(dist_matrix, method = params$linkage %||% "complete")
        },
        dbscan = {
            eps <- params$eps %||% 0.5
            minPts <- params$minPts %||% 5
            dbscan::dbscan(scaled_data, eps = eps, minPts = minPts)
        },
        stop("Unsupported clustering method")
    )
    
    # Add scaled data and original data to results
    attr(result, "scaled_data") <- scaled_data
    attr(result, "original_data") <- data
    
    return(result)
}

#' Evaluate clustering quality
#' @param clustering_result Clustering result object
#' @return List of clustering quality metrics
evaluate_clustering <- function(clustering_result) {
    scaled_data <- attr(clustering_result, "scaled_data")
    
    if (inherits(clustering_result, "kmeans")) {
        # For k-means
        metrics <- list(
            within_ss = clustering_result$tot.withinss,
            between_ss = clustering_result$betweenss,
            total_ss = clustering_result$totss,
            silhouette = cluster::silhouette(clustering_result$cluster, 
                                           dist(scaled_data))
        )
    } else if (inherits(clustering_result, "hclust")) {
        # For hierarchical clustering
        metrics <- list(
            cophenetic = cophenetic(clustering_result),
            silhouette = cluster::silhouette(cutree(clustering_result, k = 3), 
                                           dist(scaled_data))
        )
    } else if (inherits(clustering_result, "dbscan")) {
        # For DBSCAN
        metrics <- list(
            n_clusters = length(unique(clustering_result$cluster[clustering_result$cluster > 0])),
            n_noise = sum(clustering_result$cluster == 0)
        )
    }
    
    return(metrics)
}

#' Find optimal number of clusters
#' @param data Data frame of features
#' @param max_k Maximum number of clusters to consider
#' @return List containing optimal k and evaluation metrics
find_optimal_k <- function(data, max_k = 10) {
    scaled_data <- scale(data)
    
    # Calculate metrics for different k values
    metrics <- lapply(2:max_k, function(k) {
        km <- kmeans(scaled_data, centers = k)
        list(
            k = k,
            wss = km$tot.withinss,
            silhouette = mean(cluster::silhouette(km$cluster, dist(scaled_data))[,3])
        )
    })
    
    # Convert to data frame
    results <- do.call(rbind, lapply(metrics, as.data.frame))
    
    # Find optimal k using elbow method
    wss_diff <- diff(results$wss)
    elbow_k <- which.min(abs(diff(wss_diff))) + 2
    
    return(list(
        optimal_k = elbow_k,
        metrics = results
    ))
}

#' Extract cluster profiles
#' @param clustering_result Clustering result object
#' @param data Original data
#' @return Data frame of cluster characteristics
get_cluster_profiles <- function(clustering_result) {
    data <- attr(clustering_result, "original_data")
    
    if (inherits(clustering_result, "kmeans")) {
        clusters <- clustering_result$cluster
    } else if (inherits(clustering_result, "hclust")) {
        clusters <- cutree(clustering_result, k = 3)
    } else {
        clusters <- clustering_result$cluster
    }
    
    # Calculate summary statistics for each cluster
    profiles <- lapply(split(data, clusters), function(cluster_data) {
        sapply(cluster_data, function(x) {
            c(mean = mean(x), sd = sd(x))
        })
    })
    
    return(profiles)
}