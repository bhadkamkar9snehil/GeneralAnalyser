# Clustering Analysis Functions

#' Perform clustering analysis
#' @param data Data frame containing features
#' @param feature_cols Feature column names
#' @param method Clustering method (K-Means, Hierarchical, DBSCAN)
#' @param n_clusters Number of clusters (for K-Means and Hierarchical)
#' @return List containing clustering results and metrics
performClustering <- function(data, feature_cols, method = "K-Means", n_clusters = NULL) {
    # Data preparation
    features <- data[, feature_cols, drop = FALSE]
    features <- scale(features)  # Standardize features
    
    # Handle missing values
    features <- na.omit(features)
    
    # Perform clustering
    result <- switch(method,
        "K-Means" = perform_kmeans(features, n_clusters),
        "Hierarchical" = perform_hierarchical(features, n_clusters),
        "DBSCAN" = perform_dbscan(features),
        stop("Unsupported clustering method")
    )
    
    result$method <- method
    result$feature_cols <- feature_cols
    result
}

# Helper functions for different clustering methods
perform_kmeans <- function(features, n_clusters) {
    if(is.null(n_clusters)) n_clusters <- 3
    
    # Perform k-means clustering
    model <- kmeans(features, centers = n_clusters)
    
    # Calculate silhouette score
    sil <- cluster::silhouette(model$cluster, dist(features))
    avg_sil <- mean(sil[, 3])
    
    # Calculate cluster centers and sizes
    centers <- data.frame(model$centers)
    names(centers) <- colnames(features)
    sizes <- model$size
    
    list(
        model = model,
        clusters = model$cluster,
        centers = centers,
        sizes = sizes,
        silhouette_score = avg_sil,
        within_ss = model$tot.withinss,
        between_ss = model$betweenss
    )
}

perform_hierarchical <- function(features, n_clusters) {
    if(is.null(n_clusters)) n_clusters <- 3
    
    # Calculate distance matrix
    dist_matrix <- dist(features)
    
    # Perform hierarchical clustering
    hc <- hclust(dist_matrix, method = "ward.D2")
    
    # Cut tree to get clusters
    clusters <- cutree(hc, k = n_clusters)
    
    # Calculate silhouette score
    sil <- cluster::silhouette(clusters, dist_matrix)
    avg_sil <- mean(sil[, 3])
    
    # Calculate cluster centers and sizes
    centers <- matrix(NA, nrow = n_clusters, ncol = ncol(features))
    sizes <- numeric(n_clusters)
    for(i in 1:n_clusters) {
        cluster_points <- features[clusters == i, ]
        centers[i,] <- colMeans(cluster_points)
        sizes[i] <- nrow(cluster_points)
    }
    centers <- data.frame(centers)
    names(centers) <- colnames(features)
    
    list(
        model = hc,
        clusters = clusters,
        centers = centers,
        sizes = sizes,
        silhouette_score = avg_sil,
        dendrogram = hc
    )
}

perform_dbscan <- function(features, eps = NULL, minPts = NULL) {
    if(is.null(eps)) {
        # Automatically determine eps using k-nearest neighbors
        kNNdist <- dbscan::kNNdist(features, k = 4)
        eps <- mean(kNNdist) + 2 * sd(kNNdist)
    }
    if(is.null(minPts)) minPts <- ncol(features) + 1
    
    # Perform DBSCAN clustering
    model <- dbscan::dbscan(features, eps = eps, minPts = minPts)
    
    # Calculate silhouette score if there's more than one cluster
    if(length(unique(model$cluster)) > 1) {
        valid_points <- model$cluster > 0
        sil <- cluster::silhouette(model$cluster[valid_points], 
                                 dist(features[valid_points,]))
        avg_sil <- mean(sil[, 3])
    } else {
        avg_sil <- NA
    }
    
    list(
        model = model,
        clusters = model$cluster,
        n_clusters = length(unique(model$cluster[model$cluster > 0])),
        silhouette_score = avg_sil,
        noise_points = sum(model$cluster == 0),
        eps = eps,
        minPts = minPts
    )
}

#' Plot clustering results
#' @param results Results from performClustering
#' @return plotly object
plotClustering <- function(results) {
    if(length(results$feature_cols) < 2) {
        stop("Need at least 2 features for visualization")
    }
    
    # Create scatter plot using first two features
    plot_data <- data.frame(
        Feature1 = features[,1],
        Feature2 = features[,2],
        Cluster = as.factor(results$clusters)
    )
    
    # Create scatter plot
    p <- plot_ly(data = plot_data,
                 x = ~Feature1,
                 y = ~Feature2,
                 color = ~Cluster,
                 type = "scatter",
                 mode = "markers",
                 marker = list(size = 10)) %>%
        layout(title = paste(results$method, "Clustering Results"),
               xaxis = list(title = results$feature_cols[1]),
               yaxis = list(title = results$feature_cols[2]))
    
    # Add cluster centers for K-Means and Hierarchical
    if(results$method %in% c("K-Means", "Hierarchical")) {
        centers <- data.frame(
            Feature1 = results$centers[,1],
            Feature2 = results$centers[,2]
        )
        p <- p %>% add_markers(data = centers,
                              x = ~Feature1,
                              y = ~Feature2,
                              marker = list(size = 15,
                                          symbol = "x",
                                          line = list(width = 2)),
                              name = "Cluster Centers",
                              showlegend = TRUE)
    }
    
    p
}

#' Print clustering summary
#' @param results Results from performClustering
#' @return Text output of clustering metrics
printClusteringSummary <- function(results) {
    cat("Clustering Method:", results$method, "\n")
    
    if(results$method == "DBSCAN") {
        cat("Number of Clusters:", results$n_clusters, "\n")
        cat("Number of Noise Points:", results$noise_points, "\n")
        cat("Eps:", round(results$eps, 4), "\n")
        cat("MinPts:", results$minPts, "\n")
    } else {
        cat("Number of Clusters:", length(results$sizes), "\n")
        cat("Cluster Sizes:\n")
        print(results$sizes)
    }
    
    if(!is.na(results$silhouette_score)) {
        cat("\nSilhouette Score:", round(results$silhouette_score, 4), "\n")
    }
    
    if(results$method == "K-Means") {
        cat("Within-cluster Sum of Squares:", round(results$within_ss, 2), "\n")
        cat("Between-cluster Sum of Squares:", round(results$between_ss, 2), "\n")
    }
}