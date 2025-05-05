test_that("clustering analysis works", {
    # Create sample clustering data
    set.seed(123)
    n <- 100
    x1 <- c(rnorm(50, mean = 0), rnorm(50, mean = 3))
    x2 <- c(rnorm(50, mean = 0), rnorm(50, mean = 3))
    data <- data.frame(x1 = x1, x2 = x2)
    
    # Test k-means clustering
    kmeans_result <- perform_clustering(data, method = "kmeans", params = list(k = 2))
    expect_s3_class(kmeans_result, "kmeans")
    expect_equal(length(unique(kmeans_result$cluster)), 2)
    
    # Test hierarchical clustering
    hclust_result <- perform_clustering(data, method = "hierarchical")
    expect_s3_class(hclust_result, "hclust")
    
    # Test DBSCAN clustering
    dbscan_result <- perform_clustering(data, method = "dbscan")
    expect_type(dbscan_result, "list")
    expect_true("cluster" %in% names(dbscan_result))
})

test_that("clustering evaluation works", {
    # Create simple clustered data
    set.seed(123)
    data <- data.frame(
        x = c(rnorm(50, 0, 0.5), rnorm(50, 3, 0.5)),
        y = c(rnorm(50, 0, 0.5), rnorm(50, 3, 0.5))
    )
    
    # Perform and evaluate clustering
    result <- perform_clustering(data, method = "kmeans", params = list(k = 2))
    eval_metrics <- evaluate_clustering(result)
    
    # Check evaluation metrics
    expect_type(eval_metrics, "list")
    expect_true(all(c("within_ss", "between_ss", "total_ss") %in% names(eval_metrics)))
})

test_that("optimal k finding works", {
    # Create data with clear clusters
    set.seed(123)
    data <- rbind(
        matrix(rnorm(100, mean = 0, sd = 0.3), ncol = 2),
        matrix(rnorm(100, mean = 3, sd = 0.3), ncol = 2),
        matrix(rnorm(100, mean = 6, sd = 0.3), ncol = 2)
    )
    data <- as.data.frame(data)
    
    # Find optimal k
    result <- find_optimal_k(data, max_k = 6)
    
    expect_type(result, "list")
    expect_true(all(c("optimal_k", "metrics") %in% names(result)))
    expect_true(result$optimal_k >= 2 && result$optimal_k <= 6)
})

test_that("cluster profiling works", {
    # Create simple clustered data
    set.seed(123)
    data <- data.frame(
        x = c(rnorm(30, 0), rnorm(30, 5)),
        y = c(rnorm(30, 0), rnorm(30, 5))
    )
    
    # Get cluster profiles
    result <- perform_clustering(data, method = "kmeans", params = list(k = 2))
    profiles <- get_cluster_profiles(result)
    
    expect_type(profiles, "list")
    expect_equal(length(profiles), 2)  # Two clusters
    expect_true(all(sapply(profiles, function(p) all(c("mean", "sd") %in% rownames(p)))))
})

test_that("clustering functions handle errors appropriately", {
    # Test non-numeric data
    data <- data.frame(x = letters[1:10], y = 1:10)
    expect_error(perform_clustering(data))
    
    # Test invalid method
    data <- data.frame(x = 1:10, y = 1:10)
    expect_error(perform_clustering(data, method = "invalid"))
    
    # Test invalid parameters
    expect_error(perform_clustering(data, method = "kmeans", params = list(k = 0)))
    
    # Test empty data
    expect_error(perform_clustering(data.frame()))
})