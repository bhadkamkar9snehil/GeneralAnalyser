test_that("time series plot creation works", {
    # Create sample time series data
    data <- data.frame(
        time = seq_len(100),
        value = sin(seq_len(100)/10) + rnorm(100, sd = 0.1)
    )
    
    # Test static plot
    static_plot <- create_ts_plot(data, interactive = FALSE)
    expect_s3_class(static_plot, "gg")
    
    # Test interactive plot
    interactive_plot <- create_ts_plot(data, interactive = TRUE)
    expect_s3_class(interactive_plot, "plotly")
})

test_that("regression plot creation works", {
    # Create sample regression data
    set.seed(123)
    data <- data.frame(
        x = rnorm(100),
        y = rnorm(100) + 2 * rnorm(100)
    )
    
    # Test plot creation with and without CI
    plot_with_ci <- create_regression_plot(data, "x", "y", add_ci = TRUE)
    plot_without_ci <- create_regression_plot(data, "x", "y", add_ci = FALSE)
    
    expect_s3_class(plot_with_ci, "plotly")
    expect_s3_class(plot_without_ci, "plotly")
})

test_that("classification plot creation works", {
    # Create sample classification data
    set.seed(123)
    data <- data.frame(
        x = c(rnorm(50), rnorm(50, 2)),
        y = c(rnorm(50), rnorm(50, 2)),
        class = factor(rep(c("A", "B"), each = 50))
    )
    
    # Test plot creation
    plot <- create_classification_plot(data, "x", "y", "class")
    expect_s3_class(plot, "plotly")
})

test_that("clustering plot creation works", {
    # Create sample clustering data
    set.seed(123)
    data <- data.frame(
        x = c(rnorm(30), rnorm(30, 3), rnorm(30, 0, 3)),
        y = c(rnorm(30), rnorm(30, 3), rnorm(30, 0, 3))
    )
    clusters <- kmeans(data, centers = 3)$cluster
    
    # Test plot creation
    plot <- create_clustering_plot(data, clusters, "x", "y")
    expect_s3_class(plot, "plotly")
})

test_that("anomaly detection plot creation works", {
    # Create sample data with anomalies
    set.seed(123)
    data <- rnorm(100)
    anomalies <- abs(data) > 2
    
    # Test plot creation
    plot <- create_anomaly_plot(data, anomalies)
    expect_s3_class(plot, "plotly")
})

test_that("data table creation works", {
    # Create sample data
    data <- data.frame(
        x = 1:10,
        y = letters[1:10],
        z = rnorm(10)
    )
    
    # Test with default options
    table <- create_data_table(data)
    expect_s3_class(table, "datatables")
    
    # Test with custom options
    custom_options <- list(pageLength = 5, scrollX = FALSE)
    custom_table <- create_data_table(data, custom_options)
    expect_s3_class(custom_table, "datatables")
})

test_that("metrics dashboard creation works", {
    # Create sample metrics
    metrics <- list(
        accuracy = 0.95,
        rmse = 0.05,
        mae = 0.03,
        r2 = 0.92
    )
    
    # Test dashboard creation
    dashboard <- create_metrics_dashboard(metrics)
    expect_true(inherits(dashboard, "shiny.tag"))
    
    # Test metric validation
    expect_error(create_metrics_dashboard(list()))  # Empty list
    expect_error(create_metrics_dashboard(list(a = "not_numeric")))  # Non-numeric value
})

test_that("metric formatting works", {
    # Test various numeric formats
    expect_equal(format_metric_value(1234567), "1.2M")
    expect_equal(format_metric_value(12345), "12.3K")
    expect_equal(format_metric_value(123), "123")
    expect_equal(format_metric_value(1.2345), "1.23")
})

test_that("metric descriptions are correct", {
    # Test known metric descriptions
    expect_type(get_metric_description("accuracy"), "character")
    expect_type(get_metric_description("rmse"), "character")
    
    # Test unknown metric fallback
    expect_equal(get_metric_description("unknown"), "Metric value")
})