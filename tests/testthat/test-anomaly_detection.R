test_that("basic anomaly detection works", {
    # Create sample data with known anomalies
    set.seed(123)
    normal_data <- rnorm(100)
    anomalies <- c(10, -10, 8, -8)  # Clear outliers
    data <- c(normal_data, anomalies)
    
    # Test IQR method
    iqr_result <- detect_anomalies(data, method = "iqr")
    expect_type(iqr_result, "list")
    expect_true(all(c("anomalies", "scores", "bounds") %in% names(iqr_result)))
    expect_true(sum(iqr_result$anomalies) >= length(anomalies))
    
    # Test z-score method
    zscore_result <- detect_anomalies(data, method = "zscore")
    expect_type(zscore_result, "list")
    expect_true(all(c("anomalies", "scores") %in% names(zscore_result)))
    
    # Verify that clear outliers are detected
    anomaly_indices <- length(normal_data) + 1:length(anomalies)
    expect_true(all(zscore_result$anomalies[anomaly_indices]))
})

test_that("seasonal anomaly detection works", {
    # Create seasonal data with anomalies
    set.seed(123)
    t <- 1:100
    seasonal <- sin(2 * pi * t/12)  # Annual seasonality
    trend <- 0.1 * t
    noise <- rnorm(100, sd = 0.1)
    anomalies <- rep(0, 100)
    anomalies[c(25, 50, 75)] <- 5  # Add clear anomalies
    data <- seasonal + trend + noise + anomalies
    
    # Test seasonal anomaly detection
    result <- detect_seasonal_anomalies(data, frequency = 12)
    
    expect_type(result, "list")
    expect_true(all(c("seasonal", "remainder", "decomposition") %in% names(result)))
    
    # Check that major anomalies are detected
    expect_true(any(result$remainder$anomalies[c(25, 50, 75)]))
})

test_that("contextual anomaly detection works", {
    # Create data with contextual anomalies
    set.seed(123)
    n <- 100
    context <- data.frame(
        temp = rnorm(n, mean = 20, sd = 5),
        humidity = rnorm(n, mean = 60, sd = 10)
    )
    # Energy consumption should correlate with temperature and humidity
    target <- 2 * context$temp + 0.5 * context$humidity + rnorm(n, sd = 2)
    # Add contextual anomalies
    target[c(25, 50, 75)] <- target[c(25, 50, 75)] + 20
    
    data <- cbind(context, consumption = target)
    
    # Test contextual anomaly detection
    result <- detect_contextual_anomalies(
        data,
        target = "consumption",
        context = c("temp", "humidity")
    )
    
    expect_type(result, "list")
    expect_true(all(c("anomalies", "scores", "model", "residuals") %in% names(result)))
    expect_true(any(result$anomalies[c(25, 50, 75)]))
})

test_that("anomaly report generation works", {
    # Create data with known anomalies
    set.seed(123)
    data <- rnorm(100)
    data[c(1, 50, 100)] <- c(10, -10, 8)  # Add clear anomalies
    
    # Detect anomalies and generate report
    detection_result <- detect_anomalies(data, method = "zscore")
    report <- generate_anomaly_report(detection_result, data)
    
    expect_type(report, "list")
    expect_true(all(c(
        "total_observations",
        "num_anomalies",
        "anomaly_rate",
        "mean_score",
        "max_score"
    ) %in% names(report)))
    
    # Check report values
    expect_equal(report$total_observations, 100)
    expect_true(report$num_anomalies > 0)
    expect_true(report$anomaly_rate > 0 && report$anomaly_rate < 1)
})

test_that("anomaly detection functions handle errors appropriately", {
    # Test invalid data
    expect_error(detect_anomalies(NULL))
    expect_error(detect_anomalies(NA))
    
    # Test invalid method
    data <- rnorm(100)
    expect_error(detect_anomalies(data, method = "invalid_method"))
    
    # Test invalid threshold
    expect_error(detect_anomalies(data, threshold = -1))
    expect_error(detect_anomalies(data, threshold = 2))
    
    # Test invalid seasonal frequency
    expect_error(detect_seasonal_anomalies(data, frequency = 0))
    
    # Test contextual anomalies with missing variables
    data <- data.frame(x = 1:10, y = 1:10)
    expect_error(detect_contextual_anomalies(data, "z", "x"))
})