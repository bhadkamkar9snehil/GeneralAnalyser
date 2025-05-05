test_that("time series forecasting works", {
    # Create sample time series data
    data <- ts(rnorm(100) + 1:100, frequency = 12)
    
    # Test forecast function
    forecast_result <- ts_forecast(data, horizon = 12, method = "auto.arima")
    expect_s3_class(forecast_result, "forecast")
    expect_equal(length(forecast_result$mean), 12)
    
    # Test decomposition
    decomp_result <- ts_decompose(data)
    expect_true(all(c("seasonal", "trend", "random") %in% names(decomp_result)))
    
    # Test seasonality check
    season_result <- check_seasonality(data)
    expect_type(season_result, "list")
    expect_true(all(c("seasonal", "acf") %in% names(season_result)))
})

test_that("time series functions handle errors appropriately", {
    # Test invalid data
    expect_error(ts_forecast(NA))
    expect_error(ts_forecast(NULL))
    
    # Test invalid method
    data <- ts(1:10)
    expect_error(ts_forecast(data, method = "invalid_method"))
    
    # Test invalid horizon
    expect_error(ts_forecast(data, horizon = -1))
})