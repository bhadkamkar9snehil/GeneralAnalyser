test_that("regression model fitting works", {
    # Create sample data
    set.seed(123)
    n <- 100
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    y <- 2 * x1 - 1.5 * x2 + rnorm(n, sd = 0.5)
    data <- data.frame(y = y, x1 = x1, x2 = x2)
    
    # Test linear regression
    model <- fit_regression(data, "y", c("x1", "x2"), method = "linear")
    expect_s3_class(model, "lm")
    expect_equal(length(coef(model)), 3) # intercept + 2 predictors
    
    # Test logistic regression
    y_binary <- as.factor(y > median(y))
    data$y <- y_binary
    log_model <- fit_regression(data, "y", c("x1", "x2"), method = "logistic")
    expect_s3_class(log_model, "glm")
    
    # Test cross-validation
    cv_results <- cross_validate(model, data)
    expect_true("train" %in% class(cv_results))
})

test_that("regression diagnostics work", {
    # Create simple linear model
    x <- 1:10
    y <- x + rnorm(10)
    data <- data.frame(x = x, y = y)
    model <- fit_regression(data, "y", "x", method = "linear")
    
    # Test diagnostics
    diag <- regression_diagnostics(model)
    expect_type(diag, "list")
    expect_true(all(c("residuals", "fitted", "leverage") %in% names(diag)))
    
    # Check values
    expect_equal(length(diag$residuals), 10)
    expect_equal(length(diag$fitted), 10)
    expect_equal(length(diag$leverage), 10)
})

test_that("regression functions handle errors appropriately", {
    # Test missing data
    expect_error(fit_regression(NULL, "y", "x"))
    
    # Test invalid method
    data <- data.frame(x = 1:10, y = 1:10)
    expect_error(fit_regression(data, "y", "x", method = "invalid"))
    
    # Test missing variables
    expect_error(fit_regression(data, "z", "x"))
})