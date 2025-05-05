test_that("classification model training works", {
    # Create sample classification data
    set.seed(123)
    n <- 100
    x1 <- rnorm(n)
    x2 <- rnorm(n)
    y <- factor(ifelse(x1 + x2 + rnorm(n, sd = 0.5) > 0, "A", "B"))
    data <- data.frame(y = y, x1 = x1, x2 = x2)
    
    # Test random forest
    rf_model <- train_classifier(data, "y", c("x1", "x2"), method = "rf")
    expect_s3_class(rf_model, "randomForest")
    
    # Test SVM
    svm_model <- train_classifier(data, "y", c("x1", "x2"), method = "svm")
    expect_s3_class(svm_model, "svm")
    
    # Test decision tree
    tree_model <- train_classifier(data, "y", c("x1", "x2"), method = "tree")
    expect_s3_class(tree_model, "rpart")
})

test_that("classifier evaluation works", {
    # Create simple classification data
    set.seed(123)
    train_data <- data.frame(
        x = rnorm(100),
        y = factor(rep(c("A", "B"), each = 50))
    )
    test_data <- data.frame(
        x = rnorm(20),
        y = factor(rep(c("A", "B"), each = 10))
    )
    
    # Train and evaluate model
    model <- train_classifier(train_data, "y", "x", method = "rf")
    eval_results <- evaluate_classifier(model, test_data, test_data$y)
    
    # Check evaluation metrics
    expect_type(eval_results, "list")
    expect_true(all(c("confusion_matrix", "accuracy") %in% names(eval_results)))
    expect_true(eval_results$accuracy >= 0 && eval_results$accuracy <= 1)
})

test_that("feature importance analysis works", {
    # Create multivariate classification data
    set.seed(123)
    data <- data.frame(
        x1 = rnorm(100),
        x2 = rnorm(100),
        x3 = rnorm(100),
        y = factor(rep(c("A", "B"), each = 50))
    )
    
    # Train random forest and get feature importance
    rf_model <- train_classifier(data, "y", c("x1", "x2", "x3"), method = "rf")
    importance <- feature_importance(rf_model, c("x1", "x2", "x3"))
    
    expect_s3_class(importance, "data.frame")
    expect_equal(nrow(importance), 3)  # One row per feature
    expect_true(all(c("Feature", "Importance") %in% names(importance)))
})

test_that("classification functions handle errors appropriately", {
    # Test missing data
    expect_error(train_classifier(NULL, "y", "x"))
    
    # Test invalid method
    data <- data.frame(x = 1:10, y = factor(rep(c("A", "B"), 5)))
    expect_error(train_classifier(data, "y", "x", method = "invalid"))
    
    # Test missing variables
    expect_error(train_classifier(data, "z", "x"))
    
    # Test non-factor response
    data$y <- 1:10
    expect_error(train_classifier(data, "y", "x", method = "rf"))
})