#' Regression Analysis Functions
#' 
#' This module contains functions for various types of regression analysis
#' including linear, logistic, and polynomial regression.

#' @import caret
#' @import glmnet
NULL

#' Fit regression model
#' @param data Data frame containing predictor and response variables
#' @param response Name of response variable
#' @param predictors Vector of predictor variable names
#' @param method Regression method (linear, logistic, polynomial)
#' @return Fitted model object
fit_regression <- function(data, response, predictors, method = "linear") {
    formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
    
    model <- switch(method,
        linear = lm(formula, data = data),
        logistic = glm(formula, data = data, family = binomial),
        polynomial = lm(update(formula, . ~ . + poly(., degree = 2)), data = data),
        stop("Unsupported regression method")
    )
    
    return(model)
}

#' Cross-validate regression model
#' @param model Fitted model object
#' @param data Training data
#' @param k Number of folds
#' @return Cross-validation results
cross_validate <- function(model, data, k = 5) {
    ctrl <- trainControl(method = "cv", number = k)
    
    cv_model <- train(
        form = formula(model),
        data = data,
        method = if(inherits(model, "glm")) "glm" else "lm",
        trControl = ctrl
    )
    
    return(cv_model)
}

#' Generate regression diagnostics
#' @param model Fitted model object
#' @return List of diagnostic plots and statistics
regression_diagnostics <- function(model) {
    if (!inherits(model, c("lm", "glm"))) {
        stop("Model must be lm or glm object")
    }
    
    list(
        residuals = resid(model),
        fitted = fitted(model),
        leverage = hatvalues(model),
        cooks_distance = cooks.distance(model),
        vif = if(inherits(model, "lm")) car::vif(model) else NULL
    )
}

#' Perform regression analysis with multiple methods
#' @param data Data frame containing features and target
#' @param target_col Target column name
#' @param feature_cols Feature column names
#' @param method Regression method (linear, polynomial, logistic, ridge, lasso)
#' @return List containing model and metrics
performRegression <- function(data, target_col, feature_cols, method = "linear") {
    # Data preparation
    formula_str <- paste(target_col, "~", paste(feature_cols, collapse = " + "))
    formula_obj <- as.formula(formula_str)
    
    # Train/test split
    set.seed(123)
    sample_size <- floor(0.8 * nrow(data))
    train_indices <- sample(seq_len(nrow(data)), size = sample_size)
    train_data <- data[train_indices, ]
    test_data <- data[-train_indices, ]
    
    # Model fitting and prediction
    result <- switch(method,
        "Linear" = fit_linear(formula_obj, train_data, test_data),
        "Polynomial" = fit_polynomial(target_col, feature_cols, train_data, test_data),
        "Logistic" = fit_logistic(formula_obj, train_data, test_data),
        "Ridge" = fit_ridge(target_col, feature_cols, train_data, test_data),
        "Lasso" = fit_lasso(target_col, feature_cols, train_data, test_data),
        stop("Unsupported regression method")
    )
    
    # Add feature importance
    if(method %in% c("Linear", "Ridge", "Lasso")) {
        result$feature_importance <- abs(coef(result$model))[-1]  # Exclude intercept
        names(result$feature_importance) <- feature_cols
    }
    
    result$method <- method
    result$feature_cols <- feature_cols
    result$target_col <- target_col
    
    result
}

# Helper functions for different regression types
fit_linear <- function(formula, train_data, test_data) {
    model <- lm(formula, data = train_data)
    predictions <- predict(model, newdata = test_data)
    actual <- test_data[[all.vars(formula)[1]]]
    
    list(
        model = model,
        predictions = predictions,
        actual = actual,
        rmse = Metrics::rmse(actual, predictions),
        r_squared = summary(model)$r.squared
    )
}

fit_polynomial <- function(target_col, feature_cols, train_data, test_data, degree = 2) {
    # Create polynomial terms
    poly_formula <- as.formula(paste(target_col, "~", 
        paste(sapply(feature_cols, function(x) {
            paste0("poly(", x, ", ", degree, ", raw = TRUE)")
        }), collapse = " + ")))
    
    model <- lm(poly_formula, data = train_data)
    predictions <- predict(model, newdata = test_data)
    actual <- test_data[[target_col]]
    
    list(
        model = model,
        predictions = predictions,
        actual = actual,
        rmse = Metrics::rmse(actual, predictions),
        r_squared = summary(model)$r.squared
    )
}

fit_logistic <- function(formula, train_data, test_data) {
    model <- glm(formula, data = train_data, family = "binomial")
    predictions_prob <- predict(model, newdata = test_data, type = "response")
    predictions <- ifelse(predictions_prob > 0.5, 1, 0)
    actual <- test_data[[all.vars(formula)[1]]]
    
    # Calculate metrics
    confusion_matrix <- table(Predicted = predictions, Actual = actual)
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    
    list(
        model = model,
        predictions = predictions,
        predictions_prob = predictions_prob,
        actual = actual,
        accuracy = accuracy,
        confusion_matrix = confusion_matrix
    )
}

fit_ridge <- function(target_col, feature_cols, train_data, test_data, alpha = 0) {
    x_train <- as.matrix(train_data[, feature_cols])
    y_train <- train_data[[target_col]]
    x_test <- as.matrix(test_data[, feature_cols])
    y_test <- test_data[[target_col]]
    
    # Standardize features
    x_means <- colMeans(x_train)
    x_sds <- apply(x_train, 2, sd)
    x_train_scaled <- scale(x_train)
    x_test_scaled <- scale(x_test, center = x_means, scale = x_sds)
    
    # Fit model using cv.glmnet
    cv_fit <- cv.glmnet(x_train_scaled, y_train, alpha = alpha)
    model <- glmnet(x_train_scaled, y_train, alpha = alpha, lambda = cv_fit$lambda.min)
    predictions <- predict(model, newx = x_test_scaled)
    
    list(
        model = model,
        predictions = predictions,
        actual = y_test,
        rmse = Metrics::rmse(y_test, predictions),
        lambda = cv_fit$lambda.min
    )
}

fit_lasso <- function(target_col, feature_cols, train_data, test_data) {
    fit_ridge(target_col, feature_cols, train_data, test_data, alpha = 1)
}

#' Plot regression results
#' @param results Results from performRegression
#' @return plotly object
plotRegression <- function(results) {
    if(results$method == "Logistic") {
        # ROC curve for logistic regression
        roc_data <- data.frame(
            FPR = NA,
            TPR = NA
        )
        thresholds <- seq(0, 1, length.out = 100)
        for(thresh in thresholds) {
            preds <- ifelse(results$predictions_prob > thresh, 1, 0)
            cm <- table(Predicted = preds, Actual = results$actual)
            TPR <- cm[2,2] / sum(cm[,2])
            FPR <- cm[2,1] / sum(cm[,1])
            roc_data <- rbind(roc_data, data.frame(FPR = FPR, TPR = TPR))
        }
        roc_data <- na.omit(roc_data)
        
        plot_ly(data = roc_data, x = ~FPR, y = ~TPR, type = "scatter", mode = "lines") %>%
            add_lines(x = c(0,1), y = c(0,1), line = list(dash = "dash", color = "gray")) %>%
            layout(title = "ROC Curve",
                   xaxis = list(title = "False Positive Rate"),
                   yaxis = list(title = "True Positive Rate"))
    } else {
        # Actual vs Predicted plot for other regression types
        plot_data <- data.frame(
            Actual = results$actual,
            Predicted = results$predictions
        )
        
        plot_ly(data = plot_data) %>%
            add_markers(x = ~Actual, y = ~Predicted, 
                       marker = list(size = 8, opacity = 0.6)) %>%
            add_lines(x = c(min(plot_data$Actual), max(plot_data$Actual)),
                     y = c(min(plot_data$Actual), max(plot_data$Actual)),
                     line = list(dash = "dash", color = "gray")) %>%
            layout(title = "Actual vs Predicted",
                   xaxis = list(title = "Actual Values"),
                   yaxis = list(title = "Predicted Values"))
    }
}