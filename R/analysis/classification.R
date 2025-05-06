#' Classification Analysis Functions
#' 
#' This module contains functions for various classification methods
#' including random forests, SVM, and decision trees.

#' @import caret
#' @import randomForest
#' @import e1071
NULL

#' Train classification model
#' @param data Data frame containing features and target
#' @param target Name of target variable
#' @param features Vector of feature names
#' @param method Classification method (rf, svm, tree)
#' @return Trained model object
train_classifier <- function(data, target, features, method = "rf") {
    formula <- as.formula(paste(target, "~", paste(features, collapse = " + ")))
    
    model <- switch(method,
        rf = randomForest(formula, data = data),
        svm = svm(formula, data = data, kernel = "radial"),
        tree = rpart(formula, data = data),
        stop("Unsupported classification method")
    )
    
    return(model)
}

#' Evaluate classifier performance
#' @param model Trained classifier
#' @param test_data Test dataset
#' @param actual Actual class labels
#' @return List of performance metrics
evaluate_classifier <- function(model, test_data, actual) {
    predicted <- predict(model, test_data)
    
    # Create confusion matrix
    conf_matrix <- table(Actual = actual, Predicted = predicted)
    
    # Calculate metrics
    n <- length(actual)
    correct <- sum(diag(conf_matrix))
    accuracy <- correct / n
    
    # For binary classification
    if (nlevels(factor(actual)) == 2) {
        tp <- conf_matrix[2,2]
        fp <- conf_matrix[1,2]
        fn <- conf_matrix[2,1]
        tn <- conf_matrix[1,1]
        
        precision <- tp / (tp + fp)
        recall <- tp / (tp + fn)
        f1_score <- 2 * (precision * recall) / (precision + recall)
        
        return(list(
            confusion_matrix = conf_matrix,
            accuracy = accuracy,
            precision = precision,
            recall = recall,
            f1_score = f1_score
        ))
    }
    
    # For multiclass
    return(list(
        confusion_matrix = conf_matrix,
        accuracy = accuracy
    ))
}

#' Feature importance analysis
#' @param model Trained classifier
#' @param features Feature names
#' @return Data frame of feature importance scores
feature_importance <- function(model, features) {
    if (inherits(model, "randomForest")) {
        importance <- importance(model)
        return(data.frame(
            Feature = features,
            Importance = importance[,1]
        ))
    } else if (inherits(model, "svm")) {
        # For SVM, use weights for linear kernel
        if (model$kernel == "linear") {
            weights <- t(model$coefs) %*% model$SV
            return(data.frame(
                Feature = features,
                Importance = abs(weights)
            ))
        }
    } else if (inherits(model, "rpart")) {
        importance <- model$variable.importance
        return(data.frame(
            Feature = names(importance),
            Importance = importance
        ))
    }
    
    stop("Feature importance not implemented for this model type")
}

#' Perform classification analysis
#' @param data Data frame containing features and target
#' @param target_col Target column name
#' @param feature_cols Feature column names
#' @param method Classification method (Random Forest, SVM, Decision Tree)
#' @return List containing model and metrics
performClassification <- function(data, target_col, feature_cols, method = "Random Forest") {
    # Data preparation
    data[[target_col]] <- as.factor(data[[target_col]])
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
        "Random Forest" = fit_random_forest(formula_obj, train_data, test_data),
        "SVM" = fit_svm(formula_obj, train_data, test_data),
        "Decision Tree" = fit_decision_tree(formula_obj, train_data, test_data),
        stop("Unsupported classification method")
    )
    
    result$method <- method
    result$feature_cols <- feature_cols
    result$target_col <- target_col
    result
}

# Helper functions for different classification methods
fit_random_forest <- function(formula, train_data, test_data) {
    model <- randomForest::randomForest(formula, data = train_data,
                                      ntree = 500, importance = TRUE)
    predictions <- predict(model, newdata = test_data)
    actual <- test_data[[all.vars(formula)[1]]]
    
    # Calculate metrics
    conf_matrix <- table(Predicted = predictions, Actual = actual)
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    # Feature importance
    importance <- randomForest::importance(model)
    
    list(
        model = model,
        predictions = predictions,
        actual = actual,
        accuracy = accuracy,
        confusion_matrix = conf_matrix,
        feature_importance = importance[,1]  # Mean decrease in accuracy
    )
}

fit_svm <- function(formula, train_data, test_data) {
    model <- e1071::svm(formula, data = train_data, probability = TRUE)
    predictions <- predict(model, newdata = test_data)
    predictions_prob <- attr(predict(model, newdata = test_data, probability = TRUE), "probabilities")
    actual <- test_data[[all.vars(formula)[1]]]
    
    # Calculate metrics
    conf_matrix <- table(Predicted = predictions, Actual = actual)
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    list(
        model = model,
        predictions = predictions,
        predictions_prob = predictions_prob,
        actual = actual,
        accuracy = accuracy,
        confusion_matrix = conf_matrix
    )
}

fit_decision_tree <- function(formula, train_data, test_data) {
    model <- rpart::rpart(formula, data = train_data, method = "class")
    predictions <- predict(model, newdata = test_data, type = "class")
    predictions_prob <- predict(model, newdata = test_data, type = "prob")
    actual <- test_data[[all.vars(formula)[1]]]
    
    # Calculate metrics
    conf_matrix <- table(Predicted = predictions, Actual = actual)
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    
    # Variable importance
    importance <- model$variable.importance
    
    list(
        model = model,
        predictions = predictions,
        predictions_prob = predictions_prob,
        actual = actual,
        accuracy = accuracy,
        confusion_matrix = conf_matrix,
        feature_importance = importance
    )
}

#' Plot classification results
#' @param results Results from performClassification
#' @return plotly object
plotClassification <- function(results) {
    # Confusion Matrix Heatmap
    conf_matrix <- results$confusion_matrix
    conf_df <- as.data.frame(conf_matrix)
    names(conf_df) <- c("Predicted", "Actual", "Count")
    
    plot_ly(data = conf_df,
            x = ~Predicted,
            y = ~Actual,
            z = ~Count,
            type = "heatmap",
            colors = colorRamp(c("white", "steelblue"))) %>%
        layout(title = paste(results$method, "Confusion Matrix"),
               xaxis = list(title = "Predicted Class"),
               yaxis = list(title = "Actual Class"))
}

#' Print classification metrics
#' @param results Results from performClassification
#' @return Text output of metrics
printClassificationMetrics <- function(results) {
    cat("Classification Method:", results$method, "\n")
    cat("Accuracy:", round(results$accuracy * 100, 2), "%\n\n")
    cat("Confusion Matrix:\n")
    print(results$confusion_matrix)
    
    if(!is.null(results$feature_importance)) {
        cat("\nFeature Importance:\n")
        importance_df <- data.frame(
            Feature = names(results$feature_importance),
            Importance = results$feature_importance
        )
        importance_df <- importance_df[order(-importance_df$Importance), ]
        print(importance_df)
    }
}