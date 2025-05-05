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