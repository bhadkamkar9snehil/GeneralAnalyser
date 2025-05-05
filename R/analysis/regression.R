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