#' Model selection functions
#' @export

select_best_model <- function(models, test_data, actual) {
    scores <- sapply(models, function(model) {
        pred <- predict(model, newdata = test_data)
        Metrics::rmse(actual, pred)
    })
    models[[which.min(scores)]]
}

cross_validate <- function(data, target_col, model_fn, folds = 5) {
    fold_indices <- cut(seq(1, nrow(data)), breaks = folds, labels = FALSE)
    scores <- numeric(folds)
    
    for(i in 1:folds) {
        test_idx <- which(fold_indices == i)
        train <- data[-test_idx, ]
        test <- data[test_idx, ]
        
        model <- model_fn(train)
        pred <- predict(model, newdata = test)
        scores[i] <- Metrics::rmse(test[[target_col]], pred)
    }
    
    mean(scores)
}