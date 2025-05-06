#' Visualization functions for GeneralAnalyser
#' @export

plot_time_series <- function(data, time_col, value_col, title = "Time Series Plot") {
    ggplot(data, aes_string(x = time_col, y = value_col)) +
        geom_line() +
        labs(title = title) +
        theme_minimal()
}

plot_correlation_matrix <- function(data, method = "pearson") {
    cor_matrix <- cor(data, method = method)
    melted_cor <- reshape2::melt(cor_matrix)
    ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
}