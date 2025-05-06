# Report Generator

#' Generate analysis report
#' @param analysis_results List of analysis results
#' @param config Report configuration
#' @return Report content
generateReport <- function(analysis_results, config = list()) {
    # Initialize rmarkdown if not available
    if(!requireNamespace("rmarkdown", quietly = TRUE)) {
        stop("Package 'rmarkdown' is required for report generation")
    }
    
    # Set default options
    options <- list(
        format = "html",
        title = "Analysis Report",
        author = "GeneralAnalyser",
        date = Sys.Date(),
        toc = TRUE,
        number_sections = TRUE,
        theme = "default"
    )
    
    # Merge with provided configuration
    options <- modifyList(options, config)
    
    # Create temporary report file
    report_file <- tempfile(fileext = ".Rmd")
    
    # Generate report content
    report_content <- generateReportContent(analysis_results, options)
    
    # Write report content to file
    writeLines(report_content, report_file)
    
    # Render report
    output_file <- rmarkdown::render(
        input = report_file,
        output_format = switch(options$format,
            "html" = rmarkdown::html_document(
                toc = options$toc,
                number_sections = options$number_sections,
                theme = options$theme
            ),
            "pdf" = rmarkdown::pdf_document(
                toc = options$toc,
                number_sections = options$number_sections
            ),
            "word" = rmarkdown::word_document(
                toc = options$toc,
                number_sections = options$number_sections
            ),
            stop("Unsupported output format")
        )
    )
    
    output_file
}

#' Generate report content
#' @param results Analysis results
#' @param options Report options
#' @return Report content as string
generateReportContent <- function(results, options) {
    # Initialize report sections
    sections <- list(
        header = generateHeader(options),
        summary = generateSummary(results),
        methodology = generateMethodology(results),
        results = generateResults(results),
        visualization = generateVisualization(results),
        conclusion = generateConclusion(results)
    )
    
    # Combine sections
    paste(unlist(sections), collapse = "\n\n")
}

#' Generate report header
#' @param options Report options
#' @return Header content
generateHeader <- function(options) {
    sprintf('---
title: "%s"
author: "%s"
date: "%s"
output:
  %s_document:
    toc: %s
    number_sections: %s
    theme: %s
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)
```
',
        options$title,
        options$author,
        options$date,
        options$format,
        tolower(as.character(options$toc)),
        tolower(as.character(options$number_sections)),
        options$theme
    )
}

#' Generate executive summary
#' @param results Analysis results
#' @return Summary content
generateSummary <- function(results) {
    c(
        "# Executive Summary",
        "\nThis report presents the results of the data analysis performed using GeneralAnalyser.",
        generateHighlights(results)
    )
}

#' Generate methodology section
#' @param results Analysis results
#' @return Methodology content
generateMethodology <- function(results) {
    c(
        "# Methodology",
        "\n## Data Preprocessing",
        generatePreprocessingSummary(results$preprocessing),
        "\n## Analysis Approach",
        generateAnalysisApproach(results$analysis)
    )
}

#' Generate results section
#' @param results Analysis results
#' @return Results content
generateResults <- function(results) {
    c(
        "# Analysis Results",
        generateAnalysisResults(results)
    )
}

#' Generate visualization section
#' @param results Analysis results
#' @return Visualization content
generateVisualization <- function(results) {
    c(
        "# Visualizations",
        generatePlots(results)
    )
}

#' Generate conclusion section
#' @param results Analysis results
#' @return Conclusion content
generateConclusion <- function(results) {
    c(
        "# Conclusion and Recommendations",
        generateInsights(results),
        generateRecommendations(results)
    )
}

#' Generate key highlights
#' @param results Analysis results
#' @return Highlights content
generateHighlights <- function(results) {
    highlights <- c()
    
    # Add data overview
    if(!is.null(results$data_summary)) {
        highlights <- c(highlights,
            "\n## Data Overview",
            sprintf("- Dataset size: %d observations, %d variables",
                   results$data_summary$n_rows,
                   results$data_summary$n_cols),
            sprintf("- Time period: %s to %s",
                   results$data_summary$time_range[1],
                   results$data_summary$time_range[2])
        )
    }
    
    # Add key findings
    if(!is.null(results$key_findings)) {
        highlights <- c(highlights,
            "\n## Key Findings",
            paste("-", results$key_findings)
        )
    }
    
    highlights
}

#' Generate preprocessing summary
#' @param preprocessing Preprocessing results
#' @return Preprocessing summary content
generatePreprocessingSummary <- function(preprocessing) {
    if(is.null(preprocessing)) return(NULL)
    
    summary <- c()
    
    # Data cleaning summary
    if(!is.null(preprocessing$cleaning)) {
        summary <- c(summary,
            "\n### Data Cleaning",
            sprintf("- Missing values handled: %d",
                   preprocessing$cleaning$missing_handled),
            sprintf("- Outliers detected and treated: %d",
                   preprocessing$cleaning$outliers_handled),
            sprintf("- Duplicate records removed: %d",
                   preprocessing$cleaning$duplicates_removed)
        )
    }
    
    # Feature engineering summary
    if(!is.null(preprocessing$feature_engineering)) {
        summary <- c(summary,
            "\n### Feature Engineering",
            "- Features created:",
            paste(" ", names(preprocessing$feature_engineering$new_features))
        )
    }
    
    summary
}

#' Generate analysis approach description
#' @param analysis Analysis configuration
#' @return Analysis approach content
generateAnalysisApproach <- function(analysis) {
    if(is.null(analysis)) return(NULL)
    
    approach <- c()
    
    # Add analysis type description
    if(!is.null(analysis$type)) {
        approach <- c(approach,
            sprintf("\n### %s Analysis", tools::toTitleCase(analysis$type)),
            analysis$description
        )
    }
    
    # Add methodology details
    if(!is.null(analysis$methodology)) {
        approach <- c(approach,
            "\n### Methods Used",
            paste("-", analysis$methodology)
        )
    }
    
    approach
}

#' Generate analysis results
#' @param results Analysis results
#' @return Results content
generateAnalysisResults <- function(results) {
    if(is.null(results$analysis)) return(NULL)
    
    content <- c()
    
    # Add statistical summary
    if(!is.null(results$analysis$statistics)) {
        content <- c(content,
            "\n## Statistical Summary",
            "```{r}",
            "knitr::kable(results$analysis$statistics)",
            "```"
        )
    }
    
    # Add model results if applicable
    if(!is.null(results$analysis$model)) {
        content <- c(content,
            "\n## Model Performance",
            generateModelSummary(results$analysis$model)
        )
    }
    
    content
}

#' Generate model summary
#' @param model Model results
#' @return Model summary content
generateModelSummary <- function(model) {
    if(is.null(model)) return(NULL)
    
    summary <- c()
    
    # Model metrics
    if(!is.null(model$metrics)) {
        summary <- c(summary,
            "\n### Performance Metrics",
            "```{r}",
            "knitr::kable(model$metrics)",
            "```"
        )
    }
    
    # Feature importance
    if(!is.null(model$feature_importance)) {
        summary <- c(summary,
            "\n### Feature Importance",
            "```{r}",
            "plotFeatureImportance(model$feature_importance)",
            "```"
        )
    }
    
    summary
}

#' Generate plots
#' @param results Analysis results
#' @return Visualization content
generatePlots <- function(results) {
    if(is.null(results$plots)) return(NULL)
    
    plots <- c()
    
    # Add each plot with description
    for(plot in results$plots) {
        plots <- c(plots,
            sprintf("\n## %s", plot$title),
            plot$description,
            "```{r, fig.width=8, fig.height=6}",
            plot$code,
            "```"
        )
    }
    
    plots
}

#' Generate insights
#' @param results Analysis results
#' @return Insights content
generateInsights <- function(results) {
    if(is.null(results$insights)) return(NULL)
    
    c(
        "\n## Key Insights",
        paste("-", results$insights)
    )
}

#' Generate recommendations
#' @param results Analysis results
#' @return Recommendations content
generateRecommendations <- function(results) {
    if(is.null(results$recommendations)) return(NULL)
    
    c(
        "\n## Recommendations",
        paste("-", results$recommendations)
    )
}

#' Export report to file
#' @param report Report content
#' @param file_path Output file path
#' @param format Output format
#' @return Export status
exportReport <- function(report, file_path, format = "html") {
    tryCatch({
        file.copy(report, file_path, overwrite = TRUE)
        list(success = TRUE, message = sprintf("Report exported to %s", file_path))
    }, error = function(e) {
        list(success = FALSE, message = sprintf("Failed to export report: %s", e$message))
    })
}