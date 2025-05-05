#' Theme Module
#' 
#' This module handles theme customization and consistent styling across the dashboard.

#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
NULL

#' Create color palette
#' @param theme Theme name
#' @return List of colors for the theme
create_color_palette <- function(theme = "default") {
    switch(theme,
        "default" = list(
            primary = "#2C3E50",
            secondary = "#3498DB",
            success = "#2ECC71",
            warning = "#F1C40F",
            danger = "#E74C3C",
            info = "#3498DB",
            light = "#ECF0F1",
            dark = "#34495E",
            text = "#2C3E50",
            background = "#FFFFFF"
        ),
        "dark" = list(
            primary = "#375A7F",
            secondary = "#444444",
            success = "#00bc8c",
            warning = "#F39C12",
            danger = "#E74C3C",
            info = "#3498DB",
            light = "#303030",
            dark = "#1a1a1a",
            text = "#FFFFFF",
            background = "#222222"
        ),
        "light" = list(
            primary = "#007BFF",
            secondary = "#6C757D",
            success = "#28A745",
            warning = "#FFC107",
            danger = "#DC3545",
            info = "#17A2B8",
            light = "#F8F9FA",
            dark = "#343A40",
            text = "#212529",
            background = "#FFFFFF"
        )
    )
}

#' Create plot theme
#' @param theme Theme name
#' @return ggplot theme object
create_plot_theme <- function(theme = "default") {
    colors <- create_color_palette(theme)
    
    ggplot2::theme_minimal() +
        ggplot2::theme(
            text = element_text(color = colors$text),
            plot.title = element_text(
                face = "bold",
                size = 16,
                hjust = 0,
                margin = margin(b = 20)
            ),
            plot.subtitle = element_text(
                size = 12,
                margin = margin(b = 10)
            ),
            plot.caption = element_text(
                size = 10,
                color = colors$secondary
            ),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            panel.grid.major = element_line(
                color = ifelse(theme == "dark",
                             alpha(colors$light, 0.1),
                             alpha(colors$dark, 0.1))
            ),
            panel.grid.minor = element_line(
                color = ifelse(theme == "dark",
                             alpha(colors$light, 0.05),
                             alpha(colors$dark, 0.05))
            ),
            strip.text = element_text(
                size = 12,
                face = "bold"
            ),
            legend.title = element_text(size = 11),
            legend.text = element_text(size = 10),
            legend.position = "bottom",
            legend.box = "horizontal"
        )
}

#' Create dashboard theme
#' @param theme Theme name
#' @return List of CSS styles
create_dashboard_theme <- function(theme = "default") {
    colors <- create_color_palette(theme)
    
    list(
        # Header styles
        header = list(
            "background-color" = colors$primary,
            "border-bottom" = paste0("1px solid ", colors$dark)
        ),
        
        # Sidebar styles
        sidebar = list(
            "background-color" = ifelse(theme == "dark",
                                      colors$dark,
                                      colors$light),
            "color" = colors$text,
            ".nav-item" = list(
                "color" = colors$text,
                "&:hover" = list(
                    "background-color" = colors$primary,
                    "color" = colors$light
                ),
                "&.active" = list(
                    "background-color" = colors$primary,
                    "color" = colors$light
                )
            )
        ),
        
        # Box styles
        box = list(
            "background-color" = ifelse(theme == "dark",
                                      colors$dark,
                                      colors$background),
            "border-radius" = "8px",
            "box-shadow" = ifelse(theme == "dark",
                                "0 2px 4px rgba(0,0,0,0.2)",
                                "0 2px 4px rgba(0,0,0,0.1)"),
            ".box-header" = list(
                "background-color" = colors$primary,
                "color" = colors$light
            )
        ),
        
        # Button styles
        button = list(
            "primary" = list(
                "background-color" = colors$primary,
                "border-color" = colors$primary,
                "color" = colors$light,
                "&:hover" = list(
                    "background-color" = darken(colors$primary),
                    "border-color" = darken(colors$primary)
                )
            ),
            "secondary" = list(
                "background-color" = colors$secondary,
                "border-color" = colors$secondary,
                "color" = colors$light
            )
        ),
        
        # Input styles
        input = list(
            "background-color" = ifelse(theme == "dark",
                                      lighten(colors$dark),
                                      colors$light),
            "border" = paste0("1px solid ", colors$secondary),
            "color" = colors$text,
            "&:focus" = list(
                "border-color" = colors$primary,
                "box-shadow" = paste0("0 0 0 0.2rem ", alpha(colors$primary, 0.25))
            )
        ),
        
        # Table styles
        table = list(
            "background-color" = ifelse(theme == "dark",
                                      colors$dark,
                                      colors$background),
            "color" = colors$text,
            "thead" = list(
                "background-color" = colors$primary,
                "color" = colors$light
            ),
            "tbody tr:hover" = list(
                "background-color" = ifelse(theme == "dark",
                                         lighten(colors$dark),
                                         darken(colors$light))
            )
        )
    )
}

#' Initialize theme
#' @param theme Theme name
#' @return NULL
initialize_theme <- function(theme = "default") {
    # Set up color palette
    colors <- create_color_palette(theme)
    
    # Set up plot theme
    plot_theme <- create_plot_theme(theme)
    
    # Set up dashboard theme
    dashboard_theme <- create_dashboard_theme(theme)
    
    # Apply themes
    options(
        generalanalyser.theme = theme,
        generalanalyser.colors = colors,
        generalanalyser.plot_theme = plot_theme,
        generalanalyser.dashboard_theme = dashboard_theme
    )
}

# Helper functions

#' Lighten a color
#' @param color Color to lighten
#' @param amount Amount to lighten (0-1)
#' @return Lightened color
lighten <- function(color, amount = 0.1) {
    rgb <- col2rgb(color)
    rgb <- rgb + (255 - rgb) * amount
    rgb <- pmin(255, pmax(0, rgb))
    rgb(rgb[1]/255, rgb[2]/255, rgb[3]/255)
}

#' Darken a color
#' @param color Color to darken
#' @param amount Amount to darken (0-1)
#' @return Darkened color
darken <- function(color, amount = 0.1) {
    rgb <- col2rgb(color)
    rgb <- rgb * (1 - amount)
    rgb <- pmin(255, pmax(0, rgb))
    rgb(rgb[1]/255, rgb[2]/255, rgb[3]/255)
}

#' Add alpha to a color
#' @param color Color to modify
#' @param alpha Alpha value (0-1)
#' @return Color with alpha
alpha <- function(color, alpha = 1) {
    rgb <- col2rgb(color)
    rgb(rgb[1]/255, rgb[2]/255, rgb[3]/255, alpha)
}