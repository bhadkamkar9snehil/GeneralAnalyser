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

# Theme Configuration

#' Create custom theme
#' @param primary_color Primary color
#' @param secondary_color Secondary color
#' @param text_color Text color
#' @param background_color Background color
#' @return List of theme settings
createCustomTheme <- function(primary_color = "#2C3E50",
                            secondary_color = "#3498DB",
                            text_color = "#2C3E50",
                            background_color = "#FFFFFF") {
    list(
        # General
        background_color = background_color,
        text_color = text_color,
        
        # Headers
        header_background = primary_color,
        header_text = "#FFFFFF",
        
        # Sidebar
        sidebar_background = "#2C3E50",
        sidebar_text = "#FFFFFF",
        sidebar_hover = "#3498DB",
        
        # Boxes
        box_background = "#FFFFFF",
        box_border = "#E5E5E5",
        box_header = secondary_color,
        box_header_text = "#FFFFFF",
        
        # Buttons
        button_primary = secondary_color,
        button_info = "#3498DB",
        button_success = "#2ECC71",
        button_warning = "#F1C40F",
        button_danger = "#E74C3C",
        
        # Tables
        table_header = "#F5F5F5",
        table_border = "#E5E5E5",
        table_hover = "#F9F9F9",
        
        # Plots
        plot_background = "#FFFFFF",
        plot_gridlines = "#E5E5E5",
        plot_text = text_color,
        
        # Forms
        input_background = "#FFFFFF",
        input_border = "#E5E5E5",
        input_focus = secondary_color,
        
        # Progress bars
        progress_background = "#F5F5F5",
        progress_bar = secondary_color
    )
}

#' Apply theme to dashboard
#' @param theme Theme settings list
applyTheme <- function(theme) {
    css <- sprintf("
        body {
            background-color: %s;
            color: %s;
        }
        
        .content-wrapper {
            background-color: %s;
        }
        
        .main-header {
            background-color: %s;
        }
        
        .main-header .logo,
        .main-header .navbar {
            background-color: %s;
            color: %s;
        }
        
        .main-sidebar {
            background-color: %s;
        }
        
        .sidebar a {
            color: %s !important;
        }
        
        .sidebar a:hover {
            background-color: %s !important;
        }
        
        .box {
            background-color: %s;
            border-color: %s;
        }
        
        .box-header {
            background-color: %s;
            color: %s;
        }
        
        .btn-primary {
            background-color: %s;
            border-color: %s;
        }
        
        .btn-info {
            background-color: %s;
            border-color: %s;
        }
        
        .btn-success {
            background-color: %s;
            border-color: %s;
        }
        
        .btn-warning {
            background-color: %s;
            border-color: %s;
        }
        
        .btn-danger {
            background-color: %s;
            border-color: %s;
        }
        
        .table thead th {
            background-color: %s;
            border-color: %s;
        }
        
        .table-hover tbody tr:hover {
            background-color: %s;
        }
        
        .form-control {
            background-color: %s;
            border-color: %s;
        }
        
        .form-control:focus {
            border-color: %s;
        }
        
        .progress {
            background-color: %s;
        }
        
        .progress-bar {
            background-color: %s;
        }
        
        /* Plot customization */
        .plotly .main-svg {
            background-color: %s !important;
        }
        
        .plotly .gridlayer path {
            stroke: %s !important;
        }
        
        .plotly .gtitle,
        .plotly .xtitle,
        .plotly .ytitle {
            fill: %s !important;
        }
    ",
    theme$background_color,
    theme$text_color,
    theme$background_color,
    theme$header_background,
    theme$header_background,
    theme$header_text,
    theme$sidebar_background,
    theme$sidebar_text,
    theme$sidebar_hover,
    theme$box_background,
    theme$box_border,
    theme$box_header,
    theme$box_header_text,
    theme$button_primary,
    theme$button_primary,
    theme$button_info,
    theme$button_info,
    theme$button_success,
    theme$button_success,
    theme$button_warning,
    theme$button_warning,
    theme$button_danger,
    theme$button_danger,
    theme$table_header,
    theme$table_border,
    theme$table_hover,
    theme$input_background,
    theme$input_border,
    theme$input_focus,
    theme$progress_background,
    theme$progress_bar,
    theme$plot_background,
    theme$plot_gridlines,
    theme$plot_text
    )
    
    tags$style(HTML(css))
}

#' Create theme selector UI
#' @param id Input ID
#' @return Theme selection UI element
createThemeSelector <- function(id) {
    ns <- NS(id)
    
    box(
        title = "Theme Settings",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        
        # Predefined themes
        selectInput(ns("preset_theme"), "Select Theme",
                   choices = c("Default", "Dark", "Light", "Custom")),
        
        # Custom theme settings (shown when Custom is selected)
        conditionalPanel(
            condition = sprintf("input['%s'] == 'Custom'", ns("preset_theme")),
            
            colourInput(ns("primary_color"), "Primary Color",
                       value = "#2C3E50"),
            colourInput(ns("secondary_color"), "Secondary Color",
                       value = "#3498DB"),
            colourInput(ns("text_color"), "Text Color",
                       value = "#2C3E50"),
            colourInput(ns("background_color"), "Background Color",
                       value = "#FFFFFF"),
            
            actionButton(ns("apply_theme"), "Apply Theme",
                        class = "btn-primary btn-block")
        )
    )
}

#' Get predefined theme settings
#' @param theme_name Name of the predefined theme
#' @return Theme settings list
getPredefinedTheme <- function(theme_name) {
    switch(theme_name,
        "Dark" = createCustomTheme(
            primary_color = "#34495E",
            secondary_color = "#2980B9",
            text_color = "#ECF0F1",
            background_color = "#2C3E50"
        ),
        "Light" = createCustomTheme(
            primary_color = "#3498DB",
            secondary_color = "#2980B9",
            text_color = "#2C3E50",
            background_color = "#ECF0F1"
        ),
        # Default theme
        createCustomTheme()
    )
}

#' Create theme preview
#' @param theme Theme settings list
#' @return UI element showing theme preview
createThemePreview <- function(theme) {
    div(
        class = "theme-preview",
        style = sprintf("background-color: %s; padding: 15px; border-radius: 4px;",
                       theme$background_color),
        
        # Header
        div(
            class = "preview-header",
            style = sprintf("background-color: %s; color: %s; padding: 10px; margin-bottom: 10px;",
                          theme$header_background, theme$header_text),
            "Header"
        ),
        
        # Content
        div(
            style = sprintf("color: %s;", theme$text_color),
            
            # Buttons
            div(
                class = "btn-group",
                style = "margin-bottom: 10px;",
                
                tags$button(class = "btn btn-primary",
                           style = sprintf("background-color: %s;",
                                         theme$button_primary),
                           "Primary"),
                tags$button(class = "btn btn-info",
                           style = sprintf("background-color: %s;",
                                         theme$button_info),
                           "Info"),
                tags$button(class = "btn btn-success",
                           style = sprintf("background-color: %s;",
                                         theme$button_success),
                           "Success")
            ),
            
            # Box
            div(
                class = "preview-box",
                style = sprintf("background-color: %s; border: 1px solid %s; padding: 10px; margin-bottom: 10px;",
                              theme$box_background, theme$box_border),
                
                div(
                    style = sprintf("background-color: %s; color: %s; padding: 5px; margin: -10px -10px 10px -10px;",
                                  theme$box_header, theme$box_header_text),
                    "Box Header"
                ),
                "Box Content"
            ),
            
            # Form input
            div(
                class = "form-group",
                tags$input(
                    class = "form-control",
                    style = sprintf("background-color: %s; border-color: %s;",
                                  theme$input_background, theme$input_border),
                    placeholder = "Input Preview"
                )
            ),
            
            # Progress bar
            div(
                class = "progress",
                style = sprintf("background-color: %s;",
                              theme$progress_background),
                div(
                    class = "progress-bar",
                    style = sprintf("width: 60%%; background-color: %s;",
                                  theme$progress_bar),
                    "60%"
                )
            )
        )
    )
}