test_that("color palette creation works", {
    # Test default theme
    default_colors <- create_color_palette("default")
    expect_type(default_colors, "list")
    expect_equal(default_colors$primary, "#2C3E50")
    
    # Test dark theme
    dark_colors <- create_color_palette("dark")
    expect_type(dark_colors, "list")
    expect_equal(dark_colors$primary, "#375A7F")
    
    # Test light theme
    light_colors <- create_color_palette("light")
    expect_type(light_colors, "list")
    expect_equal(light_colors$primary, "#007BFF")
})

test_that("plot theme creation works", {
    themes <- c("default", "dark", "light")
    
    for (theme in themes) {
        plot_theme <- create_plot_theme(theme)
        expect_s3_class(plot_theme, "theme")
        expect_true(!is.null(plot_theme$text$colour))
    }
})

test_that("dashboard theme creation works", {
    themes <- c("default", "dark", "light")
    
    for (theme in themes) {
        dashboard_theme <- create_dashboard_theme(theme)
        expect_type(dashboard_theme, "list")
        expect_true(!is.null(dashboard_theme$header))
        expect_true(!is.null(dashboard_theme$sidebar))
        expect_true(!is.null(dashboard_theme$box))
    }
})

test_that("theme initialization works", {
    themes <- c("default", "dark", "light")
    
    for (theme in themes) {
        initialize_theme(theme)
        
        # Check that options were set
        expect_equal(getOption("generalanalyser.theme"), theme)
        expect_type(getOption("generalanalyser.colors"), "list")
        expect_s3_class(getOption("generalanalyser.plot_theme"), "theme")
        expect_type(getOption("generalanalyser.dashboard_theme"), "list")
    }
})

test_that("color manipulation functions work", {
    color <- "#2C3E50"
    
    # Test lighten
    lighter <- lighten(color)
    expect_type(lighter, "character")
    expect_match(lighter, "^#[0-9A-Fa-f]{6}$")
    
    # Test darken
    darker <- darken(color)
    expect_type(darker, "character")
    expect_match(darker, "^#[0-9A-Fa-f]{6}$")
    
    # Test alpha
    with_alpha <- alpha(color, 0.5)
    expect_type(with_alpha, "character")
    expect_true(startsWith(with_alpha, "rgba"))
    
    # Test extreme values
    expect_error(lighten(color, 2))  # Amount > 1
    expect_error(darken(color, -1))  # Amount < 0
    expect_error(alpha(color, 1.5))  # Alpha > 1
})