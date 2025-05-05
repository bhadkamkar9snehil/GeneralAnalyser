test_that("sidebar menu creation works", {
    menu <- create_sidebar_menu()
    expect_s3_class(menu, "shiny.tag")
    expect_true(inherits(menu, "nav"))
    expect_true(length(menu$children) > 0)
})

test_that("data input UI creation works", {
    ui <- create_data_input_ui("test")
    expect_s3_class(ui, "shiny.tag")
    
    # Check for required elements
    html <- as.character(ui)
    expect_true(grepl('input.*file', html))
    expect_true(grepl('input.*header', html))
    expect_true(grepl('input.*sep', html))
    expect_true(grepl('input.*quote', html))
})

test_that("analysis options UI creation works", {
    # Test for each analysis type
    types <- c("time_series", "regression", "classification", 
               "clustering", "anomaly")
    
    for (type in types) {
        ui <- create_analysis_options("test", type)
        expect_s3_class(ui, "shiny.tag")
        
        html <- as.character(ui)
        expect_true(grepl('box', html))
        expect_true(grepl('Analysis Options', html))
    }
    
    # Test specific options for each type
    ts_ui <- create_analysis_options("test", "time_series")
    expect_true(grepl('horizon', as.character(ts_ui)))
    
    reg_ui <- create_analysis_options("test", "regression")
    expect_true(grepl('target', as.character(reg_ui)))
    
    clf_ui <- create_analysis_options("test", "classification")
    expect_true(grepl('train_ratio', as.character(clf_ui)))
})

test_that("results panel creation works", {
    # Create sample results
    results <- list(
        metrics = list(
            accuracy = 0.95,
            rmse = 0.05
        )
    )
    
    ui <- create_results_panel("test", results)
    expect_s3_class(ui, "shiny.tag")
    
    html <- as.character(ui)
    expect_true(grepl('Plot', html))
    expect_true(grepl('Metrics', html))
    expect_true(grepl('Table', html))
})

test_that("notification creation works", {
    types <- c("success", "warning", "error")
    messages <- paste("Test", types, "message")
    
    for (i in seq_along(types)) {
        notif <- create_notification(types[i], messages[i])
        expect_type(notif, "list")
    }
})

test_that("progress UI creation works", {
    ui <- create_progress_ui("test")
    expect_s3_class(ui, "shiny.tag")
    
    html <- as.character(ui)
    expect_true(grepl('progress-container', html))
    expect_true(grepl('Cancel', html))
})

test_that("settings panel creation works", {
    ui <- create_settings_panel("test")
    expect_s3_class(ui, "shiny.tag")
    
    html <- as.character(ui)
    expect_true(grepl('Theme', html))
    expect_true(grepl('Plot Height', html))
    expect_true(grepl('Rows per Page', html))
    expect_true(grepl('Export Format', html))
})

test_that("help tooltip creation works", {
    tooltip <- create_help_tooltip("test", "Help text")
    expect_s3_class(tooltip, "shiny.tag")
    
    html <- as.character(tooltip)
    expect_true(grepl('tooltip', html))
    expect_true(grepl('Help text', html))
})

test_that("confirmation dialog creation works", {
    dialog <- create_confirmation_dialog("test", "Test Title", "Test Message")
    expect_s3_class(dialog, "shiny.tag")
    
    html <- as.character(dialog)
    expect_true(grepl('Test Title', html))
    expect_true(grepl('Test Message', html))
    expect_true(grepl('Cancel', html))
    expect_true(grepl('Confirm', html))
})

test_that("error message creation works", {
    # Test basic error message
    error <- create_error_message("Test Error")
    expect_s3_class(error, "shiny.tag")
    expect_true(grepl('Test Error', as.character(error)))
    
    # Test error message with details
    error_with_details <- create_error_message("Test Error", "Error details")
    html <- as.character(error_with_details)
    expect_true(grepl('Test Error', html))
    expect_true(grepl('Error details', html))
})