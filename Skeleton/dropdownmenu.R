library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(
      leftUi = tagList(
        dropdownButton(
          label = "Controls",
          icon = icon("sliders-h"),
          status = "primary",
          circle = FALSE,
          sliderInput(
            inputId = "n",
            label = "Number of observations",
            min = 10, max = 100, value = 30
          ),
          prettyToggle(
            inputId = "na",
            label_on = "NAs kept",
            label_off = "NAs removed",
            icon_on = icon("check"),
            icon_off = icon("trash")
          )
        ),
        dropdownMenu(
          type = "messages", 
          badgeStatus = "success",
          messageItem(from = "Support Team", message = "This is the content of a message.", time = "5 mins"),
          messageItem(from = "Support Team", message = "This is the content of another message.", time = "2 hours"),
          messageItem(from = "New User", message = "Can I get some help?", time = "Today")
        )
      )
    ),
    sidebar = dashboardSidebar(),
    body = dashboardBody(
      setShadow(class = "dropdown-menu")
    ),
    title = "DashboardPage"
  ),
  server = function(input, output) { }
)
