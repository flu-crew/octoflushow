library(shiny)

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Swine Surveillance App"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    # FIXME: remove hard-coded range (instead get it from `min(d$Date)` and `max(d$Date)`)
    dateRangeInput(
      "dateRange",
      label = "Date Range",
      start = lubridate::date("2009-11-01"),
      end   = lubridate::date("2019-02-28")),
    checkboxInput(
      "byMonth",
      label = "Plot by month",
      value = TRUE
    ),
    radioButtons(
      "segmentChoice",
      label    = "Segment",
      choices  = list("H1", "H3", "N1", "N2", "PB2", "PB1", "PA", "NP", "M", "NS"),
      selected = "H1"
    )
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("basic_plot")
  )
))
