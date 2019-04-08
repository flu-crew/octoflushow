library(shiny)

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Swine Surveillance App"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    # FIXME: remove hard-coded range (instead get it from `min(d$Date)` and `max(d$Date)`)
    dateRangeInput("dateRange",
                   label="Date Range",
                   start=lubridate::date("2009-11-01"),
                   end=lubridate::date("2019-02-28"))
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("basic_plot")
  )
))
