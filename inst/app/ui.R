library(shiny)

shinyUI(navbarPage("Swine Surveillance App",
  tabPanel(
    "Raw Data",
    DT::dataTableOutput("raw_data_table"),
    downloadButton("downloadData", "Download Data")
  ),
  tabPanel(
    "Basic plots",
    sidebarLayout(
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
          inline   = TRUE,
          selected = "H1"
        ),
        radioButtons(
          "plotChoice",
          label = "What do you want to plot?", 
          choices = list("basic", "state"),
          inline = TRUE,
          selected = "basic"
        )
      ),
      mainPanel(
        plotOutput("selected_plot"),
        downloadButton("download_selected_plot", "Download Figure")
      )
  ))
))
