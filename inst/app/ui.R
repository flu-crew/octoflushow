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
