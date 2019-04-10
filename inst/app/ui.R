library(shiny)

shinyUI(navbarPage("Swine Surveillance App",
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
        ),
        div(
          style="display:inline-block;vertical-align:top; width:150px;",
          downloadButton("download_selected_plot", "Download Figure")
        ),
        div(
          style="display:inline-block;vertical-align:top; width:150px;",
          downloadButton("downloadData", "Download Data")
        )
      ),
      mainPanel(
        plotOutput("selected_plot")
      )
    ),
    DT::dataTableOutput("raw_data_table")
  )
))
