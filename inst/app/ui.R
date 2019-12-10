library(shiny)
library(shinyBS)
library(wilbur)

infile <- system.file("app-data", "A0_Master_List.xlsx", package="wilbur")
sheet <- "Data"
choices <- colnames(readxl::read_excel(infile, sheet = sheet, col_types = "text", n_max=3))
names(choices) <- choices
selected <- c("Barcode", "Date", "State", "Subtype", "Strain", "Constellation")
stopifnot(all(selected %in% choices))

shinyUI(navbarPage("Swine Surveillance App",
  tabPanel(
    "Data",
    fluidPage(
      fluidRow(
        column(7),
        column(2, actionButton("go", "Select Columns")),
        column(2, downloadButton("downloadData", "Download Data"))
      ),
      fluidRow(
        column(12,
          DT::dataTableOutput("raw_data_table"),
          bsModal(
            "modalSelectColumns",
            "Select Columns",
            "go",
            size = "large",
            checkboxGroupInput(
              "selected_columns",
              "Select columns",
              choices=choices,
              selected=selected,
              inline=TRUE
            )
          )
        )
      )
    )
  ),
  tabPanel(
    "Clades by time",
    sidebarLayout(
     sidebarPanel(
        radioButtons(
          "floorDateBy",
          label = "Plot dates by",
          choices = list("month", "quarter", "year"),
          inline = TRUE,
          selected = "quarter"
        ),
        radioButtons(
          "segmentChoice",
          label    = "Segment",
          choices  = list("H1", "H3", "N1", "N2", "PB2", "PB1", "PA", "NP", "M", "NS"),
          inline   = TRUE,
          selected = "H1"
        ),
        div(
          style="display:inline-block;vertical-align:top; width:150px;",
          downloadButton("download_time_plot", "Download Figure")
        )
      ),
      mainPanel(
        plotOutput("time_plot"),
	tags$script("$(document).on('shiny:sessioninitialized',function(event){var clientWidth = document.getElementById('time_plot').clientWidth;console.log(clientWidth);var clientHeight = document.getElementById('time_plot').clientHeight; Shiny.onInputChange('shiny_width',clientWidth); Shiny.onInputChange('shiny_height',clientHeight);})"),
	tags$script("jQuery(window).resize(function(){var clientWidth = document.getElementById('time_plot').clientWidth;console.log(clientWidth);var clientHeight = document.getElementById('time_plot').clientHeight; Shiny.onInputChange('shiny_width',clientWidth); Shiny.onInputChange('shiny_height',clientHeight);})")
      )
    )
  ),
  tabPanel(
    "Clades by state",
    sidebarLayout(
     sidebarPanel(
        div(
          style="display:inline-block;vertical-align:top; width:150px;",
          downloadButton("download_state_plot", "Download Figure")
        )
      ),
      mainPanel(
        plotOutput("state_plot"),
	tags$script("$(document).on('shiny:sessioninitialized',function(event){var clientWidth = document.getElementById('state_plot').clientWidth;console.log(clientWidth);var clientHeight = document.getElementById('state_plot').clientHeight; Shiny.onInputChange('shiny_width',clientWidth); Shiny.onInputChange('shiny_height',clientHeight);})"),
	tags$script("jQuery(window).resize(function(){var clientWidth = document.getElementById('state_plot').clientWidth;console.log(clientWidth);var clientHeight = document.getElementById('state_plot').clientHeight; Shiny.onInputChange('shiny_width',clientWidth); Shiny.onInputChange('shiny_height',clientHeight);})")
      )
    )
  ),
  tabPanel(
    "Clades by heatmap",
    sidebarLayout(
      sidebarPanel(
        div(
          style="display:inline-block;vertical-align:top; width:150px;",
          downloadButton("download_heatmap_plot", "Download Figure")
        )
      ),
      mainPanel(
        plotOutput("heatmap_plot")#,
#        tags$script("$(document).on('shiny:sessioninitialized',function(event){var clientWidth = document.getElementById('state_plot').clientWidth;console.log(clientWidth);var clientHeight = document.getElementById('state_plot').clientHeight; Shiny.onInputChange('shiny_width',clientWidth); Shiny.onInputChange('shiny_height',clientHeight);})"),
#        tags$script("jQuery(window).resize(function(){var clientWidth = document.getElementById('state_plot').clientWidth;console.log(clientWidth);var clientHeight = document.getElementById('state_plot').clientHeight; Shiny.onInputChange('shiny_width',clientWidth); Shiny.onInputChange('shiny_height',clientHeight);})")
      )
    ),
    h1("asdf")
  )
  
))
