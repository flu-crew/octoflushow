library(shiny)
library(shinyBS)
library(wilbur)

infile <- system.file("app-data", "A0_Master_List.xlsx", package="wilbur")
sheet <- "Data"
choices <- colnames(readxl::read_excel(infile, sheet = sheet, col_types = "text", n_max=3))
names(choices) <- choices
selected <- c("Barcode", "Date", "State", "Subtype", "Strain", "Constellation")
stopifnot(all(selected %in% choices))

resize_on_change <- function(elementID){
  js <- sprintf("
    $(document).on('shiny:visualchange',function(event){
      var clientWidth_%s = document.getElementById('%s').clientWidth;
      var clientHeight_%s = document.getElementById('%s').clientHeight;
      Shiny.setInputValue('shiny_width_%s', clientWidth_%s);
      Shiny.setInputValue('shiny_height_%s', clientHeight_%s);
      console.log('on visual change');
      console.log(clientWidth_%s);
      console.log(clientHeight_%s);
    })", elementID, elementID, elementID, elementID, elementID, elementID, elementID, elementID, elementID, elementID, elementID)
  tags$script(js)
}

shinyUI(navbarPage("Swine Surveillance App",
  tabPanel(
    "Landing page",
    fluidPage(
      h1("THIS IS HOW YOU DO THING 1")
      h1("THIS IS HOW YOU DO THING 2")
      h1("THIS IS HOW YOU DO THING 3")
      h1("EXAMPLE FIGURE!!!!!!!")
      h1("CLICK HERE TO SEE A QUARTERLY REPORT !!!! :)")
    )
  ),
  tabPanel(
    "Data",
    fluidPage(
      fluidRow(
        column(5),
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
          "segmentChoiceBar",
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
        resize_on_change("time_plot")
      )
    )
  ),
  tabPanel(
    "Clades by state",
    sidebarLayout(
     sidebarPanel(
       radioButtons(
         "segmentChoiceState",
         label    = "Segment",
         choices  = list("H1", "H3", "N1", "N2", "PB2", "PB1", "PA", "NP", "M", "NS"),
         inline   = TRUE,
         selected = "H1"
       ),
        div(
          style="display:inline-block;vertical-align:top; width:150px;",
          downloadButton("download_state_plot", "Download Figure")
        )
      ),
      mainPanel(
        plotOutput("state_plot"),
        resize_on_change("state_plot")
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
        plotOutput("heatmap_plot"),
        resize_on_change("heatmap_plot")
      )
    )
  )
))
