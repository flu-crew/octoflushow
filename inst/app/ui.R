library(shiny)
library(shinyBS)
require(magrittr)

infile <- system.file("app-data", "A0_Master_List.tab", package="octoflushow")
# Add WGS because that is a column that I add after loading the masterlist
choices <- c(colnames(readr::read_tsv(infile)), "WGS")

names(choices) <- choices
selected <- c("Barcode", "Date", "State", "Subtype", "Strain","GL_Clade","H1","H3","N1","N2", "Constellation", "WGS")
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

#logofile <- system.file("app-data", "logo.png", package="octoflushow")

shinyUI(navbarPage("octoFLUshow",

  tabPanel(
    "Landing page",
    fluidPage(
      includeHTML("landing-page.html")
    )
  ),

  tabPanel(
    "Data",
    fluidPage(
      fluidRow(
        column(2, actionButton("go", "Select Columns")),
        column(2, downloadButton("downloadExcel", "Download Excel")),
        column(2, downloadButton("downloadTAB", "Download TSV")),
        column(6, textAreaInput(
          "strain_selection",
          label = "Select strains (comma-delimited list of barcodes, strains, or regular expressions)",
          value = "",
          rows=2,
          placeholder = "A01104056, Illinois/A01565507, Utah.*201[89]"
        ) %>% shiny::tagAppendAttributes(style = 'width: 100%;')) 
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
        checkboxInput("collapse_n2_bar", "Collapse N2", value=FALSE),
        checkboxInput("collapse_gamma_bar", "Collapse Gamma", value=FALSE),
        checkboxInput("collapse_c4_bar", "Collapse C-IV", value=FALSE),
        radioButtons(
          "segmentChoiceBar",
          label    = "Segment",
          choices  = list("H1", "H3", "N1", "N2", "PB2", "PB1", "PA", "NP", "M", "NS"),
          inline   = TRUE,
          selected = "H1"
        ),
        radioButtons(
          "global_bar",
          label = "HA nomenclature",
          choices = list("US", "global"),
          inline = TRUE,
          selected = "US"
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
    "HA/NA by time",
    sidebarLayout(
     sidebarPanel(
        radioButtons(
          "floorDateByHanaBar",
          label = "Plot dates by",
          choices = list("month", "quarter", "year"),
          inline = TRUE,
          selected = "quarter"
        ),
        checkboxInput("collapse_n2_hana_bar", "Collapse N2", value=FALSE),
        checkboxInput("collapse_gamma_hana_bar", "Collapse Gamma", value=FALSE),
        checkboxInput("collapse_c4_hana_bar", "Collapse C-IV", value=FALSE),
        radioButtons(
          "global_hana_bar",
          label = "HA nomenclature",
          choices = list("US", "global"),
          inline = TRUE,
          selected = "US"
        ),
        div(
          style="display:inline-block;vertical-align:top; width:150px;",
          downloadButton("download_hana_time_plot", "Download Figure")
        )
      ),
      mainPanel(
        plotOutput("hana_time_plot"),
        resize_on_change("hana_time_plot")
      )
    )
  ),

  tabPanel(
    "HA/NA/Const by time",
    sidebarLayout(
     sidebarPanel(
        radioButtons(
          "floorDateByTripleBar",
          label = "Plot dates by",
          choices = list("month", "quarter", "year"),
          inline = TRUE,
          selected = "quarter"
        ),
        checkboxInput("collapse_n2_triple_bar", "Collapse N2", value=FALSE),
        checkboxInput("collapse_gamma_triple_bar", "Collapse Gamma", value=FALSE),
        checkboxInput("collapse_c4_triple_bar", "Collapse C-IV", value=FALSE),
        radioButtons(
          "global_triple_bar",
          label = "HA nomenclature",
          choices = list("US", "global"),
          inline = TRUE,
          selected = "US"
        ),
        div(
          style="display:inline-block;vertical-align:top; width:150px;",
          downloadButton("download_triple_time_plot", "Download Figure")
        )
      ),
      mainPanel(
        plotOutput("triple_time_plot"),
        resize_on_change("triple_time_plot")
      )
    )
  ),

  tabPanel(
    "Clades by state",
    sidebarLayout(
     sidebarPanel(
       checkboxInput("collapse_n2_state", "Collapse N2", value=FALSE),
       checkboxInput("collapse_gamma_state", "Collapse Gamma", value=FALSE),
       checkboxInput("collapse_c4_state", "Collapse C-IV", value=FALSE),
       radioButtons(
         "segmentChoiceState",
         label    = "Segment",
         choices  = list("H1", "H3", "N1", "N2", "PB2", "PB1", "PA", "NP", "M", "NS"),
         inline   = TRUE,
         selected = "H1"
       ),
       radioButtons(
         "global_state",
         label = "HA nomenclature",
         choices = list("US", "global"),
         inline = TRUE,
         selected = "US"
       ),
       radioButtons(
         "fillMethodState",
         label    = "Fill Method",
         choices  = list("count", "clade-max", "state-percentage"),
         inline   = TRUE,
         selected = "count"
       ),
       checkboxInput("state_counts", "Show Counts", value=FALSE),
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
    "Clade heatmap",
    sidebarLayout(
      sidebarPanel(
        checkboxInput("collapse_n2_clade_heatmap", "Collapse N2", value=FALSE),
        checkboxInput("collapse_gamma_heatmap", "Collapse Gamma", value=FALSE),
        checkboxInput("collapse_c4_heatmap", "Collapse C-IV", value=FALSE),
        radioButtons(
          "global_heatmap",
          label = "HA nomenclature",
          choices = list("US", "global"),
          inline = TRUE,
          selected = "US"
        ),
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
  ),

  tabPanel(
    "Constellation heatmap",
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          "global_constellation",
          label = "HA nomenclature",
          choices = list("US", "global"),
          inline = TRUE,
          selected = "US"
        ),
        div(
          style="display:inline-block;vertical-align:top; width:150px;",
          downloadButton("download_constellation_plot", "Download Figure")
        )
      ),
      mainPanel(
        plotOutput("constellation_plot"),
        resize_on_change("constellation_plot")
      )
    )
  )
  ))
