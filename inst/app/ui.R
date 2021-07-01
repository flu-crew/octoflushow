library(shiny)
library(shinyBS)

infile <- system.file("app-data", "A0_Master_List.tab", package="octoflushow")
choices <- colnames(readr::read_tsv(infile))

names(choices) <- choices
selected <- c("Barcode", "Date", "State", "Subtype", "Strain","H1","H3","N1","N2", "Constellation")
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

shinyUI(navbarPage("octoFLU Show",

  tabPanel(
    "Landing page",
    fluidPage(
      h1("USDA Influenza A Virus in Swine Surveillence"),
      p("The purpose of this site is to summarize the genetic diversity of influenza A virus in swine submitted to the national USDA influenza A virus in swine passive surveillence program. octoFLU Show provides a graphical dashboard of these changes and is maintained by the Flu-Crew at the National Animal Disease Center, USDA-ARS."),
#      tags$img(src = "https://github.com/flu-crew/octoFLU/blob/master/img/octoFLU_revised_V3-01.png", width = "100px", height = "100px"),
#      tags$img(src = logofile, width = "100px", height = "100px"),
      h2("Support"),
      p("octoFLU Show is in active development. If you need to report a problem, ask a question, or have a suggested enhancement, please contact Tavis Anderson, Zebulun Arendsee, Jennifer Chang, or Amy Vincent."), 
      h3("How to cite"),
      p("If you use data provided by octoFLU Show in your work, please credit in the following format:"),
#      p("octoFLU show, https://flu-crew.org/octoflushow/, (data retrieved <?php echo (new DateTime())->format('d M, Y');?>)"),
      p("octoFLU Show, https://flu-crew.org/octoflushow/, (data retrieved on dd/MM/YYYY)."),
      p("Citation is subject to change pending publication."),
      h2("Data"),
      p("Use the Data tab to select the surveillance data you would like to graph."),
      p("The Data tab contains a table of all influenza A virus in swine isolates with A0 barcodes. Use this tab to filter and focus on subsets of the data. Filters will affect which isolates are included in the plots on the next three tabs."),
      p("Common filters include:"),
      tags$ul(
      tags$li("Subset to any isolates submitted in the last two years (select date and drag the bar)"),
      tags$li("Focus on isolates collected from a particular state (type in two letter state code in state column)"),
      tags$li("Determine which isolates have a particular clade (type the clade name in the clade column) and regional information (click on Clades-by-state tab)")
      ),
      h2("Clades by time"),
      p("Temporal patterns of clades represented in a barchart. Select a segment (H1, H3, PB2,...NS) of interest and the phylogenetic clade of that segment is shown in stacked barchart by time."),
      h2("Clades by state"),
      p("Regional patterns of clades represented in a ggmap. Select a segment (H1, H3, PB2, ... NS) of interest and the map is faceted by phylogenetic clade with isolate counts."),
      h2("Clades by heatmap"),
      p("Hemagglutinin (HA) and neuraminidase (NA) patterns represented in a heatmap. HA clades are represented by rows while NA clades are represented by columns. Heatmap can be used to determine most common HA-NA clade pairing for a particular data filter."),
  h2("Constellation heatmap"),
  p("Internal gene constellation patterns of subtypes where T=TRIG, P=Pandemic, and V=LAIV-related. Strains with mixed subtypes or mixed internal constellations were filtered out. The heatmap can be used to determine the most common Gene Constellation from the left Totals column."),
  h3("Acknowledgements:"),
  p("We were supported by USDA-ARS, USDA-APHIS, and by an NIH-National Institute of Allergy and Infectious Diseases (NIAID) interagency agreement associated with CRIP (Center of Research in Influenza Pathogenesis), an NIAID-funded Center of Excellence in Influenza Research and Surveillance (CEIRS, HHSN272201400008C). We are grateful to the pork producers, swine veterinarians, and laboratories for participating in the USDA Influenza Virus Surveillance System for swine. This research used resources provided by the SCINet project of the USDA Agricultural Research Service, ARS project number 0500-00093-001-00-D.")
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
        checkboxInput("collapse_n2_clade_heatmap", "Collapse N2", value=FALSE),
        checkboxInput("collapse_gamma_heatmap", "Collapse Gamma", value=FALSE),
        checkboxInput("collapse_c4_heatmap", "Collapse C-IV", value=FALSE),
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
