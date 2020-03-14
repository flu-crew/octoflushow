library(shiny)
library(shinyBS)
library(wilbur)
#library(png)

infile <- system.file("app-data", "A0_Master_List.xlsx", package="wilbur")
sheet <- "Data"
choices <- colnames(readxl::read_excel(infile, sheet = sheet, col_types = "text", n_max=3))
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

#logofile <- system.file("app-data", "logo.png", package="wilbur")

shinyUI(navbarPage("octoFLU show",
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
    "Landing page",
    fluidPage(
      h1("Influenza A Virus in Swine Surveillence"),
      p("The purpose of this site is to summarize the phylogenetic clade diversity of Influenza A Virus strains submitted to the passive surveillence program. octoFLU show provides a graphical dashboard of these changes and is maintained by Flu-Crew."),
#      tags$img(src = "https://github.com/flu-crew/octoFLU/blob/master/img/octoFLU_revised_V3-01.png", width = "100px", height = "100px"),
#      tags$img(src = logofile, width = "100px", height = "100px"),
      h2("Data"),
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
