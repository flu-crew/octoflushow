library(shiny)

require(magrittr)

d <- octoflushow::load_current()

# update data in various ways prior to plotting
plot_munge <- function(d, collapse_n2_clade, collapse_gamma_clade, collapse_c4_clade, global_ha="us"){
  if(global_ha == "global"){
  	# regex must match all global clades in config.yaml
    d$H1 = ifelse(grepl("^1[A-Z]", d$GL_Clade), as.character(d$GL_Clade), NA)
    d$H3 = ifelse(grepl("^(3|[12][0-9]{3})|[Hh]uman", d$GL_Clade), as.character(d$GL_Clade), NA)
  }
  if(collapse_n2_clade){
    d = octoflushow::collapse_n2(d)
  }
  if(collapse_gamma_clade){
    d = octoflushow::collapse_gamma(d)
  }
  if(collapse_c4_clade){
    d = octoflushow::collapse_c4(d)
  }
  d
}

server <- function(input, output, session) {
  # input$data_is_loaded is set to true the first time the interactive data
  # table is # opened. If the data table has not been opened, then the #
  # input$raw_data_table_rows_all term is undefined.
  data_is_loaded <- reactiveVal(value=FALSE)

  d_rct <- reactive({
      if(data_is_loaded()){
        # The data table has been loaded, so only view the selected rows
        d[input$raw_data_table_rows_all, ]
      } else {
        # The data table has not been loaded, so view everything
        d
      }
  })

  output$time_plot <- renderPlot({
      print(time_plot_rct())
  })

  output$hana_time_plot <- renderPlot({
      print(hana_time_plot_rct())
  })

  output$triple_time_plot <- renderPlot({
      print(triple_time_plot_rct())
  })

  output$state_plot <- renderPlot({
      print(state_plot_rct())
  })
  
  output$heatmap_plot <- renderPlot({
    print(heatmap_plot_rct())
  })
  
  output$constellation_plot <- renderPlot({
    print(constellation_plot_rct())
  })

  time_plot_rct <- reactive({
    plot_munge(d_rct(), input$collapse_n2_bar, input$collapse_gamma_bar, input$collapse_c4_bar, input$global_bar) %>%
      octoflushow::plot_basic(floorDateBy=input$floorDateBy, segment=input$segmentChoiceBar)
  })

  hana_time_plot_rct <- reactive({
    plot_munge(d_rct(), input$collapse_n2_hana_bar, input$collapse_gamma_hana_bar, input$collapse_c4_hana_bar, input$global_hana_bar) %>%
      octoflushow::hana_barplots(floorDateBy=input$floorDateByHanaBar, global=input$global_hana_bar == "global")
  })

  triple_time_plot_rct <- reactive({
    plot_munge(d_rct(), input$collapse_n2_triple_bar, input$collapse_gamma_triple_bar, input$collapse_c4_triple_bar, input$global_triple_bar) %>%
      octoflushow::triple_barplots(floorDateBy=input$floorDateByTripleBar, global=input$global_triple_bar == "global")
  })

  state_plot_rct <- reactive({
    plot_munge(d_rct(), input$collapse_n2_state, input$collapse_gamma_state, input$collapse_c4_state, input$global_state) %>%
      octoflushow::facetMaps(segment=input$segmentChoiceState, normalization=input$fillMethodState, count=input$state_counts)
  })
  
  heatmap_plot_rct <- reactive({
    plot_munge(d_rct(), input$collapse_n2_clade_heatmap, input$collapse_gamma_heatmap, input$collapse_c4_heatmap, input$global_heatmap) %>%
      octoflushow::heatmap_HANA(totals=TRUE)
  })
  
  constellation_plot_rct <- reactive({
    plot_munge(d_rct(), TRUE, TRUE, TRUE, input$global_constellation) %>%
    octoflushow::plot_constellation()
  })

  make_downloader <- function(function_name){
    reactive_function = get(glue::glue("{function_name}_rct"))
    width_field = glue::glue("shiny_width_{function_name}")
    height_field = glue::glue("shiny_height_{function_name}")
    downloadHandler(
      filename = function(){glue::glue("swine-survey-{function_name}.pdf")},
      content = function(file){
          ggplot2::ggsave(file,
            reactive_function(),
            device="pdf",
            width = input[[width_field]]/72,
            height = input[[height_field]]/72)
      }
    )
  }

  output$download_time_plot <- make_downloader("time_plot")
  output$download_hana_time_plot <- make_downloader("hana_time_plot")
  output$download_triple_time_plot <- make_downloader("triple_time_plot")
  output$download_state_plot <- make_downloader("state_plot")
  output$download_heatmap_plot <- make_downloader("heatmap_plot")
  output$download_constellation_plot <- make_downloader("constellation_plot")

  output$downloadExcel <- downloadHandler(
    filename = 'swine-surveillance-data.xlsx',
    content = function(file) {
      # See this example for an explanation: https://yihui.shinyapps.io/DT-info/
      # <output id>_rows_all gets all selected rows on all pages
      # <output id>_rows_current gets only the selected rows on the current page
      row_indices <- input$raw_data_table_rows_all
      selected_data <- d[row_indices, input$selected_columns]
      writexl::write_xlsx(selected_data, path=file)
    }
  )

  output$downloadTAB <- downloadHandler(
    filename = 'swine-surveillance-data.txt',
    content = function(file) {
      # See this example for an explanation: https://yihui.shinyapps.io/DT-info/
      # <output id>_rows_all gets all selected rows on all pages
      # <output id>_rows_current gets only the selected rows on the current page
      row_indices <- input$raw_data_table_rows_all
      selected_data <- d[row_indices, input$selected_columns]
      readr::write_tsv(selected_data, file=file)
    }
  )

  d_col_rct <- reactive({
      if(nchar(input$strain_selection) > 0){
        pattern <- strsplit(input$strain_selection, "[\n\r]+|[,;]+") %>%
            unlist %>% 
            sub(pattern="\\s*([^ \t]*)\\s*", replacement="\\1", perl=TRUE) %>%
            Filter(f=function(x) nchar(x) > 0) %>%
            paste0(collapse="|")
        d[grepl(pattern, d$Strain), input$selected_columns]
      } else {
        d[, input$selected_columns]
      }
  })

  output$raw_data_table <- DT::renderDataTable(
    {
      data_is_loaded(TRUE)
      d_col_rct()
    },
    filter="top",
    rownames=FALSE,
    style="bootstrap",
    selection=list(
      mode="single",
      target="column",
      selected=0
    ),
    extensions = c("ColReorder"),
    options = list(
      dom = 'Blfrtip',
      autoWidth=FALSE,
      orderMulti=TRUE,
      searching=TRUE,
      search.regex=TRUE,
      lengthMenu=list(c(10,25,50,100,100), c('10','25', '50', '100', '1000')),
      pageLength=25
    )
  )
}

shinyServer(server)
