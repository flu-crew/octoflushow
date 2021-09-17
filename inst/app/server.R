library(shiny)
require(magrittr)

d <- octoflushow::load_current()


# update data in various ways prior to plotting
plot_munge <- function(d, collapse_n2_clade, collapse_gamma_clade, collapse_c4_clade){
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
      print(basic_plot_rct())
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

  basic_plot_rct <- reactive({
    plot_munge(d_rct(), input$collapse_n2_bar, input$collapse_gamma_bar, input$collapse_c4_bar) %>%
      octoflushow::plot_basic(floorDateBy=input$floorDateBy, segment=input$segmentChoiceBar)
  })

  state_plot_rct <- reactive({
    plot_munge(d_rct(), input$collapse_n2_state, input$collapse_gamma_state, input$collapse_c4_state) %>%
      octoflushow::facetMaps(segment=input$segmentChoiceState, normalization=input$fillMethodState)
  })
  
  heatmap_plot_rct <- reactive({
    plot_munge(d_rct(), input$collapse_n2_clade_heatmap, input$collapse_gamma_heatmap, input$collapse_c4_heatmap) %>%
      octoflushow::heatmap_HANA(totals=TRUE)
  })
  
  constellation_plot_rct <- reactive({
    plot_munge(d_rct(), TRUE, TRUE, TRUE) %>%
    octoflushow::plot_constellation()
  })

  output$download_time_plot <- downloadHandler(
    filename = function(){"swine-survey-time_plot.pdf"},
    content = function(file){
        ggplot2::ggsave(file, basic_plot_rct(), device="pdf", width = input$shiny_width_time_plot/72, height = input$shiny_height_time_plot/72)
    }
  )
  
  output$download_state_plot <- downloadHandler(
    filename = function(){"swine-survey-state_plot.pdf"},
    content = function(file){
      ggplot2::ggsave(file, state_plot_rct(), device="pdf", width = input$shiny_width_state_plot/72, height = input$shiny_height_state_plot/72)
    }
  )
  
  output$download_heatmap_plot <- downloadHandler(
    filename = function(){"swine-survey-heatmap_plot.pdf"},
    content = function(file){
      ggplot2::ggsave(file, heatmap_plot_rct(), device="pdf", width = input$shiny_width_heatmap_plot/72, height = input$shiny_height_heatmap_plot/72)
    }
  )
  
  output$download_constellation_plot <- downloadHandler(
    filename = function(){"swine-survey-constellation_plot.pdf"},
    content = function(file){
      ggplot2::ggsave(file, constellation_plot_rct(), device="pdf", width = input$shiny_width_constellation_plot/72, height = input$shiny_height_constellation_plot/72)
    }
  )

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
            sub(pattern=" *([^ \t]*) *", replacement="\\1") %>%
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
