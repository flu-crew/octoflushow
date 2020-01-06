library(shiny)
library(wilbur)

d <- wilbur::load_current()

shinyServer(function(input, output) {

  d_rct <- reactive({
      row_indices <- input$raw_data_table_rows_all
      d[row_indices, ]
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


  basic_plot_rct <- reactive({
    plot_basic(d_rct(), floorDateBy=input$floorDateBy, segment=input$segmentChoiceBar)
  })

  state_plot_rct <- reactive({
    facetMaps(d_rct(), segment=input$segmentChoiceState)
  })
  
  heatmap_plot_rct <- reactive({
    plot_heatmap(d_rct())
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

  output$downloadData <- downloadHandler(
    filename = 'swine-surveillance-data.xlsx',
    content = function(file) {
      # See this example for an explanation: https://yihui.shinyapps.io/DT-info/
      # <output id>_rows_all gets all selected rows on all pages
      # <output id>_rows_current gets only the selected rows on the current page
      row_indices <- input$raw_data_table_rows_all
      selected_data <- d[row_indices, ]
      writexl::write_xlsx(selected_data, path=file)
    }
  )

  d_col_rct <- reactive({
      d[, input$selected_columns]
  })

  output$raw_data_table <- DT::renderDataTable(
    d_col_rct(),
    filter="top",
    rownames=FALSE,
    style="bootstrap",
    selection=list(
      mode="single",
      target="column",
      selected=0
    ),
    extensions = c("Buttons", "ColReorder"),
    options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      autoWidth=FALSE,
      orderMulti=TRUE,
      searching=TRUE,
      search.regex=TRUE
    )
  )
})
