library(shiny)
library(wilbur)

d <- wilbur::load_current()

shinyServer(function(input, output) {

  d_rct <- reactive({
      row_indices <- input$raw_data_table_rows_all
      d[row_indices, ]
  })

  output$selected_plot <- renderPlot({
    if(input$plotChoice == "state"){
      print(state_plot_rct())
    } else {
      print(basic_plot_rct())
    }
  })


  basic_plot_rct <- reactive({
    plot_basic(d_rct(), floorDateBy=input$floorDateBy, segment=input$segmentChoice)
  })

  state_plot_rct <- reactive({
    facetMaps(d_rct(), segment=input$segmentChoice)
  })

  output$download_selected_plot <- downloadHandler(
    filename = function() {
      if(input$plotChoice == "state"){
        "swine-survey-state_plot.pdf"
      } else {
        "swine-survey-basic_plot.pdf"
      }
    },
    content = function(file){
      if(input$plotChoice == "state"){
        ggplot2::ggsave(file, state_plot_rct(), device="pdf", width=8, height=6)
      } else {
        ggplot2::ggsave(file, basic_plot_rct(), device="pdf", width=8, height=12)
      }
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

  output$raw_data_table <- DT::renderDataTable(
    d,
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
      dom = 'RCT<"clear">lfrtip',
      buttons = I('colvis'),
      autoWidth=FALSE,
      orderMulti=TRUE,
      searching=TRUE,
      search.regex=TRUE
    )
  )
})
