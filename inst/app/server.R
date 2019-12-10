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


  basic_plot_rct <- reactive({
    plot_basic(d_rct(), floorDateBy=input$floorDateBy, segment=input$segmentChoice)
  })

  state_plot_rct <- reactive({
    facetMaps(d_rct(), segment=input$segmentChoice)
  })

  output$download_time_plot <- downloadHandler(
    filename = function() {
      if(input$plotChoice == "state"){
        "swine-survey-state_plot.pdf"
      } else {
        "swine-survey-basic_plot.pdf"
      }
    },
    content = function(file){
      if(input$plotChoice == "state"){
#        ggplot2::ggsave(file, state_plot_rct(), device="pdf", width=8, height=6)
        ggplot2::ggsave(file, state_plot_rct(), device="pdf", width = input$shiny_width/72, height=input$shiny_height/72)
      } else {
#        ggplot2::ggsave(file, basic_plot_rct(), device="pdf", width=8, height=12)
        ggplot2::ggsave(file, basic_plot_rct(), device="pdf", width = input$shiny_width/72, height = input$shiny_height/72)
      }
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
