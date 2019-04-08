library(shiny)
library(wilbur)

d <- wilbur::load_current()

shinyServer(function(input, output) {

  d_rct <- reactive({
    subset(d, Date >= input$dateRange[1] & Date <= input$dateRange[2])
  })

  basic_plot_rct <- reactive({
    wilbur::plot_basic(d_rct(), byMonth=input$byMonth, segment=input$segmentChoice)
  })

  output$basic_plot <- renderPlot({ print(basic_plot_rct()) })

  output$downloadFigure <- downloadHandler(
    filename = function() {"swine-survey-plot.pdf"},
    content = function(file) {
      ggplot2::ggsave(file, basic_plot_rct(), device="pdf", width=8, height=6)
    }
  )
})
