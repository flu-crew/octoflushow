library(shiny)
library(wilbur)

d <- wilbur::load_current()

shinyServer(function(input, output) {

  d_rct <- reactive({
    subset(d, Date >= input$dateRange[1] & Date <= input$dateRange[2])
  })

  output$basic_plot <- renderPlot({
    wilbur::plot_basic(d_rct(), byMonth=input$byMonth, segment=input$segmentChoice)
  })
})
