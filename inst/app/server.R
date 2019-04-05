library(shiny)
library(wilbur)

shinyServer(function(input, output) {
  output$basic_plot <- renderPlot({
    d <- wilbur::load_current()
    wilbur::plot_basic(d)
  })
})
