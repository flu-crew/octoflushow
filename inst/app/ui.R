library(shiny)

shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Swine Surveillance App"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("obs", 
                "I do nothing:", 
                min = 1,
                max = 1000, 
                value = 500)
  ),

  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("basic_plot")
  )
))
