
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

shinyServer(function(input, output) {
  
  output$contents <- DT::renderDataTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath, sep = input$sep)
    DT::datatable(df)
  })
  
  output$summary <- renderPrint({
    req(input$file1)
    df <- read.csv(input$file1$datapath, sep = input$sep)
    summary(df)
  })
}
)
