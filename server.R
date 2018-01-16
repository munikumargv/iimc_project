
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

shinyServer(function(input, output) {
  
  output$ui <- renderUI({"Apollo 13"})
  
  output$contents <- DT::renderDataTable({
    if(input$radio == 2){
      req(input$file1)
      df <- read.csv(input$file1$datapath, sep = input$sep)
      DT::datatable(df)
    }
    else{
      DT::datatable(mtcars)
    }
  })
  
  output$summary <- renderPrint({
    if(input$radio == 2){
    req(input$file1)
    df <- read.csv(input$file1$datapath, sep = input$sep)
    summary(df)
    }
    else{
      summary(mtcars)
    }
  })
}
)
