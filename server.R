
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
source("chooser.R")

selectData <- function (input){
  if(input$radio == 2){
    req(input$file1)
    df <- read.csv(input$file1$datapath, sep = input$sep)
    df
  }else{
    mtcars
  }
}

function(input, output) {
  output$fields <- renderUI({
    fluidPage(
      # Horizontal line ----
      tags$hr(),
      chooserInput("mychooser", "Available frobs", "Selected frobs",
                   names(selectData(input)), c(), size = 10, multiple = TRUE
      ),
      verbatimTextOutput("selection")
    )
  })
  
  output$contents <- DT::renderDataTable({
      DT::datatable(selectData(input))
  })
  
  output$summary <- renderPrint({
    summary(selectData(input))
  })
}

