
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

shinyServer(function(input, output) {

  output$txtout <- renderText({
    paste(input$txt, input$slider, format(input$date), sep = ", ")
  })
  output$table <- renderTable({
    head(mtcars, 4)
  })
  
  output$contents <- DT::renderDataTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath, sep = input$sep)
    DT::datatable(df)
  })
}
)
