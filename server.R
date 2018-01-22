
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
  output$out2 <- renderPrint(input$in2)
  
  output$out3 <- renderPrint(input$in3)
  
  output$selection <- renderPrint(
    input$mychooser
  )
  
  output$fields <- renderUI({
    fluidPage(
      fluidRow(
        column(4,
          # Horizontal line ----
          tags$br(),
          h3("Step 1"),
          h5("Choose Predictors:"),
          chooserInput("mychooser", "Available frobs", "Selected frobs",
                       names(selectData(input)), c(), size = 10, multiple = TRUE
          )#,
          #tags$br(),
          #verbatimTextOutput("selection")
        ),
        column(4,
               # Horizontal line ----
               tags$br(),
               h3("Step 2"),
               h5("Choose Outcome Variable:"),
               selectInput('in2', 'Options', names(selectData(input)), selectize=FALSE),
               verbatimTextOutput('out2')
        ),
        column(4,
               # Horizontal line ----
               tags$br(),
               h3("Step 3"),
               h5("Choose Prediction Models:"),
               selectInput('in3', 'Options', c("Logistic", "Naive Bayes", "Neural Networks", "SVM"), multiple=TRUE, selectize=TRUE)#,
               #verbatimTextOutput('out3')
        )        
      ),
      fluidRow(
        tags$hr()
      )
    )
  })
  
  output$contents <- DT::renderDataTable({
      DT::datatable(selectData(input))
  })
  
  output$summary <- renderPrint({
    summary(selectData(input))
  })
}

