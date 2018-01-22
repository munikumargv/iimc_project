
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(mlbench)
data(PimaIndiansDiabetes)
source("chooser.R")

selectData <- function (input){
  if(input$radio == 2){
    req(input$file1)
    df <- read.csv(input$file1$datapath, sep = input$sep)
    df
  }else{
    PimaIndiansDiabetes
  }
}

#
logitModel <- function(input){
  my.data <- selectData(input)
  outcome <- renderPrint(input$in2)
  #predictors <- renderPrint(input$mychooser)
  logit.model <- glm(outcome ~ ., data = my.data, family = "binomial")
  logit.model
}

function(input, output) {
  output$out2 <- renderPrint(input$in2)
  output$out3 <- renderPrint(input$in3)
  output$selection <- renderPrint(input$mychooser)
  
  logitModelReactive <- eventReactive(input$action, {
                                                      logitModel(input)
                                                      }, ignoreNULL = FALSE
                                      )
  reactiveSample <- eventReactive(input$action, {renderPrint("Hello Me!")}, ignoreNULL = FALSE)
  
  # Generate a summary of the dataset ----
  output$logit.model.summary <- renderPrint({
    summary(logitModelReactive)
  })
  
  output$fields <- renderUI({
    fluidPage(
      fluidRow(
        column(4,
          # Horizontal line ----
          tags$br(),
          h4("Step 1"),
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
               h4("Step 2"),
               h5("Choose Outcome Variable:"),
               selectInput('in2', 'Options', names(selectData(input)), selectize=FALSE),
               verbatimTextOutput('out2')
        ),
        column(4,
               # Horizontal line ----
               tags$br(),
               h4("Step 3"),
               h5("Choose Prediction Models:"),
               selectInput('in3', 'Options', c("Logistic", "Naive Bayes", "Neural Networks", "SVM"), multiple=TRUE, selectize=TRUE)#,
               #verbatimTextOutput('out3')
        )
      ),
      fluidRow(
        tags$hr(),
        column(4,
               # Horizontal line ----
               tags$br(),
               h4("Step 4"),
               h5("Choose Data Imputation Method:"),
               selectInput('in4', 'Options', c("Mice"), selectize=FALSE)
        ),
        column(4,
               # Horizontal line ----
               tags$br(),
               h4("Step 5"),
               h5("Run the Model(s):"),
               tags$br(),
               actionButton("action", label = "Run Now!", class = "btn-primary")
        )
      ),
      fluidRow(
        tags$hr()
        #,h5(reactiveSample())
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

