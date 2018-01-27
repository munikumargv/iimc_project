
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(mlbench)
library(e1071)
library(caret)
library(nnet)
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

#Logistic Regression Model
logitFunc <- function(input){
  my.data <- selectData(input)
  y <- input$in2
  x <- paste0(unlist((input$mychooser)[2]), collapse = "+")
  f <- as.formula(paste(y, x, sep="~"))
  glm(f, data = my.data, family = "binomial")
}

#Naive Bayes Model
naiveBayesFunc <- function(input){
    my.data <- selectData(input)
    y <- input$in2
    x <- paste0(unlist((input$mychooser)[2]), collapse = "+")
    f <- as.formula(paste(y, x, sep="~"))
    naiveBayes(f, data = my.data)
}

#Neural Networks Model
nnetFunc <- function(input){
    my.data <- selectData(input)
    y <- input$in2
    x <- paste0(unlist((input$mychooser)[2]), collapse = "+")
    f <- as.formula(paste(y, x, sep="~"))
    nnet(f, data = my.data, size=10, decay = 0.025, maxit = 10000)
}

function(input, output) {
  output$out2 <- renderPrint(input$in2)
  output$out3 <- renderPrint(input$in3)
  output$selection <- renderPrint(input$mychooser)
  
  ##-------------------------------------------------
  logitModel <- eventReactive(input$action, {
      logitFunc(input)
  })
 
  output$nPlotLogistic <- renderPlot({
      plot(logitModel())
  })
  
  output$nTextLogistic <- renderTable({
      as.data.frame(summary(logitModel())$coeff)
  })
  
  ##-------------------------------------------------  
  naiveBayesModel <- eventReactive(input$action, {
      naiveBayesFunc(input)
  })
 
  output$nPlotNaiveBayes <- renderPlot({
      plot(naiveBayesModel())
  })
  
  output$nTextNaiveBayes <- renderTable({
      as.data.frame(naiveBayesModel()$tables[1])
  })
  
  ##---------------------------------------------------
  nnetModel <- eventReactive(input$action, {
      nnetFunc(input)
  })
  
  output$nPlotNNet <- renderPlot({
      plot(nnetModel())
  })
  
  output$nTextNNet <- renderText({
      summary(nnetModel()[1])
  })
  
  ##---------------------------------------------------
  
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
               h5("Train the Model(s):"),
               tags$br(),
               actionButton("action", label = "Run Now!", class = "btn-primary")
        )
      ),
      fluidRow(
        tags$hr(),
        h4("Model Summary"),
        plotOutput("nPlotLogistic")
      )   
    )
  })
  
  #Logistic Regression Tab
  output$fields.lr <- renderUI({
    fluidPage(
      fluidRow(
        h4("Model Summary"),
        tableOutput("nTextLogistic")
      )
    )
  })
  
  #Naive Bayes Tab
  output$fields.nb <- renderUI({
      fluidPage(
          fluidRow(
              h4("Model Summary"),
              tableOutput("nTextNaiveBayes")
          )
      )
  })  

  #Neural Networks Tab
  output$fields.nnet <- renderUI({
      fluidPage(
          fluidRow(
              h4("Model Summary"),
              verbatimTextOutput("nTextNNet")
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