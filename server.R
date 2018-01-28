
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

##
function(input, output) {
  output$out2 <- renderPrint(input$in2)
  output$out3 <- renderPrint(input$in3)
  output$selection <- renderPrint(input$mychooser)
  
  ##-------------------------------------------------
  logitModel <- eventReactive(input$actionTrain, {
      logitFunc(input)
  })
 
  output$nPlotLogistic <- renderPlot({
      plot(logitModel())
  })
  
  output$nTextLogistic <- renderTable({
      as.data.frame(summary(logitModel())$coeff)
  })
  
  ##-------------------------------------------------  
  naiveBayesModel <- eventReactive(input$actionTrain, {
      naiveBayesFunc(input)
  })
 
  output$nPlotNaiveBayes <- renderPlot({
      plot(naiveBayesModel())
  })
  
  output$nTextNaiveBayes <- renderTable({
      as.data.frame(naiveBayesModel()$tables[1])
  })
  
  ##---------------------------------------------------
  nnetModel <- eventReactive(input$actionTrain, {
      nnetFunc(input)
  })
  
  output$nPlotNNet <- renderPlot({
      plot(nnetModel())
  })
  
  output$nTextNNet <- renderText({
      summary(nnetModel()[1])
  })
  
  ##---------------------------------------------------
  output$dataselector <- renderUI({
  fluidPage(
      fluidRow(
          column(12,      
                    # Horizontal line ----
                    tags$br(),
                    
                    radioButtons("radio", label = h5("Use example data or upload your data:"),
                               choices = list("Load Example dataset" = 1, 
                                              "Upload your dataset" = 2), 
                               selected = 1),
                    
                    # Horizontal line ----
                    tags$hr(),
                    
                    # Input: Select a file ----
                    fileInput("file1", h5("Choose CSV File"),
                            multiple = TRUE,
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                    
                    tabsetPanel(
                      tabPanel("Data Snapshot", DT::dataTableOutput("contents")),
                      tabPanel("Data Summary", verbatimTextOutput("summary"))
                    ),
                 tags$br()
                )
            )
        )
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
               h5("Train the Model(s):"),
               tags$br(),
               actionButton("actionTrain", label = "Train Now!", class = "btn-primary")
        ),
        column(4,
               # Horizontal line ----
               tags$br(),
               h4("Step 6"),
               h5("Test the Model(s):"),
               tags$br(),
               actionButton("actionTest", label = "Test Now!", class = "btn-primary")
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