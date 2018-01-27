
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(shinythemes)
source("chooser.R")

radioSelection <- textOutput("radioSelection")

shinyUI(
  tagList(
    shinythemes::themeSelector(),
    navbarPage(
      theme = "yeti",  # <--- To use a theme, uncomment this
      "Prediction Machine",
      tabPanel("Classification",
               mainPanel(
                 tabsetPanel(
                   tabPanel("Data Selector",
                            # Horizontal line ----
                            tags$br(),
                            
                            radioButtons("radio", label = h5("Use example data or upload your data:"),
                                         choices = list("Load Example dataset" = 1, 
                                                        "Upload your dataset" = 2), 
                                         selected = 1),
                            
                            
                            textOutput("radioSelection"),
                            #uiOutput("ui"),
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Select a file ----
                            fileInput("file1", h5("Choose CSV File"),
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Select separator ----
                            radioButtons("sep", h5("Separator"),
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ","),
                            # Horizontal line ----
                            tags$hr(),
                            
                            tabsetPanel(
                              tabPanel("Data Snapshot", DT::dataTableOutput("contents")),
                              tabPanel("Data Summary", verbatimTextOutput("summary"))
                            )
                   ),
                   tabPanel("Model Configuration", uiOutput("fields")),
                   tabPanel("Model Prediction", "This panel is intentionally left blank"),
                   tabPanel("Logistic Regression", uiOutput("fields.lr")),
                   tabPanel("Naive Bayes", uiOutput("fields.nb")),
                   tabPanel("Neural Networks", uiOutput("fields.nnet")),
                   tabPanel("SVM", "This panel is intentionally left blank")
                 )
               )
      ),
      tabPanel("Regression", "This panel is intentionally left blank"),
      tabPanel("Clustering", "This panel is intentionally left blank"),
      tabPanel("Principal Component Analysis", "This panel is intentionally left blank")
    )
  )
)
