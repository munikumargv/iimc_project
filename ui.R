
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(shinythemes)

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
                            tags$hr(),
                            
                            # Input: Select a file ----
                            fileInput("file1", "Choose CSV File",
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")),
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Select separator ----
                            radioButtons("sep", "Separator",
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ","),
                            # Horizontal line ----
                            tags$hr(),
                            
                            tabsetPanel(
                              tabPanel("Data Snapshot", DT::dataTableOutput("contents")),
                              tabPanel("Data Summary", "This panel is intentionally left blank")
                            )
                   ),
                   tabPanel("Logistic Regression", "This panel is intentionally left blank"),
                   tabPanel("Naive Bayes Classifier", "This panel is intentionally left blank"),
                   tabPanel("Neural Networks", "This panel is intentionally left blank"),
                   tabPanel("SVM", "This panel is intentionally left blank"),
                   tabPanel("Model Comparison", "This panel is intentionally left blank"),
                   tabPanel("Prediction", "This panel is intentionally left blank")
                 )
               )
      ),
      tabPanel("Regression", "This panel is intentionally left blank"),
      tabPanel("Clustering", "This panel is intentionally left blank"),
      tabPanel("Principal Component Analysis", "This panel is intentionally left blank")
    )
  )
)
