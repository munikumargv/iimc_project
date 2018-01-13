
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
shinyUI(
  tagList(
    shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "ML As a Service",
      tabPanel("Classification",
               sidebarPanel(
                 fileInput("file", "File input:"),
                 textInput("txt", "Text input:", "general"),
                 sliderInput("slider", "Slider input:", 1, 100, 30),
                 tags$h5("Deafult actionButton:"),
                 actionButton("action", "Search"),
                 
                 tags$h5("actionButton with CSS class:"),
                 actionButton("action2", "Action button", class = "btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("CART",
                            h4("Table"),
                            tableOutput("table"),
                            h4("Verbatim text output"),
                            verbatimTextOutput("txtout"),
                            h1("Header 1"),
                            h2("Header 2"),
                            h3("Header 3"),
                            h4("Header 4"),
                            h5("Header 5")
                   ),
                   tabPanel("Logistic", "This panel is intentionally left blank"),
                   tabPanel("Naive Bayes", "This panel is intentionally left blank"),
                   tabPanel("Neural Networks", "This panel is intentionally left blank"),
                   tabPanel("Support Vector Machines", "This panel is intentionally left blank")
                 )
               )
      ),
      tabPanel("Regression", "This panel is intentionally left blank"),
      tabPanel("Clustering", "This panel is intentionally left blank")
    )
  )
)
