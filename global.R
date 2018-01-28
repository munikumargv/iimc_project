
#Data Selector
selectData <- function (input){
    if(input$radio == 2){
        req(input$file1)
        df <- read.csv(input$file1$datapath, sep = input$sep)
        df
    }else{
        PimaIndiansDiabetes
    }
}

#Build Formula
modelFormula <- function(input){
    y <- input$in2
    x <- paste0(unlist((input$mychooser)[2]), collapse = "+")
    f <- as.formula(paste(y, x, sep="~"))
}

#Logistic Regression Model
logitFunc <- function(input){
    my.data <- selectData(input)
    glm(modelFormula(input), data = my.data, family = "binomial")
}

#Naive Bayes Model
naiveBayesFunc <- function(input){
    my.data <- selectData(input)
    naiveBayes(modelFormula(input), data = my.data)
}

#Neural Networks Model
nnetFunc <- function(input){
    my.data <- selectData(input)
    nnet(modelFormula(input), data = my.data, size=10, decay = 0.025, maxit = 10000)
}