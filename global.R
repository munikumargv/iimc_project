
#Data Selector
selectData <- function (input){
    if(input$radio == 2){
        req(input$file1)
        df <- read.csv(input$file1$datapath, sep = ",")
        df
    }else{
        PimaIndiansDiabetes
    }
}

#Split data into training and test datasets
splitData <- function(inputData, outcome){
    train.idx <- createDataPartition(inputData[, outcome], p=0.70, list = FALSE)
    train <- inputData[train.idx,]
    test <- inputData[-train.idx, ]
    list(train, test)
}

#Derive Training Data
getTrainingData <- function(input){
    splitData(selectData(input), input$in2)[[1]]
}

#Derive Test Data
getTestData <- function(input){
    splitData(selectData(input), input$in2)[[2]]
}

#Build Formula
modelFormula <- function(input){
    y <- input$in2
    x <- paste0(unlist((input$mychooser)[2]), collapse = "+")
    f <- as.formula(paste(y, x, sep="~"))
}

#Logistic Regression Model
logitFunc <- function(input){
    glm(modelFormula(input), data = getTrainingData(input), family = "binomial")
}

#Naive Bayes Model
naiveBayesFunc <- function(input){
    my.data <- selectData(input)
    naiveBayes(modelFormula(input), data = getTrainingData(input))
}

#Neural Networks Model
nnetFunc <- function(input){
    my.data <- selectData(input)
    nnet(modelFormula(input), data = getTrainingData(input), size=10, decay = 0.025, maxit = 10000)
}
