
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
splitData <- function(inputData, input){
    #train.idx <- createDataPartition(inputData[, outcome], p=0.70, list = FALSE)
    #train <- inputData[train.idx,]
    #test <- inputData[-train.idx, ]
    
    ### Make variables factors into factors
    factor_variables <- input$in2
    inputData[factor_variables] <- lapply(inputData[factor_variables], function(x) as.factor(x))
  
    sample <- sample.split(inputData[, input$in2], SplitRatio = .75)
    train = subset(inputData, sample == TRUE)
    test = subset(inputData, sample == FALSE)
    list(train, test)
}

#Derive Training Data
getTrainingData <- function(input){
    splitData(selectData(input), input)[[1]]
}

#Derive Test Data
getTestData <- function(input){
    splitData(selectData(input), input)[[2]]
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
    naiveBayes(modelFormula(input), data = getTrainingData(input))
}

#Neural Networks Model
nnetFunc <- function(input){
    nnet(modelFormula(input), data = getTrainingData(input), size=10, decay = 0.025, maxit = 10000)
}

#Support Vector Machines Model
svmFunc <- function(input){
  svm(modelFormula(input), data = getTrainingData(input))
}

#Confusion Matrix for Naive Bayes
predict.naivebayes.fulldata <- function(input, outcome){
    test.data <- getTestData(input)
    naivebayes.predicted.fulldata <- predict(naiveBayesFunc(input), test.data)
    table(naivebayes.predicted.fulldata, test.data[,outcome], dnn = c("Predicted", "Actual"))
}

#Confusion Matrix for Logistic
predict.logit.fulldata <- function(input, outcome){
  test.data <- getTestData(input)
  logit.predicted.fulldata <- predict(logitFunc(input), test.data)
  logit.predicted.fulldata <- ifelse(logit.predicted.fulldata < 0.7, 0, 1)
  table(logit.predicted.fulldata, test.data[,outcome], dnn = c("Predicted", "Actual"))
}

#Confusion Matrix for Neural Networks
predict.nnet.fulldata <- function(input, outcome){
  test.data <- getTestData(input)
  nnet.predicted.fulldata <- predict(nnetFunc(input), test.data)
  nnet.predicted.fulldata <- ifelse(nnet.predicted.fulldata < 0.7, 0, 1)
  table(nnet.predicted.fulldata, test.data[,outcome], dnn = c("Predicted", "Actual"))
}

#Confusion Matrix for SVM
predict.svm.fulldata <- function(input, outcome){
  test.data <- getTestData(input)
  svm.predicted.fulldata <- predict(svmFunc(input), test.data)
  #svm.predicted.fulldata <- ifelse(svm.predicted.fulldata < 0.7, 0, 1)
  table(svm.predicted.fulldata, test.data[,outcome], dnn = c("Predicted", "Actual"))
}
