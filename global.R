
##--------------------------------Data Processing Starts------------------------
#Step 1: Data Selector
selectData <- function (input){
    if(input$radio == 2){
        req(input$file1)
        df <- read.csv(input$file1$datapath, sep = ",", stringsAsFactors = FALSE)
        df
    }else{
        PimaIndiansDiabetes
    }
}


#Step 2: Data Pre-Processing [Convert into factors]
convertToFactors <- function(inputData, input){
  ### Convert outcome variable to a factor
  factor_variables <- input$in2
  inputData[factor_variables] <- lapply(inputData[factor_variables], function(x) as.factor(x))
  
  #Convert all data frame character columns to factors. [Is this right thing to do?]
  inputData[sapply(inputData, is.character)] <- lapply(inputData[sapply(inputData, is.character)], as.factor)  
  inputData
}

##--------------------------------------------------------------------
#Step 3: Data Pre-Processing [Impute missing data]
imputeMissingData <- function(inputData, input){
  dataImputationOption <- input$in4
  if(dataImputationOption == "Mice"){
    imputeMissingDataMice(inputData, input)
  }
  else if(dataImputationOption == "Option2"){
    imputeMissingDataOption2(inputData, input)
  }
}

#Step 3.1: Data Pre-Processing [Impute missing data: Mice]
imputeMissingDataMice <- function(inputData, input){
  print("Inside imputeMissingDataMice()")
  predictors <- unlist((input$mychooser)[2])
  my.data <- inputData[, predictors]
  nums <- sapply(my.data, is.numeric)
  #my.data stroes all numeric columns from predictors set
  my.data <- my.data[, nums]
  #Find missing data pattern
  missing.data.count <- sum(is.na(my.data))
  #Numeric Predictors
  numeric.predictors <- colnames(my.data)
  
  if(missing.data.count == 0){
    inputData
  }
  else{
    # Perform predictive imputation on all numeric columns i.e. on all data in my.data
    mice_mod <- mice(my.data, method='pmm', printFlag = FALSE)
    # Save the complete output 
    mice_output <- complete(mice_mod)
    
    #Copy over data from data frame "mice_output" to data frame "inputData"
    print(paste0("Missing Count - Before", sum(is.na(inputData[, numeric.predictors]))))
    for (i in 1:length(numeric.predictors)){
      inputData[, numeric.predictors[i]] <- mice_output[, numeric.predictors[i]]
    }
    print(paste0("Missing Count - After", sum(is.na(inputData[, numeric.predictors]))))
    
    inputData
  }
}

#Step 3.2: Data Pre-Processing [Impute missing data: Option 2]
imputeMissingDataOption2 <- function(inputData, input){
  print("Inside imputeMissingDataOption2()")
  inputData
  ##To be done
}
##--------------------------------------------------------------------

#Step 4: Data Pre-Processing [Top Level Function]
preProcessData <- function(inputData, input){
  processedData.1 <- convertToFactors(inputData, input)
  processedData.2 <- imputeMissingData(processedData.1, input)
  processedData.2
}

#Step 5: Split data into training and test datasets
splitData <- function(inputData, input){
    sample <- sample.split(inputData[, input$in2], SplitRatio = 0.75)
    train = subset(inputData, sample == TRUE)
    test = subset(inputData, sample == FALSE)
    list(train, test)
}

#Step 6: Derive Training Data
getTrainingData <- function(input){
  preProcessedData <- preProcessData(selectData(input), input)
  splitData(preProcessedData, input)[[1]]
}

#Step 7: Derive Test Data
getTestData <- function(input){
  preProcessedData <- preProcessData(selectData(input), input)
  splitData(preProcessedData, input)[[2]]
}

##--------------------------------Data Processing Ends------------------------

##--------------------------------Model Building Starts-----------------------
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
##--------------------------------Model Building Ends-------------------------

##--------------------------------Model Prediction Starts---------------------
deriveInputDataVars <- function(input){
  predictors <- unlist((input$mychooser)[2])
  inputDataVars <- getTestData(input)[, predictors]
}

##--------------------------------Model Prediction Ends-----------------------