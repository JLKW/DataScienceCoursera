library(caret);library(randomForest)

if(!exists("training_data")){
  training_data <- read.csv("./Practical Machine Learning/pml-training.csv", header=TRUE,stringsAsFactors = FALSE)
}
if(!exists("testing_data")){
  testing_data <- read.csv("./Practical Machine Learning/pml-testing.csv", header=TRUE,stringsAsFactors = FALSE)
}


# Eliminate the NA & blank columns

countBlanksAndNA <- function(cnames,training_data){
      na_count <- numeric()
      blank_count <- numeric()
      for(i in 1:length(cnames)){
        na_count<- c(na_count,sum(is.na(training_data[[cnames[i]]])))
        blank_count <- c(blank_count,sum(training_data[[cnames[i]]]==""))
      }
      return(data.frame(column_names=cnames,countOfNa=na_count,blankCount=blank_count))
}

blankOrNACount <- countBlanksAndNA(colnames(training_data),training_data)
columnsWoBlanksOrNA <- as.character(blankOrNACount[blankOrNACount$countOfNa==0&blankOrNACount$blankCount==0,1])

#Remove the first 7 columns which have no relation to the outcome eg. username, timestamp etc
columnsWoBlanksOrNA <- columnsWoBlanksOrNA[-c(1:7)]
training_dataCleaned <- training_data[,columnsWoBlanksOrNA]
training_dataCleaned$classe <- as.factor(training_dataCleaned$classe)
testing_dataCleaned <- testing_data[,columnsWoBlanksOrNA[-53]]

# Check for covariates with zero variance
nsv <- nearZeroVar(training_dataCleaned,saveMetrics = TRUE)

# Data pre-processing (standardising)
preObj <- preProcess(training_dataCleaned[,-53],method=c("center","scale"))
training_dataCleaned <- cbind(predict(preObj,training_dataCleaned[,-53]),classe=training_dataCleaned$classe)
testing_dataCleaned <- predict(preObj,testing_dataCleaned)


#Use k-fold cross validation to predict the out of sample error rate
set.seed(5)
folds <- createFolds(y=training_dataCleaned$classe,k=10,list=TRUE,returnTrain=TRUE)

#Using Classification Tree
pred_acc_tree <- data.frame()
for(f in folds){

    trainingSet <- training_dataCleaned[f,]
    testingSet <- training_dataCleaned[-f,]
    
    model_tree <- train(classe~.,method="rpart",data=trainingSet)
    prediction_tree <- predict(model_tree,newdata=testingSet)
    
    cm <- confusionMatrix(prediction_tree,testingSet$classe)
    pred_acc_tree <- rbind(pred_acc_tree,cm$overall["Accuracy"])
    
}

colnames(pred_acc_tree) <- "Prediction Accuracy"

# Using Random Forest
pred_acc_RF <- data.frame()
for(f in folds){
  
  trainingSet <- training_dataCleaned[f,]
  testingSet <- training_dataCleaned[-f,]
  
  model_RF <- randomForest(classe~.,data=trainingSet)
  prediction_RF <- predict(model_RF,newdata=testingSet)
  
  cm <- confusionMatrix(prediction_RF,testingSet$classe)
  pred_acc_RF <- rbind(pred_acc_RF,cm$overall["Accuracy"])
  
}
colnames(pred_acc_RF)<- "Prediction Accuracy"


# Application of the model to our out-of-sample test data
final_model <- randomForest(classe~.,data=training_dataCleaned)
final_prediction <- predict(final_model,newdata=testing_dataCleaned)






