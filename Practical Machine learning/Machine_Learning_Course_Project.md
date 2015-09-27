# Practical machine learning course project
## Background of this project
The purpose of this project is to predict how well the participant has lifted the dumbell (grade A,B,C,D,E) based on measurements from accelerometers on the belt, forearm, arm etc.

The data is from this source: http://groupware.les.inf.puc-rio.br/har.

Training data: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
Test data: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


## Import all the packages and the data sets

```r
library(caret);library(randomForest)
```


```r
if(!exists("training_data")){
  training_data <- read.csv(file.path(getwd(),"pml-training.csv")
                                      ,header=TRUE,stringsAsFactors = FALSE)
                            
}

if(!exists("testing_data")){
  testing_data <- read.csv(file.path(getwd(),"pml-testing.csv")
                                     ,header=TRUE,stringsAsFactors = FALSE)
}
```


## Eliminate the NA & blank columns

From an inspection of the data, we can see that there are many predictors with NA or no values (blank). We will not attempt to impute any values and will not consider these variables when building our model.

Furthermore, the first 7 predictors (columns) of the data do not seem to have any impact on the classe outcome, for example, username, timestamp etc. We shall also not consider these data when building our model.


```r
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
```

## Check for predictors with zero variance

None of the predictors remaining have zero variance

```r
nsv <- nearZeroVar(training_dataCleaned,saveMetrics = TRUE)
print(nsv)
```

```
##                      freqRatio percentUnique zeroVar   nzv
## roll_belt             1.101904     6.7781062   FALSE FALSE
## pitch_belt            1.036082     9.3772296   FALSE FALSE
## yaw_belt              1.058480     9.9734991   FALSE FALSE
## total_accel_belt      1.063160     0.1477933   FALSE FALSE
## gyros_belt_x          1.058651     0.7134849   FALSE FALSE
## gyros_belt_y          1.144000     0.3516461   FALSE FALSE
## gyros_belt_z          1.066214     0.8612782   FALSE FALSE
## accel_belt_x          1.055412     0.8357966   FALSE FALSE
## accel_belt_y          1.113725     0.7287738   FALSE FALSE
## accel_belt_z          1.078767     1.5237998   FALSE FALSE
## magnet_belt_x         1.090141     1.6664968   FALSE FALSE
## magnet_belt_y         1.099688     1.5187035   FALSE FALSE
## magnet_belt_z         1.006369     2.3290184   FALSE FALSE
## roll_arm             52.338462    13.5256345   FALSE FALSE
## pitch_arm            87.256410    15.7323412   FALSE FALSE
## yaw_arm              33.029126    14.6570176   FALSE FALSE
## total_accel_arm       1.024526     0.3363572   FALSE FALSE
## gyros_arm_x           1.015504     3.2769341   FALSE FALSE
## gyros_arm_y           1.454369     1.9162165   FALSE FALSE
## gyros_arm_z           1.110687     1.2638875   FALSE FALSE
## accel_arm_x           1.017341     3.9598410   FALSE FALSE
## accel_arm_y           1.140187     2.7367241   FALSE FALSE
## accel_arm_z           1.128000     4.0362858   FALSE FALSE
## magnet_arm_x          1.000000     6.8239731   FALSE FALSE
## magnet_arm_y          1.056818     4.4439914   FALSE FALSE
## magnet_arm_z          1.036364     6.4468454   FALSE FALSE
## roll_dumbbell         1.022388    84.2065029   FALSE FALSE
## pitch_dumbbell        2.277372    81.7449801   FALSE FALSE
## yaw_dumbbell          1.132231    83.4828254   FALSE FALSE
## total_accel_dumbbell  1.072634     0.2191418   FALSE FALSE
## gyros_dumbbell_x      1.003268     1.2282132   FALSE FALSE
## gyros_dumbbell_y      1.264957     1.4167771   FALSE FALSE
## gyros_dumbbell_z      1.060100     1.0498420   FALSE FALSE
## accel_dumbbell_x      1.018018     2.1659362   FALSE FALSE
## accel_dumbbell_y      1.053061     2.3748853   FALSE FALSE
## accel_dumbbell_z      1.133333     2.0894914   FALSE FALSE
## magnet_dumbbell_x     1.098266     5.7486495   FALSE FALSE
## magnet_dumbbell_y     1.197740     4.3012945   FALSE FALSE
## magnet_dumbbell_z     1.020833     3.4451126   FALSE FALSE
## roll_forearm         11.589286    11.0895933   FALSE FALSE
## pitch_forearm        65.983051    14.8557741   FALSE FALSE
## yaw_forearm          15.322835    10.1467740   FALSE FALSE
## total_accel_forearm   1.128928     0.3567424   FALSE FALSE
## gyros_forearm_x       1.059273     1.5187035   FALSE FALSE
## gyros_forearm_y       1.036554     3.7763735   FALSE FALSE
## gyros_forearm_z       1.122917     1.5645704   FALSE FALSE
## accel_forearm_x       1.126437     4.0464784   FALSE FALSE
## accel_forearm_y       1.059406     5.1116094   FALSE FALSE
## accel_forearm_z       1.006250     2.9558659   FALSE FALSE
## magnet_forearm_x      1.012346     7.7667924   FALSE FALSE
## magnet_forearm_y      1.246914     9.5403119   FALSE FALSE
## magnet_forearm_z      1.000000     8.5771073   FALSE FALSE
## classe                1.469581     0.0254816   FALSE FALSE
```

## Data pre-processing (standardising)

We will standardise the training data set and apply the same standardisation to the testing data set.


```r
preObj <- preProcess(training_dataCleaned[,-53],method=c("center","scale"))
training_dataCleaned <- cbind(predict(preObj,training_dataCleaned[,-53]),classe=training_dataCleaned$classe)
testing_dataCleaned <- predict(preObj,testing_dataCleaned)
```

## Use k-fold cross validation to predict the out of sample error rate

We will segregate our training data into 10 different sets. Each set will have 10% of the data for validation and 90% of the data for training.


```r
set.seed(5)
folds <- createFolds(y=training_dataCleaned$classe,k=10,list=TRUE,returnTrain=TRUE)
str(folds)
```

```
## List of 10
##  $ Fold01: int [1:17658] 1 2 3 4 5 6 7 8 9 10 ...
##  $ Fold02: int [1:17660] 1 2 3 4 5 6 7 8 9 10 ...
##  $ Fold03: int [1:17659] 1 3 4 5 6 9 10 12 15 16 ...
##  $ Fold04: int [1:17660] 1 2 3 4 6 7 8 9 10 11 ...
##  $ Fold05: int [1:17660] 1 2 4 5 6 7 8 9 10 11 ...
##  $ Fold06: int [1:17662] 1 2 3 4 5 6 7 8 9 11 ...
##  $ Fold07: int [1:17660] 1 2 3 5 6 7 8 9 10 11 ...
##  $ Fold08: int [1:17660] 2 3 4 5 6 7 8 9 10 11 ...
##  $ Fold09: int [1:17659] 1 2 3 4 5 7 8 9 10 11 ...
##  $ Fold10: int [1:17660] 1 2 3 4 5 6 7 8 10 11 ...
```

## Prediction using Classification Tree

We will use k-fold cross validation to predict the out of sample error rate for the classification tree model. 

We have separated our training data into 10 fold ie. validation data set is 10% while the training data set is 90%. We have 10 different training-validation sets.

For each training-validation set, we will train our model on the training set and use our model to predict the classe outcome in the validation set. We will then compute the accuracy rate of our prediction.

The estimated out of sample accuracy rate is then the average of the 10 accuracy rates.


```r
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
print(pred_acc_tree)
```

```
##    Prediction Accuracy
## 1            0.4933809
## 2            0.5056065
## 3            0.4865003
## 4            0.4898063
## 5            0.4994903
## 6            0.4933673
## 7            0.4923547
## 8            0.4811417
## 9            0.4987264
## 10           0.5112130
```

### Expected out of sample error rate (Classification tree)
The expected out of sample error rate is 1-accuracy rate


```r
# Average accuracy rate of the model based on 10 training-validation sets
mean(pred_acc_tree[,1])
```

```
## [1] 0.4951588
```

```r
# Average out of sample error rate
1-mean(pred_acc_tree[,1])
```

```
## [1] 0.5048412
```


## Prediction using Random Forest
We will use k-fold cross validation to predict the out of sample error rate for the random forest model. 

We have separated our training data into 10 fold ie. validation data set is 10% while the training data set is 90%. We have 10 different training-validation sets.

For each training-validation set, we will train our model on the training set and use our model to predict the classe outcome in the validation set. We will then compute the accuracy rate of our prediction.

The estimated out of sample accuracy rate is then the average of the 10 accuracy rates.


```r
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
print(pred_acc_RF)
```

```
##    Prediction Accuracy
## 1            0.9954175
## 2            0.9954128
## 3            0.9984717
## 4            0.9979613
## 5            0.9949032
## 6            0.9943878
## 7            0.9959225
## 8            0.9964322
## 9            0.9964340
## 10           0.9979613
```

### Expected out of sample error rate (Random Forest Model)
The expected out of sample error rate is 1-accuracy rate


```r
# Average accuracy rate of the RF model based on 10 training-validation sets
mean(pred_acc_RF[,1])
```

```
## [1] 0.9963304
```

```r
# Average out of sample error rate
1-mean(pred_acc_RF[,1])
```

```
## [1] 0.00366957
```


## Choice and application of the model to our out-of-sample test data
Since the accuracy rate of the random forest model is much better than the classification tree, we will choose the random forest model to predict the outcomes in the test data set.

We will train the model using the entire training data set and use the model for prediction.



```r
final_model <- randomForest(classe~.,data=training_dataCleaned)
final_prediction <- predict(final_model,newdata=testing_dataCleaned)

# Our final prediction of the classe variable is:
print(final_prediction)
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```




