---
title: "Practical Machine learning assessment"
output: 
  html_document:
    keep_md: true
---
#INTRODUCTION
This project is about predicting the manner in which the subjects performed weight lifting exercise. The data is collected from devices such as Jawbone Up, Nike FuelBand, and Fitbit.

##Data Preprocessing
Set the working driectory and load the required libraries and set seed for results to be Reproducible.

```r
setwd("C:\\project_8")
library(caret)
library(randomForest)
set.seed(50)
```
Load the given csv file to the variables

```r
pml_train <- read.csv("pml-training.csv",header=T,sep=",",na.strings=c("NA","NaN", " ", "", "#DIV/0!"))
pml_test <- read.csv("pml-testing.csv",header=T,sep=",",na.strings=c("NA","NaN", " ", "", "#DIV/0!"))
```
Removing the first column that represents the ID

```r
pml_train <- pml_train[,-1]
pml_test <- pml_test[,-1]
```
##Cleansing the data for the model ( We are using Random Forest method here)
Remove columns that do not satisfy is applied before applying to the model

```r
remove_train <- c((colSums(!is.na(pml_train[,-ncol(pml_train)])) >= 0.6*nrow(pml_train)))
pml_train   <-  pml_train[,remove_train]
```
Create data partitions for training and validation

```r
trainPart <- createDataPartition(pml_train$classe, p=0.6, list=FALSE)
pml_training <- pml_train[trainPart,]
pml_validation <- pml_train[-trainPart,]
```
Remove the near zero variance columns

```r
Train_NonZeroVar <- nearZeroVar(pml_training)
Validate_NonZeroVar <- nearZeroVar(pml_validation)
pml_train_without_nzv <- pml_training[, -Train_NonZeroVar]
pml_validate_without_nzv <- pml_validation[,-Validate_NonZeroVar]
```
##Building the model

```r
PredictModel <- randomForest(classe~.,data=pml_train_without_nzv,importance=TRUE)
PredictModel
```

```
## 
## Call:
##  randomForest(formula = classe ~ ., data = pml_train_without_nzv,      importance = TRUE) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 7
## 
##         OOB estimate of  error rate: 0.19%
## Confusion matrix:
##      A    B    C    D    E  class.error
## A 3347    1    0    0    0 0.0002986858
## B    1 2278    0    0    0 0.0004387889
## C    0    5 2045    4    0 0.0043816943
## D    0    0    3 1926    1 0.0020725389
## E    0    0    0    7 2158 0.0032332564
```
Now evaluate the results through the confusion matrix

```r
confusionMatrix(predict(PredictModel,newdata=pml_validate_without_nzv[,-ncol(pml_validate_without_nzv)]),
                pml_validate_without_nzv$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 2231    3    0    0    0
##          B    1 1515    2    0    0
##          C    0    0 1365    1    0
##          D    0    0    1 1285    1
##          E    0    0    0    0 1441
## 
## Overall Statistics
##                                           
##                Accuracy : 0.9989          
##                  95% CI : (0.9978, 0.9995)
##     No Information Rate : 0.2845          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9985          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9996   0.9980   0.9978   0.9992   0.9993
## Specificity            0.9995   0.9995   0.9998   0.9997   1.0000
## Pos Pred Value         0.9987   0.9980   0.9993   0.9984   1.0000
## Neg Pred Value         0.9998   0.9995   0.9995   0.9998   0.9998
## Prevalence             0.2845   0.1935   0.1744   0.1639   0.1838
## Detection Rate         0.2843   0.1931   0.1740   0.1638   0.1837
## Detection Prevalence   0.2847   0.1935   0.1741   0.1640   0.1837
## Balanced Accuracy      0.9995   0.9988   0.9988   0.9995   0.9997
```
Check the accuracy of the model

```r
accuracy<-c(as.numeric(predict(PredictModel,newdata=pml_validate_without_nzv
                            [,-ncol(pml_validate_without_nzv)])==pml_validate_without_nzv$classe))
accuracy<-sum(accuracy)*100/nrow(pml_validate_without_nzv)
accuracy
```

```
## [1] 99.88529
```
We can see our model is having an accuracy of 99.89 % and the out-of-sample error is only 0.11%

##Testing the model
Clean the test data as same as like we did for training set

```r
remove_test <- c((colSums(!is.na(pml_test[,-ncol(pml_test)])) >= 0.6*nrow(pml_test)))
pml_test   <-  pml_test[,remove_test]
pml_test <- pml_test[,-ncol(pml_test)]
```
Making the testing dataset to same class and structure of training dataset and apply dummy row id for dummy in the test data

```r
pml_test <- rbind(pml_train[100, -59] , pml_test)
row.names(pml_test) <- c(100,1:20)
```
Testing the model with the test data

```r
Prediction<- predict(PredictModel,newdata=pml_test[-1,])
Prediction
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```
