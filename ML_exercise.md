# Machine Learning - Predict Exercise Method
========================================================

# Executive Summary

The goal of the project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set.  

Overview of the steps involved in the prediction process:-  
Load the training dataset  
Load the testing dataset  
Preprocess the training dataset and removed incomplete observations  
Set aside 70% of the training data for training  
Set aside 30% of the training data to validate the model  
Identify columns to be used to predict outcome  
Apply randomforest to 70% training datase  
Use the model to predict outcomes for 30% training dataset  
Evaluate model  
Use the model to predict outcomes for testing dataset  



# Background

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 



## Loading the data


The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment. 


```r
library(knitr)
library(caret)
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
library(randomForest)
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
library(R.utils)
```

```
## Loading required package: R.oo
## Loading required package: R.methodsS3
## R.methodsS3 v1.6.1 (2014-01-04) successfully loaded. See ?R.methodsS3 for help.
## R.oo v1.18.0 (2014-02-22) successfully loaded. See ?R.oo for help.
## 
## Attaching package: 'R.oo'
## 
## The following objects are masked from 'package:methods':
## 
##     getClasses, getMethods
## 
## The following objects are masked from 'package:base':
## 
##     attach, detach, gc, load, save
## 
## R.utils v1.32.4 (2014-05-14) successfully loaded. See ?R.utils for help.
## 
## Attaching package: 'R.utils'
## 
## The following object is masked from 'package:utils':
## 
##     timestamp
## 
## The following objects are masked from 'package:base':
## 
##     cat, commandArgs, getOption, inherits, isOpen, parse, warnings
```

```r

if (!file.exists("./data")) {
    dir.create("./data")
}

# Download data file
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainfilename = "./data/pml-training.csv"
if (!file.exists(trainfilename)) {
    setInternet2(TRUE)
    download.file(url, trainfilename)
}
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testfilename = "./data/pml-testing.csv"
if (!file.exists(testfilename)) {
    setInternet2(TRUE)
    download.file(url, testfilename)
}

# Read training data from file
training <- read.csv(trainfilename, header = TRUE, na.strings = c("NA", ""), 
    sep = ",")

# Read testing data from file
testing <- read.csv(testfilename, header = TRUE, na.strings = c("NA", ""), sep = ",")
```



# Preprocess the data
Remove observations with NAs from training data.  
Set aside 70% for training.  
Set aside 30% for validation.   


```r
NAs <- apply(training, 2, function(x) {
    sum(is.na(x))
})
traindata <- training[, which(NAs == 0)]
trainindex <- createDataPartition(y = traindata$classe, p = 0.7, list = FALSE)
train <- traindata[trainindex, ]
valid <- traindata[-trainindex, ]
```


# Training
Training with train() function with rf took a long time.

randomForest() seems to work fast.


```r

rf1 <- randomForest(classe ~ roll_belt + pitch_belt + yaw_belt + gyros_belt_x + 
    gyros_belt_y + gyros_belt_z + accel_belt_x + accel_belt_y + accel_belt_z + 
    magnet_belt_x + magnet_belt_y + magnet_belt_z + roll_arm + pitch_arm + yaw_arm + 
    total_accel_arm + gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + 
    accel_arm_y + accel_arm_z + magnet_arm_x + magnet_arm_y + magnet_arm_z, 
    train, ntree = 100)
print(rf1)
```

```
## 
## Call:
##  randomForest(formula = classe ~ roll_belt + pitch_belt + yaw_belt +      gyros_belt_x + gyros_belt_y + gyros_belt_z + accel_belt_x +      accel_belt_y + accel_belt_z + magnet_belt_x + magnet_belt_y +      magnet_belt_z + roll_arm + pitch_arm + yaw_arm + total_accel_arm +      gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + accel_arm_y +      accel_arm_z + magnet_arm_x + magnet_arm_y + magnet_arm_z,      data = train, ntree = 100) 
##                Type of random forest: classification
##                      Number of trees: 100
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 2.26%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3851   15   13   21    6    0.014081
## B   35 2588   26    3    6    0.026336
## C   12   74 2291   18    1    0.043823
## D    8    4   35 2196    9    0.024867
## E    6    9    6    4 2500    0.009901
```

```r

rf2 <- randomForest(classe ~ roll_belt + pitch_belt + yaw_belt + gyros_belt_x + 
    gyros_belt_y + gyros_belt_z + accel_belt_x + accel_belt_y + accel_belt_z + 
    magnet_belt_x + magnet_belt_y + magnet_belt_z + roll_arm + pitch_arm + yaw_arm + 
    total_accel_arm + gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + 
    accel_arm_y + accel_arm_z + magnet_arm_x + magnet_arm_y + magnet_arm_z, 
    train, ntree = 200)
print(rf2)
```

```
## 
## Call:
##  randomForest(formula = classe ~ roll_belt + pitch_belt + yaw_belt +      gyros_belt_x + gyros_belt_y + gyros_belt_z + accel_belt_x +      accel_belt_y + accel_belt_z + magnet_belt_x + magnet_belt_y +      magnet_belt_z + roll_arm + pitch_arm + yaw_arm + total_accel_arm +      gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + accel_arm_y +      accel_arm_z + magnet_arm_x + magnet_arm_y + magnet_arm_z,      data = train, ntree = 200) 
##                Type of random forest: classification
##                      Number of trees: 200
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 2.06%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3852   22    9   20    3     0.01382
## B   28 2606   18    3    3     0.01956
## C    6   77 2294   17    2     0.04257
## D    5    1   31 2207    8     0.01998
## E    6   10    7    7 2495     0.01188
```

```r

rf3 <- randomForest(classe ~ roll_belt + pitch_belt + yaw_belt + gyros_belt_x + 
    gyros_belt_y + gyros_belt_z + accel_belt_x + accel_belt_y + accel_belt_z + 
    magnet_belt_x + magnet_belt_y + magnet_belt_z + roll_arm + pitch_arm + yaw_arm + 
    total_accel_arm + gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + 
    accel_arm_y + accel_arm_z + magnet_arm_x + magnet_arm_y + magnet_arm_z, 
    train, ntree = 300)
print(rf3)
```

```
## 
## Call:
##  randomForest(formula = classe ~ roll_belt + pitch_belt + yaw_belt +      gyros_belt_x + gyros_belt_y + gyros_belt_z + accel_belt_x +      accel_belt_y + accel_belt_z + magnet_belt_x + magnet_belt_y +      magnet_belt_z + roll_arm + pitch_arm + yaw_arm + total_accel_arm +      gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + accel_arm_y +      accel_arm_z + magnet_arm_x + magnet_arm_y + magnet_arm_z,      data = train, ntree = 300) 
##                Type of random forest: classification
##                      Number of trees: 300
## No. of variables tried at each split: 5
## 
##         OOB estimate of  error rate: 1.93%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 3856   17   12   17    4    0.012801
## B   27 2610   17    1    3    0.018059
## C    8   72 2295   20    1    0.042154
## D    6    0   33 2209    4    0.019094
## E    4   11    3    5 2502    0.009109
```

```r

rf.all <- combine(rf1, rf2, rf3)
print(rf.all)
```

```
## 
## Call:
##  randomForest(formula = classe ~ roll_belt + pitch_belt + yaw_belt +      gyros_belt_x + gyros_belt_y + gyros_belt_z + accel_belt_x +      accel_belt_y + accel_belt_z + magnet_belt_x + magnet_belt_y +      magnet_belt_z + roll_arm + pitch_arm + yaw_arm + total_accel_arm +      gyros_arm_x + gyros_arm_y + gyros_arm_z + accel_arm_x + accel_arm_y +      accel_arm_z + magnet_arm_x + magnet_arm_y + magnet_arm_z,      data = train, ntree = 100) 
##                Type of random forest: classification
##                      Number of trees: 600
## No. of variables tried at each split: 5
```

# Cross validate on 30% of training data

```r
predictions <- predict(rf.all, valid)
table(predictions, valid$classe)
```

```
##            
## predictions    A    B    C    D    E
##           A 1649    8    1    3    0
##           B   10 1122   39    3    2
##           C   10    9  983   23    1
##           D    5    0    3  932    1
##           E    0    0    0    3 1078
```

```r
accuracy = sum(predictions == valid$classe)/length(predictions)
accuracy
```

```
## [1] 0.9794
```


# Results / Prediction
Use the combined model to predict outcomes for testing  
OOB estimate of  error rate is expected to be less than  1.75%
Apply model to predict on testing data.  

```r
predictions <- predict(rf.all, testing)
predictions
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```

