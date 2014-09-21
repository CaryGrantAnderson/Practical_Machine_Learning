---
title: "Coursera Practical Machine Learning Course Project"
author: "Cary Grant Anderson"
date: "Sunday, September 21, 2014"
output: html_document
---

# 1. Summary:

In this machine learning prediction model we take a data set from a fitness experiment and predict the manner in which the experimental subjects did the exercise in the experiment.  This is the class project for the Coursera/John Hopkins University course "Practical Machine Learning".


# 2. Background:

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly 
do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 


# 3. Data:

The training data for this project are available here: 

    https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 

    https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv


The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. 


# 4. Coursera Practical Machine learning Assignment:

The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 

1. Your submission should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).

2. You should also apply your machine learning algorithm to the 20 test cases available in the test data above. Please submit your predictions in appropriate format to the programming assignment for automated grading. See the programming assignment for additional details. 


# 5. Setup.

```{r echo=FALSE}
library(lattice); 
library(ggplot2); 
library(caret)
library(randomForest)

setwd("C:/Grant/Google Drive/10. Training and Certifications/Courses - Coursera - Data Science Track/8 - Practical Machine Learning - WORKING/Course Project")
```


# 6. Load the Data.

```{r}
if (!file.exists("./data/pml-training.csv"))
{
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "./data/pml-
training.csv")
}

if (!file.exists("./data/pml-testing.csv"))
{
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "./data/pml-
testing.csv")
}

data_Testing = read.csv("./data/pml-testing.csv", na.strings = c("NA", ""))
dim(data_Testing)

data_Training = read.csv("./data/pml-training.csv", na.strings = c("NA", ""))
dim(data_Training)
```

The training data consists of 19,622 rows and 160 columns.


# 7. Explore the Data.
  
Explore the data.  The main column that we are interested in is called "classe".

```{r}
summary(data_Training$classe)
```

Exploring the data in R Studio shows that many columns (such as avg_roll_belt, max_roll_arm, etc.) are 
empty or "NA".


# 8. Clean up the Data.
  
Clean up the data first.

```{r}
na_Clean <- apply(data_Training, 2, function(x) {sum(is.na(x))}) 
training_Data_Cleaned <- data_Training[, which(na_Clean   == 0)]

na_Clean <- apply(data_Testing, 2, function(x) {sum(is.na(x))}) 
testing_Data_Cleaned <- data_Testing[, which(na_Clean  == 0)]
```

This reduces the data set to just 60 columns.


# 9. Create the partitions.
  
```{r}
inTrain <- createDataPartition(y = training_Data_Cleaned$classe, p = 0.9, list = FALSE)

training <- training_Data_Cleaned[inTrain, ]
testing  <- training_Data_Cleaned[-inTrain, ]
```


# 10. Run the Training with stochastic gradient boosting and only columns with data.

```{r}
model <- train(classe ~ user_name 
                + roll_belt
                + pitch_belt 
                + yaw_belt 
                + gyros_belt_x 
                + gyros_belt_y 
                + gyros_belt_z 
                + accel_belt_x 
                + accel_belt_y 
                + accel_belt_z 
                + magnet_belt_x 
                + magnet_belt_y 
                + magnet_belt_z 
                + roll_arm 
                + pitch_arm 
                + yaw_arm 
                + gyros_arm_x 
                + gyros_arm_y 
                + gyros_arm_z 
                + accel_arm_x 
                + accel_arm_y 
                + accel_arm_z 
                + magnet_arm_x 
                + magnet_arm_y 
                + magnet_arm_z 
                + roll_dumbbell 
                + pitch_dumbbell 
                + yaw_dumbbell, method="gbm", data=training, verbose=FALSE)
```

Results: We have a 91.6% fit with 150 trees which is quite good.

```{r}
getTrainPerf(model)

print(model)
```


# 11. Check the Effects of the Variables.
  
# We do a quick check of the effects of the variables.  We can easily and quickly see which of the experiment variables are important and those which are not.

```{r}
print(plot(varImp(model, scale = FALSE)))
```


# 12. Run the predictions.
  
```{r}
prediction_Training <- predict(model, training)

confusionMatrix(prediction_Training, training$classe)
```

We have a 93.4% fit which is quite good.


# 13. Check performance results.

```{r}
predictint_Testing <- predict(model, testing)

confusionMatrix(predictint_Testing, testing$classe)
```

We have a 91.99% fit for our testing data which is very close to our training data.


# 14. Conclusion.

Thus, we can conclude that our predictions have a very high level of accuracy.












