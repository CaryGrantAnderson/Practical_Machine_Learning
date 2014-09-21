
library(lattice); 
library(ggplot2); 
library(caret)
library(randomForest)

setwd("C:/Grant/Google Drive/10. Training and Certifications/Courses - Coursera - Data Science Track/8 - Practical Machine Learning - WORKING/Course Project")

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

summary(data_Training$classe)

na_Clean <- apply(data_Training, 2, function(x) {sum(is.na(x))}) 
training_Data_Cleaned <- data_Training[, which(na_Clean   == 0)]

na_Clean <- apply(data_Testing, 2, function(x) {sum(is.na(x))}) 
testing_Data_Cleaned <- data_Testing[, which(na_Clean  == 0)]

inTrain <- createDataPartition(y = training_Data_Cleaned$classe, p = 0.9, list = FALSE)

training <- training_Data_Cleaned[inTrain, ]
testing  <- training_Data_Cleaned[-inTrain, ]

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

getTrainPerf(model)

print(model)

print(plot(varImp(model, scale = FALSE)))

prediction_Training <- predict(model, training)

confusionMatrix(prediction_Training, training$classe)

predictint_Testing <- predict(model, testing)

confusionMatrix(predictint_Testing, testing$classe)








