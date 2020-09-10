
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest

library(data.table)
library(FeatureHashing)
library(Matrix)
library(xgboost)
require(randomForest)
require(caret)
require(dtplyr)
library(pROC)
library(stringr)
library(dummies)
library(Metrics)
library(kernlab)
library(mlbench)


# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
names(train)
summary(train)

# Here we will plot the passenger survival by class
train$Survived <- factor(train$Survived, levels=c(1,0))
levels(train$Survived) <- c("Survived", "Died")
train$Pclass <- as.factor(train$Pclass)
levels(train$Pclass) <- c("1", "2", "3")


png("1_survival_by_class.png", width=800, height=600)
mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
           
           
dev.off()

train$xSex[train$Sex == "male"] <- 1
train$xSex[train$Sex == "female"] <- 0 

plot(train$Survived, train$Sex)
plot(train$Fare, train$Pclass)
plot (train$Survived, train$Cabin)
plot (train$Survived, train$Embarked)

plot (train$xSex, train$Survived) 


