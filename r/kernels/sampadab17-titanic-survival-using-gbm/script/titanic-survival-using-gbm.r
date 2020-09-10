
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

#loading and exploring datasets
train <- read.csv('../input/train.csv')
test <- read.csv('../input/train.csv')

str(train)
str(test)
summary(train)
summary(test)

#check if data has any missing values
library(Amelia)
missmap(train, main='Missing Map for Training Data')
missmap(test, main='Missing Map for testing Data')

#Imputing the NA's
train[which(is.na(train$Age)),"Age"] <- mean(train$Age,na.rm = T)
test[which(is.na(test$Age)),"Age"] <- mean(test$Age,na.rm = T)
test[which(is.na(test$Fare)),"Fare"] <- 36

#data transformation
train$Sex <- as.numeric( ifelse(train$Sex=="male",2,1))
train$Fare <- as.numeric(train$Fare)
train$Ticket <- as.numeric(train$Ticket)
train$Embarked <- as.numeric(train$Embarked)
train$Survived <-as.factor(train$Survived)
train$PassengerId <- NULL
train$Name <-NULL
train$Cabin <- as.numeric(train$Cabin)
str(train)

test$Sex <- as.numeric( ifelse(test$Sex=="male",2,1))
test$Fare <- as.numeric(test$Fare)
test$Ticket <- as.numeric(test$Ticket)
test$Embarked <- as.numeric(test$Embarked)
test$Name <- NULL
PASS <- test$PassengerId
test$PassengerId <- NULL
test$Cabin <- as.numeric(test$Cabin)
str(test)

#fitting model
library(caret)
library(gbm)
fitControl <- trainControl(method = "repeatedcv",number = 10, repeats = 10)
set.seed(22)
model <- train(train[,2:10],train$Survived, method="gbm", trControl=fitControl,verbose=F)
summary(model)

pre_train <- predict(model, newdata = train, type = "raw")
#accuracy for training dataset
mean(pre_train==train$Survived)      #0.8945

#predictions for Test Dataset
pred_test <- predict(model, newdata = test, type = "raw")
table(pred_test)
result <- data.frame(PassengerId=PASS,Survived=pred_test)
head(result)
#write.csv(result,"Titanic Survival(gbm).csv",row.names = F)



