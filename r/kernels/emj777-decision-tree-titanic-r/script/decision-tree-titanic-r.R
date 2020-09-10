
#### LIBRARIES #################
library(readr)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)

#### LOAD FILES ################

rm(list = ls())
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
Survived <- train$Survived
train$Survived <- NULL
dataset <- bind_rows(train, test)

#### KNOWING DATA ##############

dim(dataset)
str(dataset)
summary(dataset)

#### DATA CLEANNING ############

# NA Fare: 1 record: Passenger 1044 
dataset$Fare[dataset$PassengerId == 1044] <- median(dataset$Fare, na.rm = TRUE)
# NA Age: 263 records
dataset$Age <- sapply(dataset$Age, FUN=function(x) {ifelse(is.na(x),median(dataset$Age, na.rm = TRUE),x)})
# Missing Values for Embarked: 2 records
table(dataset$Embarked) /sum(dataset$Embarked != "")
# Almost 70% are 'S', then 'S' applied to missing values
dataset$Embarked[c(62,830)] <- "S"
# Missing Values for Cabin: 1014 records = 77%
1 - (sum(dataset$Cabin != "")/nrow(dataset))
dataset$Cabin <- substr(dataset$Cabin,1,1)
table(dataset$Cabin)
dataset$Cabin[dataset$Cabin == ""] <- "H"

#Factor
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Cabin')
dataset[factor_vars] <- lapply(dataset[factor_vars], function(x) as.factor(x))

#### MODEL #####################
# Supervised Learning: Decision Tree
train_cleanned <- dataset[1:891,]
test_cleanned <- dataset[892:1309,]
train_cleanned$Survived <- Survived

DT <- rpart(Survived ~ Pclass + Sex + Embarked + Cabin, train_cleanned, method = "class", cp=0)
summary(DT)
printcp(DT)
rpart.plot(DT, type=1, extra = 102)

#### OUTPUT ####################

predict_dt <- predict(DT, test_cleanned, type = "class")
result <- data.frame(PassengerID = test_cleanned$PassengerId, Survived = predict_dt)
write.csv(result, file="result.csv", row.names = FALSE)