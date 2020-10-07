library(readr)
library(dplyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
rm(list = ls())
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
Survived <- train$Survived
train$Survived <- NULL
dataset <- bind_rows(train, test)
dim(dataset)
str(dataset)
summary(dataset)
dataset$Fare[dataset$PassengerId == 1044] <- median(dataset$Fare, na.rm = TRUE)
dataset$Age <- sapply(dataset$Age, FUN = function(x) {
    ifelse(is.na(x), median(dataset$Age, na.rm = TRUE), x)
})
table(dataset$Embarked)/sum(dataset$Embarked != "")
dataset$Embarked[c(62, 830)] <- "S"
1 - (sum(dataset$Cabin != "")/nrow(dataset))
dataset$Cabin <- substr(dataset$Cabin, 1, 1)
table(dataset$Cabin)
dataset$Cabin[dataset$Cabin == ""] <- "H"
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Cabin")
dataset[factor_vars] <- lapply(dataset[factor_vars], function(x) as.factor(x))
train_cleanned <- dataset[1:891, ]
test_cleanned <- dataset[892:1309, ]
train_cleanned$Survived <- Survived
DT <- rpart(Survived ~ Pclass + Sex + Embarked + Cabin, train_cleanned, method = "class", cp = 0)
summary(DT)
printcp(DT)
rpart.plot(DT, type = 1, extra = 102)
predict_dt <- predict(DT, test_cleanned, type = "class")
result <- data.frame(PassengerID = test_cleanned$PassengerId, Survived = predict_dt)
write.csv(result, file = "result.csv", row.names = FALSE)
