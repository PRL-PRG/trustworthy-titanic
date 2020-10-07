library(tidyverse)
library(rpart)
library(randomForest)
library(dplyr)
library(ggplot2)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read_csv("../input/test.csv")
summary(train)
summary(test)
avg <- median(train$Age, na.rm = TRUE)
train$Age <- replace(train$Age, is.na(train$Age), avg)
avg <- median(test$Age, na.rm = TRUE)
test$Age <- replace(test$Age, is.na(test$Age), avg)
avg <- median(test$Fare, na.rm = TRUE)
test$Fare <- replace(test$Fare, is.na(test$Fare), avg)
library(gridExtra)
library(grid)
subset(train, !(Embarked %in% c("C", "Q", "S")))
train$Embarked[!(train$Embarked %in% c("C", "Q", "S"))] <- "C"
a <- ggplot(train, aes(x = factor(Sex), fill = factor(Survived))) + geom_bar(position = "dodge")
b <- ggplot(train, aes(x = factor(Pclass), fill = factor(Survived))) + geom_bar(position = "dodge")
c <- ggplot(train, aes(x = factor(Parch), fill = factor(Survived))) + geom_bar(position = "dodge")
d <- ggplot(train, aes(x = factor(SibSp), fill = factor(Survived))) + geom_bar(position = "dodge")
e <- ggplot(train, aes(x = factor(Embarked), fill = factor(Survived))) + geom_bar(position = "dodge")
grid.arrange(a, b, c, d, e, ncol = 2)
ggplot(train, aes(x = factor(Embarked), fill = factor(Survived))) + geom_bar(position = "dodge") + facet_grid(Pclass ~ .)
train$AgeGrp[train$Age <= 12] <- "Kids"
train$AgeGrp[train$Age > 12 & train$Age <= 55] <- "Adults"
train$AgeGrp[train$Age > 55] <- "Seniors"
train$AgeGrp <- factor(train$AgeGrp)
prop.table(table(train$AgeGrp)) * 100
train %>% filter(AgeGrp != "Adults") %>% ggplot(aes(x = factor(Sex), fill = factor(Survived))) + geom_bar(position = "dodge") + facet_grid(AgeGrp ~ Pclass)
train$status <- "Family"
train$status[train$Parch == 0 & train$SibSp == 0] <- "Single"
train$status <- factor(train$status)
a <- prop.table(table(train$status)) * 100
b <- table(train$status)
rbind(Pctg = a, Count = b)
train %>% ggplot(aes(x = factor(status), fill = factor(Survived))) + geom_bar(position = "dodge") + facet_grid(. ~ Pclass)
library(caret)
train$Survived <- factor(train$Survived)
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"
set.seed(7)
fit.rf <- train(Survived ~ Sex + Pclass + AgeGrp + status + Fare, data = train, method = "rf", metric = metric, trControl = control)
train$Survived2 <- predict(fit.rf, train)
confusionMatrix(train$Survived2, train$Survived)
test$AgeGrp[test$Age <= 12] <- "Kids"
test$AgeGrp[test$Age > 12 & test$Age <= 55] <- "Adults"
test$AgeGrp[test$Age > 55] <- "Seniors"
test$AgeGrp <- factor(test$AgeGrp)
test$status <- "Family"
test$status[test$Parch == 0 & test$SibSp == 0] <- "Single"
test$status <- factor(test$status)
test$status <- factor(test$status)
summary(test)
test$Survived <- predict(fit.rf, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "titanic3.csv", row.names = FALSE)
