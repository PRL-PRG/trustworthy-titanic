titanic_train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
titanic_test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
library("ggplot2")
library("scales")
library("dplyr")
library("randomForest")
library("readr")
titanic_train <- bind_rows(titanic_train, titanic_test)
titanic_train$title <- gsub("(.*, )|(\\..*)", "", titanic_train$Name)
table(titanic_train$Sex, titanic_train$title)
title_edit <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
titanic_train$title[titanic_train$title == "Mlle"] <- "Miss"
titanic_train$title[titanic_train$title == "Ms"] <- "Miss"
titanic_train$title[titanic_train$title == "Mme"] <- "Mrs"
titanic_train$title[titanic_train$title %in% title_edit] <- "renamed Title"
table(titanic_train$Sex, titanic_train$title)
titanic_train$famsize <- titanic_train$SibSp + titanic_train$Parch + 1
ggplot(titanic_train[1:891, ], aes(x = famsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size")
titanic_train$Embarked[titanic_train$Embarked == ""] <- NA
sum(is.na(titanic_train$Age))
titanic_train1 <- titanic_train
med <- median(titanic_train$Age, na.rm = TRUE)
library(Hmisc)
titanic_train1$Age <- with(titanic_train, impute(Age, mean))
titanic_train1$Fare <- with(titanic_train, impute(Fare, median))
titanic_train1$Survived <- with(titanic_train, impute(Survived, 0))
titanic_train1$Embarked <- with(titanic_train, impute(Embarked, "C"))
titanic_train1$fsizeD[titanic_train1$famsize == 1] <- 1
titanic_train1$fsizeD[titanic_train1$famsize < 5 & titanic_train1$famsize > 1] <- 2
titanic_train1$fsizeD[titanic_train1$famsize > 4] <- 3
train <- titanic_train1[1:891, ]
test <- titanic_train1[892:1309, ]
set.seed(755)
titanic_model <- randomForest(Survived ~ Age + Fare + fsizeD + Pclass, data = train, na.action = na.exclude)
plot(titanic_model, ylim = c(0, 0.36))
ope <- predict(titanic_model, test)
importance <- importance(titanic_model)
Output <- data.frame(PassengerID = test$PassengerId, Survived = ope)
Output$Survived[Output$Survived > 0.5] = 1
Output$Survived[Output$Survived != 1] = 0
write.csv(Output, file = "pred.csv")
