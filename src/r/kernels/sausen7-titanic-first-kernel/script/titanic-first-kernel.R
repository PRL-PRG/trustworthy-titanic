knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(caret)
gender <- read_csv("../input/gender_submission.csv")
training <- read_csv("../input/train.csv")
training$Survived <- factor(training$Survived)
training$Pclass <- factor(training$Pclass)
training$Embarked <- factor(training$Embarked)
training$Sex <- factor(training$Sex)
training$PassengerId <- NULL
head(training)
logit <- glm(Survived ~ ., data = training, family = "binomial")
colSums(is.na(training)) * 100/nrow(training)
set.seed(42)
inTrain <- createDataPartition(y = training$Survived, p = 0.8, list = FALSE)
modeltrainset <- training[inTrain, ]
modeltestset <- training[-inTrain, ]
logit2 <- glm(Survived ~ Pclass + Sex + Age + Embarked, data = modeltrainset, family = "binomial")
summary(logit2)
pred <- predict(logit2, modeltestset, type = "response")
pred[pred > 0.5] <- 1
pred[pred < 0.5 | is.na(pred)] <- 0
pred <- as.factor(pred)
confusionMatrix(pred, modeltestset$Survived)$overall[1]
logit3 <- glm(Survived ~ Pclass + Sex + Age + Embarked + Fare, data = modeltrainset, family = "binomial")
summary(logit3)
logit4 <- glm(Survived ~ Pclass + Sex + Age + Embarked + Fare + SibSp, data = modeltrainset, family = "binomial")
summary(logit4)
pred2 <- predict(logit4, modeltestset, type = "response")
pred2[pred2 > 0.5] <- 1
pred2[pred2 < 0.5 | is.na(pred2)] <- 0
pred2 <- as.factor(pred2)
confusionMatrix(pred2, modeltestset$Survived)$overall[1]
testing <- read_csv("../input/test.csv")
testing$Pclass <- factor(testing$Pclass)
testing$Embarked <- factor(testing$Embarked)
testing$Sex <- factor(testing$Sex)
pred_final <- predict(logit4, testing, type = "response")
pred_final[pred_final > 0.5] <- 1
pred_final[pred_final < 0.5 | is.na(pred_final)] <- 0
submission_df <- data.frame(PassengerId = testing$PassengerId, Survived = as.numeric(pred_final))
write.csv(submission_df, file = "submission.csv", row.names = FALSE)
