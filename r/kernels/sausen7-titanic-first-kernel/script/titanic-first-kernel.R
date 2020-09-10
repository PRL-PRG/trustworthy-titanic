## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(caret)


gender <- read_csv('../input/gender_submission.csv')
training <- read_csv('../input/train.csv')

# convert factor variables
training$Survived <- factor(training$Survived)
training$Pclass <- factor(training$Pclass)
training$Embarked <- factor(training$Embarked)
training$Sex <- factor(training$Sex)
# since PassengerId cannot be a factor in survival, we will drop that column
# for easier analysis

training$PassengerId <- NULL

head(training)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# starting with logistic regression
logit <- glm(Survived ~ . , data = training, family = 'binomial')

# generates warning message


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# finding the columns with most NA's  as percentage of total observations
colSums(is.na(training))*100/nrow(training)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# splitting into training and testing sets
set.seed(42)
inTrain <- createDataPartition(y=training$Survived,
                               p=0.80, list=FALSE)
modeltrainset <- training[inTrain, ]
modeltestset <- training[-inTrain, ]

# removing Cabin from the model
logit2 <- glm(Survived ~ Pclass + Sex + Age + Embarked, data = modeltrainset, family = 'binomial')
summary(logit2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred <- predict(logit2, modeltestset, type = 'response')
pred[pred > 0.5 ] <- 1
pred[pred < 0.5 | is.na(pred) ] <- 0
pred <- as.factor(pred)
confusionMatrix(pred,modeltestset$Survived)$overall[1]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logit3 <- glm(Survived ~ Pclass + Sex + Age + Embarked + Fare, data = modeltrainset, family = 'binomial')
summary(logit3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logit4 <- glm(Survived ~ Pclass + Sex + Age + Embarked + Fare  + SibSp, data = modeltrainset, family = 'binomial')
summary(logit4)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred2 <- predict(logit4, modeltestset, type='response')
pred2[pred2 > 0.5 ] <- 1
pred2[pred2 < 0.5 | is.na(pred2) ] <- 0
pred2 <- as.factor(pred2)
confusionMatrix(pred2,modeltestset$Survived)$overall[1]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testing <- read_csv('../input/test.csv')
# convert factor variables
testing$Pclass <- factor(testing$Pclass)
testing$Embarked <- factor(testing$Embarked)
testing$Sex <- factor(testing$Sex)
# since PassengerId cannot be a factor in survival, we will drop that column
# for easier analysis

pred_final <- predict(logit4, testing, type='response')
pred_final[pred_final > 0.5 ] <- 1
pred_final[pred_final < 0.5 | is.na(pred_final) ] <- 0

submission_df <- data.frame(PassengerId = testing$PassengerId, Survived = as.numeric(pred_final))

write.csv(submission_df, file = 'submission.csv', row.names = FALSE)

