library(randomForest)
library(dplyr)
library(corrplot)
library(data.table)
library(rpart)
train <- read.csv("../input/train.csv")
setDT(train)
train$IsTrain <- TRUE
test <- read.csv("../input/test.csv")
setDT(test)
test$IsTrain <- FALSE
test$Survived <- NA
full <- rbind(train, test)
head(full$Fare)
full$Fare[which(is.na(full$Fare))] <- full$Pclass <- as.factor(full$Pclass)
full$Embarked <- as.factor(full$Embarked)
full$Sex <- as.factor(full$Sex)
train <- full[full$IsTrain == TRUE, ]
test <- full[full$IsTrain == FALSE, ]
train$Survived <- as.factor(train$Survived)
predicted_age_train <- rpart(Age ~ Survived + Sex + SibSp + Pclass + Parch + Ticket + Fare + Embarked, data = train[!is.na(train$Age), ], method = "anova")
train$Age[is.na(train$Age)] <- predict(predicted_age_train, train[is.na(train$Age), ])
predicted_age_test <- rpart(Age ~ Sex + SibSp + Pclass + Parch + Ticket + Fare + Embarked, data = test[!is.na(test$Age), ], method = "anova")
test$Age[is.na(test$Age)] <- predict(predicted_age_test, test[is.na(test$Age), ])
model <- randomForest(Survived ~ Age + Sex + SibSp + Pclass + Parch + Fare + Embarked, data = train, importance = TRUE, ntree = 300, set.seed(111))
predictions <- predict(model, test)
colSums(sapply(test, is.na))
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = predictions)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
