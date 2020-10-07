library(caret)
library(dplyr)
library(VIM)
library(mice)
library(randomForest)
train <- read.csv(file = "../input/train.csv")
test <- read.csv(file = "../input/test.csv")
sub <- read.csv(file = "../input/gender_submission.csv")
str(train)
str(test)
train$Survived <- as.factor(train$Survived)
levels(test$Embarked) <- levels(train$Embarked)
aggr(train, numbers = TRUE)
colSums(is.na(train))
train$Age[is.na(train$Age)] <- mean(train$Age, na.rm = T)
aggr(test, numbers = TRUE)
colSums(is.na(test))
test$Age[is.na(test$Age)] <- mean(test$Age, na.rm = T)
test$Fare[is.na(test$Fare)] <- mean(test$Fare, na.rm = T)
trainInd <- createDataPartition(train$Survived, p = 0.8, list = FALSE)
train_X <- train[trainInd, ]
train_Y <- train[-trainInd, ]
set.seed(5)
RF <- randomForest(Survived ~ ., data = train_X[, -c(1, 4, 9, 11)], ntree = 2000)
pred <- predict(RF, newdata = train_Y)
pred
pred_result <- bind_cols(train_Y, pred = pred)
pred_result[, c(1, 2, 13)]
cm <- table(train_Y$Survived, pred)
accuracy <- sum(diag(cm))/sum(cm)
accuracy
set.seed(6)
Pred <- predict(RF, newdata = test)
Pred
submission <- cbind(sub, Survived = Pred)
submission <- submission[, c(1, 3)]
write.csv(submission, file = "submission.csv", row.names = FALSE)
