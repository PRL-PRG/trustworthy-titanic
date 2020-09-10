
# This script trains a Random Forest model based on the data,
# saves a sample submission, and plots the relative importance
# of the variables in making predictions
        


# I made some changes to add more models including 

library(ggplot2)
library("e1071")


set.seed(1)
train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
test  <- read.csv("../input/test.csv",  stringsAsFactors=FALSE)

extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked")
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex      <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
}

#rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree=300, importance=TRUE)
svm_model <- svm(as.factor(train$Survived) ~ ., data=extractFeatures(train))


#submission <- data.frame(PassengerId = test$PassengerId)
#submission$Survived <- predict(rf, extractFeatures(test))
#write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)


submission_svm <- data.frame(PassengerId = test$PassengerId)
submission_svm$Survived <- predict(svm_model, extractFeatures(test))
write.csv(submission_svm, file = "1_svm_r_submission.csv", row.names=FALSE)

