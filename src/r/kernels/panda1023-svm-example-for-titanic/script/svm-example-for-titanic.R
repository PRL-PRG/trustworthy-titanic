library(ggplot2)
library("e1071")
set.seed(1)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
extractFeatures <- function(data) {
    features <- c("Pclass", "Age", "Sex", "Parch", "SibSp", "Fare", "Embarked")
    fea <- data[, features]
    fea$Age[is.na(fea$Age)] <- -1
    fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm = TRUE)
    fea$Embarked[fea$Embarked == ""] = "S"
    fea$Sex <- as.factor(fea$Sex)
    fea$Embarked <- as.factor(fea$Embarked)
    return(fea)
}
svm_model <- svm(as.factor(train$Survived) ~ ., data = extractFeatures(train))
submission_svm <- data.frame(PassengerId = test$PassengerId)
submission_svm$Survived <- predict(svm_model, extractFeatures(test))
write.csv(submission_svm, file = "1_svm_r_submission.csv", row.names = FALSE)
