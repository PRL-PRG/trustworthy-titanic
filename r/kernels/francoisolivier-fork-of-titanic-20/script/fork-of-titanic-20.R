library(ggplot2)
library(randomForest)
set.seed(69)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
extractFeatures <- function(data) {
    features <- c("Pclass", "Age", "Sex", "Parch", "SibSp", "Fare", "Embarked")
    fea <- data[, features]
    fea$Age[is.na(fea$Age)] <- median(fea$Age, na.rm = TRUE)
    fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm = TRUE)
    fea$Embarked[fea$Embarked == ""] = "S"
    fea$Sex <- as.factor(fea$Sex)
    fea$Embarked <- as.factor(fea$Embarked)
    return(fea)
}
rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree = 5000, mtry = 4, importance = TRUE)
print(rf)
varImpPlot(rf)
rf$importance
rf$importance[order(rf$importance[, 1], decreasing = TRUE), ]
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test))
write.csv(submission, file = "1_random_forest_r_submission.csv", row.names = FALSE)
