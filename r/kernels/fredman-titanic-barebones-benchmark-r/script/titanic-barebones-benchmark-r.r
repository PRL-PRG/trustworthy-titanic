library(ggplot2)
library(readr)
system("ls ../input")
set.seed(1)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
head(train)
summary(train)
selected_features <- c("Pclass", "Age", "Sex", "Parch", "SibSp", "Fare", "Embarked")
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
library(randomForest)
rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree = 100, importance = TRUE)
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test))
write.csv(submission, file = "1_random_forest_r_submission.csv", row.names = FALSE)
