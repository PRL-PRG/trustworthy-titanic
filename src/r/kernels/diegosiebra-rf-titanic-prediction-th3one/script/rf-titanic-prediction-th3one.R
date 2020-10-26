library(ggplot2)
library(randomForest)
set.seed(1)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
extractTitle <- function(name) {
    name <- as.character(name)
    if (length(grep("Miss.", name)) > 0) {
        return("Miss.")
    }
    else if (length(grep("Master.", name)) > 0) {
        return("Master.")
    }
    else if (length(grep("Mrs.", name)) > 0) {
        return("Mrs.")
    }
    else if (length(grep("Mr.", name)) > 0) {
        return("Mr.")
    }
    else {
        return("Other")
    }
}
extractFeatures <- function(data) {
    features <- c("Pclass", "Age", "Sex", "Parch", "SibSp", "Fare", "Embarked", "Title")
    fea <- data[, features]
    fea$Age[is.na(fea$Age)] <- -1
    fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm = TRUE)
    fea$Embarked[fea$Embarked == ""] = "S"
    fea$Sex <- as.factor(fea$Sex)
    fea$Embarked <- as.factor(fea$Embarked)
    fea$Title <- as.factor(fea$Title)
    return(fea)
}
train$Title <- as.factor(apply(train[c("Name")], 1, extractTitle))
test$Title <- as.factor(apply(test[c("Name")], 1, extractTitle))
rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree = 100, importance = TRUE)
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test))
write.csv(submission, file = "1_random_forest_r_submission.csv", row.names = FALSE)
imp <- importance(rf, type = 1)
featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[, 1])
p <- ggplot(featureImportance, aes(x = reorder(Feature, -Importance), y = Importance)) + geom_bar(stat = "identity", fill = "#53cfff") + theme_bw() + ylim(0, 20) + xlab("Feature") + ylab("Importance") + ggtitle("Random Forest Feature Importance\n") + theme(axis.text = element_text(size = 8))
ggsave("2_feature_importance.png", p)
