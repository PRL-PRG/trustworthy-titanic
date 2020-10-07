library(randomForest)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train$Title <- gsub("(.*, )|(\\..*)", "", train$Name)
table(train$Sex, train$Title)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
train$Title[train$Title == "Mlle"] <- "Miss"
train$Title[train$Title == "Ms"] <- "Miss"
train$Title[train$Title == "Mme"] <- "Mrs"
train$Title[train$Title %in% rare_title] <- "Rare Title"
table(train$Sex, train$Title)
train$Age[is.na(train$Age) & train$Title == "Mr"] <- mean(train$Age[train$Title == "Mr"], na.rm = T)
train$Age[is.na(train$Age) & train$Title == "Miss"] <- mean(train$Age[train$Title == "Miss"], na.rm = T)
train$Age[is.na(train$Age) & train$Title == "Mrs"] <- mean(train$Age[train$Title == "Mrs"], na.rm = T)
train$Age[is.na(train$Age) & train$Title == "Master"] <- mean(train$Age[train$Title == "Master"], na.rm = T)
train$Age[is.na(train$Age) & train$Title == "Rare Title"] <- mean(train$Age[train$Title == "Rare Title"], na.rm = T)
levels(test$Sex) <- levels(train$Sex)
levels(test$Age) <- levels(train$Age)
levels(test$Embarked) <- levels(train$Embarked)
extractFeatures <- function(data) {
    features <- c("Sex", "Embarked")
    ftr <- data[, features]
    ftr$Embarked[ftr$Embarked == ""] = "S"
    ftr$Sex <- as.factor(ftr$Sex)
    ftr$Embarked <- as.factor(ftr$Embarked)
    return(ftr)
}
rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree = 100, importance = TRUE)
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test))
write.csv(submission, file = "submission.csv", row.names = FALSE)
