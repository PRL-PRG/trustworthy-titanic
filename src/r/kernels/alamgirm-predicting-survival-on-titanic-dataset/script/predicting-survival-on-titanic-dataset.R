library(ggplot2)
library(readr)
system("ls ../input")
library(mice)
library(e1071)
library(randomForest)
print("Building model ...")
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
factCols <- c("Name", "Sex", "Ticket", "Cabin", "Embarked")
train[factCols] <- lapply(train[factCols], function(x) {
    x <- trimws(x)
    x <- ifelse(x == "", NA, x)
})
train$Salutation <- lapply(train$Name, function(x) {
    re <- regexpr("(Jonkheer[.])|(Countess[.])|(Capt[.])|(Col[.])|(Mlle[.])|(Sir[.])|(Lady[.])|(Major[.])|(Ms[.])|(Mr[.])|(Miss[.])|(Mrs[.])|(Master[.])|(Don[.])|(Rev[.])|(Dr[.])|(Mme[.])", x)
    m <- regmatches(x, re)
    if (attributes(re)["match.length"] == -1) 
        m <- NA
    as.character(m)
})
train$Salutation <- unlist(lapply(train$Salutation, function(x) {
    y <- sub("(Mlle[.])", "Miss.", x)
    y <- sub("(Mme[.])|(Ms[.])|(Countess[.])|(Lady[.])", "Mrs.", y)
    y <- sub("(Jonkheer[.])|(Capt[.])|(Col[.])|(Major[.])|(Don[.])|(Rev[.])|(Dr[.])|(Sir[.])", "Spl.", y)
    y
}))
salLevels <- sort(unique(train$Salutation))
salLabels <- 1:length(salLevels)
train$Salutation <- factor(train$Salutation, levels = salLevels, labels = salLabels)
train$FamSize <- train$SibSp + train$Parch + 1
train$FSizeFactor <- character(nrow(train))
train$FSizeFactor <- "med"
train$FSizeFactor[train$FamSize == 1] <- "small"
train$FSizeFactor[train$FamSize > 4] <- "big"
famLevels <- c("small", "med", "big")
famLabels <- 1:3
train$FamSize <- factor(train$FSizeFactor, levels = famLevels, labels = famLabels)
train$FSizeFactor <- NULL
train$Survived <- factor(train$Survived, levels = c(0, 1), labels = c(0, 1))
train$Pclass <- factor(train$Pclass, levels = c(1, 2, 3), labels = c(1, 2, 3))
train$Sex <- factor(train$Sex, levels = c("female", "male"), labels = c(0, 1))
train$Embarked <- factor(train$Embarked, levels = c("C", "Q", "S"), labels = c(1, 2, 3))
selFeatures <- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Salutation", "FamSize")
sel_train <- train[selFeatures]
imp <- mice(sel_train, printFlag = F)
train_full <- complete(imp, 1)
train_size <- nrow(train_full) * 0.8
set.seed(123)
train_ind <- sample(seq_len(nrow(train_full)), size = train_size)
train_set <- train_full[train_ind, ]
test_set <- train_full[-train_ind, ]
svmModel <- svm(formula = train_set$Survived ~ ., data = train_set[, -1], cost = 1, gamma = 0.5, type = "C-classification", kernel = "radial")
svmPred <- predict(svmModel, test_set[, -1], type = "class")
cm1 <- table(svmPred, test_set[, 1])
print(cm1)
rfModel <- randomForest(x = train_set[, -1], y = train_set[, 1])
rfPred <- predict(rfModel, test_set[, -1], type = "class")
cm2 <- table(rfPred, test_set[, 1])
print(cm2)
plot(rfModel)
importance(rfModel)
print("Testing the model ...")
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
test[factCols] <- lapply(test[factCols], function(x) {
    x <- trimws(x)
    x <- ifelse(x == "", NA, x)
})
test$Salutation <- lapply(test$Name, function(x) {
    re <- regexpr("(Jonkheer[.])|(Countess[.])|(Capt[.])|(Col[.])|(Mlle[.])|(Sir[.])|(Lady[.])|(Major[.])|(Ms[.])|(Mr[.])|(Miss[.])|(Mrs[.])|(Master[.])|(Don[.])|(Rev[.])|(Dr[.])|(Mme[.])", x)
    m <- regmatches(x, re)
    if (attributes(re)["match.length"] == -1) 
        m <- NA
    as.character(m)
})
test$Salutation <- unlist(lapply(test$Salutation, function(x) {
    y <- sub("(Mlle[.])", "Miss.", x)
    y <- sub("(Mme[.])|(Ms[.])|(Countess[.])|(Lady[.])", "Mrs.", y)
    y <- sub("(Jonkheer[.])|(Capt[.])|(Col[.])|(Major[.])|(Don[.])|(Rev[.])|(Dr[.])|(Sir[.])", "Spl.", y)
    y
}))
test$Salutation <- factor(test$Salutation, levels = salLevels, labels = salLabels)
test$FamSize <- test$SibSp + test$Parch + 1
test$FSizeFactor <- character(nrow(test))
test$FSizeFactor <- "med"
test$FSizeFactor[test$FamSize == 1] <- "small"
test$FSizeFactor[test$FamSize > 4] <- "big"
test$FamSize <- factor(test$FSizeFactor, levels = famLevels, labels = famLabels)
test$FSizeFactor <- NULL
test$Pclass <- factor(test$Pclass, levels = c(1, 2, 3), labels = c(1, 2, 3))
test$Sex <- factor(test$Sex, levels = c("female", "male"), labels = c(0, 1))
test$Embarked <- factor(test$Embarked, levels = c("C", "Q", "S"), labels = c(1, 2, 3))
selFeatures <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Salutation", "FamSize")
sel_test <- test[selFeatures]
imp <- mice(sel_test, printFlag = F)
test_full <- complete(imp, 1)
pred_rf <- predict(rfModel, test_full, type = "class")
solution <- data.frame(PassengerId = test$PassengerId, Survived = pred_rf)
write.csv(solution, "gender_submission.csv", row.names = F, quote = F)
