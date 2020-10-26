library(randomForest)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Name <- as.character(train$Name)
test$Pclass <- as.factor(test$Pclass)
test$Name <- as.character(test$Name)
summary(train)
summary(test)
library(dplyr)
subset_1 = filter(test, Pclass == 3 & Embarked == "S")
summarise(subset_1, delay = mean(Fare, na.rm = TRUE))
test$Fare[which(is.na(test$Fare))] = 13.91
summary(test)
Titles <- function(Name) {
    if (length(grep("Miss.", Name)) > 0) {
        return("Lady")
    }
    else if (length(grep("Mrs.", Name)) > 0) {
        return("Woman")
    }
    else if (length(grep("Mr.", Name)) > 0) {
        return("Man")
    }
    else if (length(grep("Master.", Name)) > 0) {
        return("Child")
    }
    else {
        return("Other")
    }
}
title_data <- NULL
for (i in 1:nrow(train)) {
    title_data <- c(title_data, Titles(train[i, 4]))
}
train$title <- as.factor(title_data)
title_data <- NULL
for (i in 1:nrow(test)) {
    title_data <- c(title_data, Titles(test[i, 3]))
}
test$title <- as.factor(title_data)
rf_data <- train[, c("Pclass", "title", "Fare")]
rf_newdata <- test[, c("Pclass", "title", "Fare")]
set.seed(12354)
rf_1 = randomForest(x = rf_data, y = train$Survived, ntree = 1000)
rf_1
rf_2 = predict(rf_1, newdata = rf_newdata)
rf_2
my_solution <- data.frame(PassengerID = test$PassengerId, Survived = rf_2)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
