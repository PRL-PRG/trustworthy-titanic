library(ggplot2)
library(readr)
library(pROC)
system("ls ../input")
my.train.data <- read.csv("../input/train.csv")
my.test.data <- read.csv("../input/test.csv")
my.train.data
my.train.data$Cabin <- ifelse(is.na(my.train.data$Cabin), "NA", my.train.data$Cabin)
drop.cols <- c("PassengerId", "Name", "Ticket", "Cabin")
my.train.data <- my.train.data[, !names(my.train.data) %in% drop.cols]
for (i in 1:ncol(my.train.data)) {
    if (is.numeric(my.train.data[, i])) {
        my.train.data[is.na(my.train.data[, i]), i] <- mean(my.train.data[, i], na.rm = TRUE)
    }
}
for (i in 1:ncol(my.test.data)) {
    if (is.numeric(my.test.data[, i])) {
        my.test.data[is.na(my.test.data[, i]), i] <- mean(my.test.data[, i], na.rm = TRUE)
    }
}
set.seed(1313)
analysis <- glm(Survived ~ ., data = my.train.data, family = binomial(link = "logit"))
summary(analysis)
score <- data.frame(Survived = predict(analysis, newdata = my.test.data, type = "response"))
score_train <- data.frame(Prediction = predict(analysis, newdata = my.train.data, type = "response"))
auc(my.train.data$Survived, score_train$Prediction)
score$Survived <- ifelse(score$Survived > 0.5, 1, 0)
complete <- cbind(my.test.data, score)
write_csv(complete[, c("PassengerId", "Survived")], path = "myPredictions.csv")
