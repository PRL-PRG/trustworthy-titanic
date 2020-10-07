library(randomForest)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train <- train[, c("Survived", "Fare", "SibSp", "Parch", "Sex")]
test <- test[, c("Fare", "SibSp", "PassengerId", "Parch", "Sex")]
test$Fare <- as.numeric(test$Fare)
train$Fare <- as.numeric(train$Fare)
summary(train)
summary(test)
train$Survived <- as.factor(train$Survived)
test[is.na(test)] <- 0
train[is.na(train)] <- 0
train.rf <- randomForest(Survived ~ Fare + SibSp + Parch + Sex, data = train, type = "response")
train.prediction <- predict(train.rf, test)
train.prediction <- as.numeric(levels(train.prediction))[train.prediction]
train.prediction[is.na(train.prediction)] <- 0
typeof(train.prediction)
summary(train.prediction)
str(train.prediction)
output <- data.frame(test$PassengerId, train.prediction)
colnames(output) <- cbind("PassengerId", "Survived")
write.csv(output, file = "Rushton_Solution.csv", row.names = F)
