library(randomForest)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
summary(train)
test$Survived <- rep(0, 418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "submit1.csv", row.names = FALSE)
