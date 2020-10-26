library(readr)
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "Result.csv", row.names = FALSE)
