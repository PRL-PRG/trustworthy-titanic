train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
str(train$Sex)
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived), 1)
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gendermodel.csv", row.names = FALSE)
