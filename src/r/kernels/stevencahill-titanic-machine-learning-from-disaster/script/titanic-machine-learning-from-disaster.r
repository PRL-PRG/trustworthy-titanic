train <- read.csv(file = "../input/train.csv", stringsAsFactors = FALSE, header = TRUE)
test <- read.csv(file = "../input/test.csv", stringsAsFactors = FALSE, header = TRUE)
str(train)
head(train, n = 10)
prop.table(table(train$Survived)) * 100
prop.table(table(train$Survived, train$Sex), 1) * 100
prop.table(table(train$Survived, train$Pclass), 1) * 100
prop.table(table(train$Pclass, train$Sex, train$Survived), 1) * 100
test$Survived <- rep(0, 418)
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "TitanicPrediction.csv", row.names = FALSE)
