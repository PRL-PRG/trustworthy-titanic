train <- read.csv('../input/train.csv')
test <- read.csv('../input/test.csv')
str(train)
str(test)
table(train$Survived)
test$Survived <- rep(0, 418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "alldie.csv", row.names = F)






































