library(ggplot2)
library(readr)
system("ls ../input")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
prop.table(table(train$Sex, train$Survived), 1)
table(test$Sex)
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "onlyfemale.csv", row.names = FALSE)
