
library(readr)

train <- read_csv("../input/train.csv") #importing the train.csv file
test <- read_csv("../input/test.csv")   #importing the test.csv file
# prop.table(table(train$Sex, train$Survived),1)
test$Survived<- 0
test$Survived[test$Sex == 'female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "Result.csv", row.names = FALSE)
