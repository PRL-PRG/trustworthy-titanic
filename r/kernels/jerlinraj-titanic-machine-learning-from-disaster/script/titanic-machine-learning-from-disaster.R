train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

str(train$Sex)

prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived), 1)

# Create new prediction that everyone dies
test$Survived <- 0

# Updating the prediction to say that all females will survive
test$Survived[test$Sex == 'female'] <- 1

# Create submission dataframe and output to file
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "gendermodel.csv", row.names = FALSE)
