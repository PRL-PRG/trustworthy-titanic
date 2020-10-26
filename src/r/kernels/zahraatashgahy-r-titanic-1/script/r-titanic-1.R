library(rpart)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train$Child <- NA
train$Child[train$Age < 10] <- 1
train$Child[train$Age >= 10] <- 0
test$Child <- NA
test$Child[test$Age < 10] <- 1
test$Child[test$Age >= 10] <- 0
train$family_size <- NA
train$family_size <- train$SibSp + train$Parch + 1
test$family_size <- NA
test$family_size <- test$SibSp + test$Parch + 1
train$Embarked[c(62, 891)] <- "S"
test$Embarked[c(62, 418)] <- "S"
train$Embarked <- factor(train$Embarked)
test$Embarked <- factor(test$Embarked)
my_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
my_prediction <- predict(my_tree, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
