library(ggplot2)
library(readr)
system("ls ../input")
train = read.csv("../input/train.csv", header = T, sep = ",")
test = read.csv("../input/test.csv", header = T, sep = ",")
summary(train)
table(train$Survived)
549/(549 + 342)
library(rpart)
mod = rpart(Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked, data = train, method = "class")
summary(mod)
predictions = predict(mod, newdata = test, type = "class")
submission <- data.frame(PassengerID = test$PassengerId, Survived = predictions)
write.csv(submission, file = "submission_tree_basic.csv", row.names = FALSE)
