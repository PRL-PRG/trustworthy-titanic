library(e1071)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train[is.na(train)] <- 0
test[is.na(test)] <- 0
str(train)
str(test)
SVMmodel <- svm(as.factor(Survived) ~ Age + SibSp + Parch + Sex + Fare, data = train, cost = 100, gamma = 1)
prediction <- predict(SVMmodel, test)
plot(prediction)
Pclass = table(prediction, test[, 2])
plot(Pclass)
me = mean(Pclass)
print(me)
output <- data.frame(test$PassengerId, prediction)
colnames(output) = cbind("PassengerId", "Survived")
write.csv(output, file = "Rushton_Solution.csv", row.names = F)
