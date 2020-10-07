library(caret)
data = read.csv("../input/train.csv", header = T, sep = ",")
test = read.csv("../input/test.csv", header = T, sep = ",")
summary(data)
table(data$Sex, data$Survived)
data$Child = 0
data$Child[data$Age < 18] = 1
summary(data)
table(data$Child, data$Survived)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
fit = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked, data = data, method = "class")
fancyRpartPlot(fit)
summary(fit)
pred = predict(fit, test, type = "class")
head(pred)
PassengerId = test$PassengerId
Survived = pred
solution <- data.frame(PassengerId, Survived)
head(solution)
write.csv(solution, file = "gender_submission.csv", row.names = F)
