train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
dim(train)
dim(test)
str(train)
str(test)
strsplit(train$Cabin[2], NULL)[[1]]
train$Deck <- factor(sapply(train$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
test$Deck <- factor(sapply(test$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
train$Survived <- as.factor(train$Survived)
set.seed(20170601)
library(e1071)
m <- naiveBayes(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Deck, data = train, method = "rf", metric = "Accuracy", na.action = na.pass)
m
library(caret)
p <- predict(m, test, type = "class")
head(p, 20)
sol <- data.frame(PassengerID = test$PassengerId, Survived = p)
write.csv(sol, file = "titanic_sol.csv", row.names = F)
