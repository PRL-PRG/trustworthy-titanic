library(ggplot2)
library(readr)
list.files("../input")
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
str(train)
dim(train)
dim(test)
str(test)
check <- colnames(train) %in% colnames(test)
colnames(train[check == F])
ts <- table(train$Survived)
ts
prop.table(ts)
tsg <- table(train$Sex, train$Survived)
tsg
prop.table(tsg, margin = 1)
tf <- test
tf$Survived <- 0
tf$Survived[tf$Sex == "female"] <- 1
solution <- data.frame(PassengerId = tf$PassengerId, Survived = tf$Survived)
colSums(is.na(train))
colSums(is.na(test))
Train <- train
Test <- test
Test$Survived <- NA
titanic <- rbind(Train, Test)
summary(titanic)
titanic[!complete.cases(titanic$Fare), ]
titanic$Fare[1044] <- mean(titanic$Fare, na.rm = TRUE)
titanic[!complete.cases(titanic$Fare), ]
library(rpart)
fit_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = titanic[is.na(titanic$Age), ], method = "anova")
titanic$Age[is.na(titanic$Age)] <- predict(fit_age, titanic[is.na(titanic$Age), ])
library(rpart)
fit_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = titanic[!is.na(titanic$Age), ], method = "anova")
titanic$Age[is.na(titanic$Age)] <- predict(fit_age, titanic[is.na(titanic$Age), ])
train2 <- titanic[1:891, ]
test2 <- titanic[892:1309, ]
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit <- rpart(Survived ~ Pclass + Sex + Fare + Age + SibSp + Parch + Embarked, data = train2, method = "class")
fancyRpartPlot(fit)
round(prop.table(table(train2$Survived)), 2)
round(prop.table(table(train2$Sex, train2$Survived), margin = 1), 2)
my_pred <- predict(fit, newdata = test2, type = "class")
solution2 <- data.frame(PassengerId = test2$PassengerId, Survived = my_pred)
write.csv(solution2, file = "Tsolution.csv", row.names = FALSE)
