library(ggplot2)
library(readr)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
str(train)
str(test)
train$Name <- as.character(train$Name)
test$Name <- as.character(test$Name)
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
test$Pclass <- as.factor(test$Pclass)
str(train)
table(train$Survived)
summary(train)
plot(train$Sex, train$Survived, col = c("red", "blue"))
colSums(is.na(train))
colSums(is.na(test))
train2 <- train
test2 <- test
test2$Survived <- NA
full <- rbind(train2, test2)
full[!complete.cases(full$Fare), ]
full$Fare[1044] <- median(full$Fare, na.rm = TRUE)
full[!complete.cases(full$Fare), ]
train[is.na(train)] <- median(train$Age, na.rm = TRUE)
test[is.na(test)] <- median(test$Age, na.rm = TRUE)
traindata <- full[1:891, ]
testdata <- full[892:1309, ]
dim(traindata)
dim(testdata)
library(rpart)
dt <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = traindata, method = "class")
prediction <- predict(dt, newdata = testdata, type = "class")
submission <- data.frame(PassengerId = testdata$PassengerId, Survived = prediction)
write.csv(submission, file = "gender_submission.csv", row.names = FALSE)
