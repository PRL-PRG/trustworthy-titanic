library("readr")
library("dplyr")
library("plyr")
library("randomForest")
train <- read_csv('../input/train.csv')
test  <- read_csv('../input/test.csv')
train$datatype <- "train"
test$datatype <- "test"
alldata <- bind_rows(train, test)
alldata$Pclass   <- factor(alldata$Pclass)
alldata$Sex      <- factor(alldata$Sex)
alldata$Embarked <- factor(alldata$Embarked)
train_1 <- alldata[alldata$datatype=="train", c("Survived","Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
test_1  <- alldata[alldata$datatype=="test",  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
summary(alldata)
rf <- randomForest(factor(Survived)~.
                     , data=train_1, metry=2,importance=T, na.action = na.roughfix, ntree=150)
plot(rf)
rf
test_roughfix <- na.roughfix(test_1)
rf.pd <- predict(rf, test_roughfix)
