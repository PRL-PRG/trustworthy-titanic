library(ggplot2)
library(readr)
library(dplyr)
library(corrplot)
library(randomForest)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = c("NA", ""))
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE, header = TRUE, na.strings = c("NA", ""))
alldata <- bind_rows(train, test)
alldata$Survived <- as.factor(alldata$Survived)
alldata$Pclass <- as.factor(alldata$Pclass)
alldata$Sex <- as.factor(alldata$Sex)
alldata$Embarked <- as.factor(alldata$Embarked)
sapply(alldata, function(x) sum(is.na(x)))
cordata <- cbind(alldata$Pclass, alldata$Sex, alldata$Age, alldata$SibSp, alldata$Parch, alldata$Fare, alldata$Embarked, alldata$Survived)
corrplot(cor(cordata, use = "complete.obs"), type = "lower", addCoef.col = "grey")
alldata[is.na(alldata$Fare) == TRUE, ]
mean(alldata$Fare[alldata$Pclass == 3 & alldata$Embarked == "S"], na.rm = TRUE)
alldata$Fare[1044] <- mean(alldata$Fare[alldata$Pclass == 3 & alldata$Embarked == "S"], na.rm = TRUE)
alldata[is.na(alldata$Embarked) == TRUE, ]
ggplot(data = alldata, aes(x = Embarked, y = Fare)) + geom_boxplot() + facet_grid(~Pclass) + geom_hline(aes(yintercept = 80), colour = "#990000", linetype = "dashed")
alldata$Embarked[c(62, 830)] <- "C"
traindata <- alldata[1:891, ]
testdata <- alldata[892:1309, ]
rf.fit <- randomForest(Survived ~ Pclass + Sex + Fare + Embarked, data = traindata, importance = TRUE, proximity = TRUE)
print(rf.fit)
prediction.rf <- predict(rf.fit, testdata)
solution <- data.frame(PassengerID = testdata$PassengerId, Survived = prediction.rf)
write.csv(solution, file = "Solution_rf.csv", row.names = F)
