knitr::opts_chunk$set(echo = TRUE)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(corrplot)
library(dplyr)
testData <- read.csv("../input/test.csv", sep = ",", stringsAsFactors = F, header = T)
trainData <- read.csv("../input/train.csv", sep = ",", stringsAsFactors = F, header = T)
testData$Survived <- NA
trainData$sample = "training"
testData$sample = "testing"
trailData <- bind_rows(trainData, testData)
tempData <- trailData
trailData$Pclass <- as.factor(trailData$Pclass)
trailData$Sex <- as.factor(trailData$Sex)
trailData$Embarked <- as.factor(trailData$Embarked)
str(trailData)
summary(trailData)
sum(is.na(trailData$Age))
trailData <- mutate(trailData, AgeCat = ifelse(SibSp > 1, 0, ifelse(Parch > 2, 1, 2)))
ageModel = rpart(Age ~ Fare + Pclass + SibSp + Parch + AgeCat, data = trailData)
trailData$predictAge = predict(ageModel, trailData)
trailData$Age <- ifelse(is.na(trailData$Age), trailData$predictAge, trailData$Age)
numOfNa <- sum(is.na(trailData$Age))
numOfNa
which(trailData$Embarked == "")
trailData[trailData$Embarked == "", ]
trailData$Embarked <- as.factor(trailData$Embarked)
emptyEmbarked <- which(trailData$Embarked == "")
embarkedModel <- train(Embarked ~ Pclass + Fare, data = trailData, method = "rpart", na.action = na.pass)
trailData$Embarked[emptyEmbarked] <- predict(embarkedModel, trailData[emptyEmbarked, ])
sum(is.na(trailData$Fare))
trailData[is.na(trailData$Fare), ]
fareModel <- rpart(Fare ~ Age + Pclass + Embarked + SibSp + Parch, data = trailData)
emptyFare <- which(is.na(trailData$Fare))
trailData$Fare[emptyFare] <- predict(fareModel, trailData[emptyFare, ])
sum(is.na(trailData$Fare))
count <- length(which(trailData$Cabin == ""))
count
trnData <- trailData[trailData$sample == "training", ]
plot <- ggplot(trnData, aes(Pclass, fill = factor(Survived)))
plot <- plot + geom_bar(stat = "count", position = "dodge")
plot <- plot + facet_grid(Sex ~ .) + coord_flip()
plot <- plot + labs(title = "Men and Women Survived by Passenger Class", x = "Ticket class accorinding to socio-economic status", y = "Survival Count", fill = "Survival")
plot
plot2 <- ggplot(trnData, aes(x = Embarked, fill = factor(Survived)))
plot2 <- plot2 + geom_bar(stat = "count", position = "dodge")
plot2 <- plot2 + ggtitle("Passengers per Boarding Location and Survival rate.")
plot2 <- plot2 + ylab("Survival Count")
plot2 <- plot2 + xlab("Boarding Location") + theme_minimal()
plot2 <- plot2 + scale_fill_discrete(name = "Survival")
plot2
plot <- ggplot(trnData, aes(x = Age, y = Fare))
plot <- plot + geom_point(aes(shape = factor(Survived), colour = factor(Survived)))
plot <- plot + facet_grid(Sex ~ .) + coord_flip()
plot <- plot + labs(title = "Survival by Age, Sex and Fare", x = "Age Of Passengers", y = "Fare passengers paid", fill = "Survival")
plot
trailData <- mutate(trailData, fitness = as.factor(ifelse(trailData$Age <= 16, "Child", ifelse(trailData$Age >= 50, "Old", "Adult"))))
table(trailData$Sex, trailData$fitness)
trnData <- trailData[trailData$sample == "training", ]
plot <- ggplot(trnData, aes(Pclass, fill = factor(Survived)))
plot <- plot + geom_bar(stat = "count", position = "dodge")
plot <- plot + facet_grid(Sex ~ fitness)
plot <- plot + labs(title = "Men and Women Survived by Passenger Class and Physical fitness", x = "Ticket class accorinding to socio-economic status", y = "Survival Count", fill = "Survival")
plot
trailData <- mutate(trailData, familySize = trailData$SibSp + trailData$Parch + 1)
trnData <- trailData[trailData$sample == "training", ]
plot1 <- ggplot(trnData, aes(x = familySize, fill = factor(Survived)))
plot1 <- plot1 + geom_bar(stat = "count", position = "dodge")
plot1 <- plot1 + ggtitle("Survival according to family size.")
plot1 <- plot1 + ylab("Survival Count")
plot1 <- plot1 + xlab("Family Size") + theme_minimal()
plot1 <- plot1 + scale_fill_discrete(name = "Survival")
plot1
trailData <- mutate(trailData, familyType = as.factor(ifelse(familySize == 1, "Single", ifelse(familySize > 1 & familySize <= 4, "Small", "Big"))))
trailData$Title <- gsub("(.*, )|(\\..*)", "", trailData$Name)
table(trailData$Sex, trailData$Title)
trailData$Title <- as.factor(ifelse(trailData$Title == "Mlle", "Miss", ifelse(trailData$Title == "Ms", "Miss", ifelse(trailData$Title == "Mme", "Mrs", ifelse(trailData$Title == "Miss", "Miss", ifelse(trailData$Title == "Mrs", "Mrs", ifelse(trailData$Title == "Mr", "Mr", ifelse(trailData$Title == "Master", "Master", "Not Known"))))))))
table(trailData$Sex, trailData$Title)
trnData <- trailData[trailData$sample == "training", ]
plot <- ggplot(trnData, aes(factor(Survived), fill = Title)) + geom_bar(position = "dodge")
plot <- plot + labs(title = "Survival Vs Title", x = "Survival", y = "Count", fill = "Title")
plot
set.seed(22519)
tstData <- trailData[trailData$sample == "testing", ]
tranData <- trailData[trailData$sample == "training", ]
fitControl <- trainControl(method = "cv", number = 3, verboseIter = F)
fit <- train(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + fitness + familyType + Title, data = tranData, method = "rf", trControl = fitControl, importance = TRUE)
plot(fit)
finalModel <- fit$finalModel
plot(finalModel)
legend("topright", colnames(finalModel$err.rate), col = 1:3, fill = 1:3)
fit$finalModel
predict <- predict(fit, newdata = tstData)
solution <- data.frame(PassengerID = tstData$PassengerId, Survived = predict)
write.csv(solution, file = "predictionSurvivalTitanic.csv", row.names = F)
str(tempData)
summary(tempData)
fit
trnData <- trailData[trailData$sample == "training", ]
plot <- ggplot(trnData, aes(Pclass, fill = factor(Survived)))
plot <- plot + geom_bar(stat = "count", position = "dodge")
plot <- plot + facet_grid(Sex ~ .) + coord_flip()
plot <- plot + labs(title = "Men and Women Survived by Passenger Class", x = "Ticket class accorinding to socio-economic status", y = "Survival Count", fill = "Survival")
plot
plot2 <- ggplot(trnData, aes(x = Embarked, fill = factor(Survived)))
plot2 <- plot2 + geom_bar(stat = "count", position = "dodge")
plot2 <- plot2 + ggtitle("Passengers per Boarding Location and Survival rate.")
plot2 <- plot2 + ylab("Survival Count")
plot2 <- plot2 + xlab("Boarding Location") + theme_minimal()
plot2 <- plot2 + scale_fill_discrete(name = "Survival")
plot2
plot <- ggplot(trnData, aes(x = Age, y = Fare))
plot <- plot + geom_point(aes(shape = factor(Survived), colour = factor(Survived)))
plot <- plot + facet_grid(Sex ~ .) + coord_flip()
plot <- plot + labs(title = "Survival by Age, Sex and Fare", x = "Age Of Passengers", y = "Fare passengers paid", fill = "Survival")
plot
trnData <- trailData[trailData$sample == "training", ]
plot <- ggplot(trnData, aes(Pclass, fill = factor(Survived)))
plot <- plot + geom_bar(stat = "count", position = "dodge")
plot <- plot + facet_grid(Sex ~ fitness)
plot <- plot + labs(title = "Men and Women Survived by Passenger Class and Physical fitness", x = "Ticket class accorinding to socio-economic status", y = "Survival Count", fill = "Survival")
plot
trnData <- trailData[trailData$sample == "training", ]
plot1 <- ggplot(trnData, aes(x = familySize, fill = factor(Survived)))
plot1 <- plot1 + geom_bar(stat = "count", position = "dodge")
plot1 <- plot1 + ggtitle("Survival according to family size.")
plot1 <- plot1 + ylab("Survival Count")
plot1 <- plot1 + xlab("Family Size") + theme_minimal()
plot1 <- plot1 + scale_fill_discrete(name = "Survival")
plot1
trnData <- trailData[trailData$sample == "training", ]
plot <- ggplot(trnData, aes(factor(Survived), fill = Title)) + geom_bar(position = "dodge")
plot <- plot + labs(title = "Survival Vs Title", x = "Survival", y = "Count", fill = "Title")
plot
