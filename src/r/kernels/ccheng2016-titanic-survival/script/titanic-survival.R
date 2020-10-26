library(ggplot2)
library(readr)
library(rpart)
library(randomForest)
library(e1071)
library(party)
list.files("../input")
trainData <- read.table("../input/train.csv", sep = ",", header = TRUE)
head(trainData)
str(trainData)
table(trainData$Survived)
prop.table(table(trainData$Survived))
testData <- read.table("../input/test.csv", sep = ",", header = T)
str(testData)
testData$Survived <- rep(0, nrow(testData))
submit <- data.frame(PassengerId = testData$PassengerId, Survived = testData$Survived)
write.csv(submit, file = "./test1.csv", row.names = F)
summary(trainData$Sex)
prop.table(table(trainData$Sex, trainData$Survived))
prop.table(table(trainData$Sex, trainData$Survived), 1)
testData$Survived <- 0
testData$Survived[testData$Sex == "female"] <- 1
testData$Survived[testData$Age < 18] <- 1
submit <- data.frame(PassengerId = testData$PassengerId, Survived = testData$Survived)
write.csv(submit, file = "./test2.csv", row.names = F)
summary(trainData$Age)
summary(trainData$Sex)
trainData$Child <- 0
trainData$Child[trainData$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data = trainData, FUN = sum)
aggregate(Survived ~ Child + Sex, data = trainData, FUN = length)
aggregate(Survived ~ Child + Sex, data = trainData, FUN = function(x) {
    sum(x)/length(x)
})
trainData$Fare2 <- "30+"
trainData$Fare2[trainData$Fare < 30 & trainData$Fare >= 20] <- "20-30"
trainData$Fare2[trainData$Fare < 20 & trainData$Fare >= 10] <- "10-20"
trainData$Fare2[trainData$Fare < 10] <- "<10"
aggregate(Survived ~ Child + Fare2 + Sex, data = trainData, FUN = function(x) {
    sum(x)/length(x)
})
aggregate(Survived ~ Fare2 + Pclass + Child + Sex, data = trainData, FUN = function(x) {
    sum(x)/length(x)
})
fol <- formula(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare)
modelR <- rpart(fol, method = "class", data = trainData)
print(modelR)
modelR <- rpart(fol, method = "class", data = trainData)
guessR <- predict(modelR, newdata = testData, type = "class")
accuracy <- (sum(guessR == testData$Survived)/nrow(testData))
print(accuracy)
plot(modelR)
text(modelR)
dev.copy(png, file = "./rpart.png", height = 480, width = 480)
dev.off()
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(modelR)
dev.copy(png, file = "./rpart2.png", height = 480, width = 480)
dev.off()
submit <- data.frame(PassengerId = testData$PassengerId, Survived = guessR)
write.csv(submit, file = "./test2final.csv", row.names = FALSE)
trainData$Name[1]
trainData <- read.table("../input/train.csv", sep = ",", header = TRUE)
testData$Survived <- NA
combi <- rbind(trainData, testData)
combi$Name <- as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name[1], split = "[,.]")
strsplit(combi$Name[1], split = "[,.]")[[1]]
strsplit(combi$Name[1], split = "[,.]")[[1]][[2]]
combi$Title <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][[2]]
})
combi$Title <- sub(" ", "", combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c("Mme", "Mlle")] <- "Mlle"
combi$Title[combi$Title %in% c("Capt", "Don", "Major", "Sir")] <- "Sir"
combi$Title[combi$Title %in% c("Dona", "Lady", "the Countess", "Jonkheer")] <- "Lady"
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][1]
})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")
combi$FamilyID[combi$FamilySize <= 2] <- "Small"
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2, ]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- "Small"
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891, ]
test <- combi[892:1309, ]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, method = "class")
fancyRpartPlot(fit)
dev.copy(png, file = "./featureEngineering.png", height = 480, width = 480)
dev.off()
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "./featureEngineering.csv", row.names = FALSE)
sample(1:10, replace = TRUE)
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = combi[!is.na(combi$Age), ], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age), ])
summary(combi$Embarked)
which(combi$Embarked == "")
combi$Embarked[c(62, 830)] = "S"
combi$Embarked <- factor(combi$Embarked)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)
combi$Fare[1044]
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- "Small"
combi$FamilyID2 <- factor(combi$FamilyID2)
library(randomForest)
train <- combi[1:891, ]
test <- combi[892:1309, ]
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data = train, importance = TRUE, ntree = 2000)
varImpPlot(fit)
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "./firstforest.csv", row.names = FALSE)
set.seed(415)
fol <- as.factor(Survived) ~ Pclass + Sex + Age + Fare + Embarked + Title + FamilySize + FamilyID2
fit <- randomForest(fol, data = train, importance = TRUE, ntree = 3000)
varImpPlot(fit)
Prediction <- predict(fit, test)
accuracy2RF <- (sum(Prediction == test$Survived)/nrow(test))
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "./secondforest.csv", row.names = FALSE)
accuracy2RF
