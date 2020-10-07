rm(list = ls())
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)
trainTitanic = read.csv("../input/train.csv", header = TRUE)
testTitanic = read.csv("../input/test.csv", header = TRUE)
str(trainTitanic)
str(testTitanic)
testTitanic$Survived = NA
completeTitanic = rbind(trainTitanic, testTitanic)
head(completeTitanic)
sapply(completeTitanic[, -c(2)], function(x) {
    sum(is.na(x))
})
sapply(completeTitanic[, -c(2)], function(x) {
    sum(x == "")
})
completeTitanic$Fare[is.na(completeTitanic$Fare)] = median(completeTitanic$Fare, na.rm = TRUE)
completeTitanic$Embarked[completeTitanic$Embarked == ""] <- "S"
completeTitanic$FamilySize <- completeTitanic$SibSp + completeTitanic$Parch + 1
completeTitanic$Title <- sapply(as.character(completeTitanic$Name), FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][2]
})
completeTitanic$Title <- sub(" ", "", completeTitanic$Title)
completeTitanic$Title <- as.character(completeTitanic$Title)
completeTitanic$Title[completeTitanic$Title %in% c("Dona", "Lady", "the Countess", "Ms", "Mme", "Mlle")] <- "Mrs"
completeTitanic$Title[completeTitanic$Title %in% c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Rev", "Sir")] <- "Others"
levels(as.factor(completeTitanic$Title))
table(completeTitanic$Title)
completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == "Mr")] <- median(completeTitanic$Age[completeTitanic$Title == "Mr"], na.rm = TRUE)
completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == "Mrs")] <- median(completeTitanic$Age[completeTitanic$Title == "Mrs"], na.rm = TRUE)
completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == "Master")] <- median(completeTitanic$Age[completeTitanic$Title == "Master"], na.rm = TRUE)
completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == "Miss")] <- median(completeTitanic$Age[completeTitanic$Title == "Miss"], na.rm = TRUE)
completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == "Others")] <- median(completeTitanic$Age[completeTitanic$Title == "Others"], na.rm = TRUE)
completeTitanic$Title[completeTitanic$Age < 18] <- "Children"
completeTitanic$Title <- factor(completeTitanic$Title)
levels(completeTitanic$Title)
completeTitanic$Survived = factor(completeTitanic$Survived)
completeTitanic$Pclass = factor(completeTitanic$Pclass)
completeTitanic$Name = as.character(completeTitanic$Name)
completeTitanic$Title = factor(completeTitanic$Title)
summary(completeTitanic)
columnToDelete <- c("Cabin", "PassengerId", "Name", "Ticket")
completeTitanic[, columnToDelete] <- NULL
trainTitanicData <- head(completeTitanic, n = nrow(trainTitanic))
testTitanicData <- tail(completeTitanic, n = nrow(testTitanic))
set.seed(1)
numfold = trainControl(method = "cv", number = 10)
cpgrid = expand.grid(.cp = seq(0.01, 0.7, 0.01))
cpVal = train(Survived ~ ., data = trainTitanicData, method = "rpart", trControl = numfold, tuneGrid = cpgrid)
cpVal
dec_tree <- rpart(Survived ~ ., data = trainTitanicData, cp = 0.02, method = "class")
prp(dec_tree)
pv2 <- predict(dec_tree, type = "class")
table(trainTitanicData$Survived, pv2)
predictTrain <- predict(dec_tree, type = "class")
actualTrain <- trainTitanicData$Survived
table(predictTrain, actualTrain)
confusionMatrix(predictTrain, actualTrain)
precision(predictTrain, actualTrain)
predictTest <- predict(dec_tree, newdata = testTitanicData, type = "class")
table(predictTest)
newdf_ <- data.frame(PassengerId = testTitanic$PassengerId, Survived = 0)
newdf_$Survived <- predictTest
write.csv(newdf_, "output_file.csv", row.names = FALSE)
