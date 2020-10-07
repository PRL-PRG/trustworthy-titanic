library(caret)
library(randomForest)
library(stringr)
trainSet <- read.table("../input/train.csv", sep = ",", header = TRUE)
testSet <- read.table("../input/test.csv", sep = ",", header = TRUE)
head(trainSet)
head(testSet)
table(trainSet[, c("Survived", "Pclass")])
library(fields)
bplot.xy(trainSet$Survived, trainSet$Age)
testSet$Nlen <- str_length(testSet$Name)
trainSet$Nlen <- str_length(trainSet$Name)
trainSet$Fam <- trainSet$Parch + trainSet$SibSp
testSet$Fam <- testSet$Parch + testSet$SibSp
trainSet$Survived <- factor(trainSet$Survived)
trainSet$Title <- regmatches(as.character(trainSet$Name), regexpr("\\,[A-z ]{1,20}\\.", as.character(trainSet$Name)))
trainSet$Title <- unlist(lapply(trainSet$Title, FUN = function(x) substr(x, 3, nchar(x) - 1)))
table(trainSet$Title)
trainSet$Title[which(trainSet$Title %in% c("Mme", "Mlle"))] <- "Miss"
trainSet$Title[which(trainSet$Title %in% c("Lady", "Ms", "the Countess", "Dona"))] <- "Mrs"
trainSet$Title[which(trainSet$Title == "Dr" & trainSet$Sex == "female")] <- "Mrs"
trainSet$Title[which(trainSet$Title == "Dr" & trainSet$Sex == "male")] <- "Mr"
trainSet$Title[which(trainSet$Title %in% c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir"))] <- "Mr"
trainSet$Title <- as.factor(trainSet$Title)
testSet$Title <- regmatches(as.character(testSet$Name), regexpr("\\,[A-z ]{1,20}\\.", as.character(testSet$Name)))
testSet$Title <- unlist(lapply(testSet$Title, FUN = function(x) substr(x, 3, nchar(x) - 1)))
table(testSet$Title)
testSet$Title[which(testSet$Title %in% c("Mme", "Mlle"))] <- "Miss"
testSet$Title[which(testSet$Title %in% c("Lady", "Ms", "the Countess", "Dona"))] <- "Mrs"
testSet$Title[which(testSet$Title == "Dr" & testSet$Sex == "female")] <- "Mrs"
testSet$Title[which(testSet$Title == "Dr" & testSet$Sex == "male")] <- "Mr"
testSet$Title[which(testSet$Title %in% c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir"))] <- "Mr"
testSet$Title <- as.factor(testSet$Title)
set.seed(421)
model <- train(Survived ~ Pclass + Sex + Fam + Nlen + Embarked + Fare + Title, data = trainSet, method = "rf", trControl = trainControl(method = "cv", number = 5))
print(model)
testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)
testSet$Survived <- predict(model, newdata = testSet)
submission <- testSet[, c("PassengerId", "Survived")]
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
