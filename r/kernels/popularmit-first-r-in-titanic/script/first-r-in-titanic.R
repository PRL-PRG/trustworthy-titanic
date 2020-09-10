
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(party)

# The train and test data is stored in the ../input directory



set.seed(1)
trainData <- read.csv("../input/train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("../input/test.csv", header = TRUE, stringsAsFactors = FALSE)

dataprepatation <- function(data) {
       features <- c("Pclass", "Age", "Sex","Parch","SibSp", "Fare","Embarked")
     fea <- data[,features]
     
     fea$Age[is.na(fea$Age)] <- mean(fea$Age,na.rm=TRUE)
     fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
     fea$Embarked[fea$Embarked==""] = "S"
     fea$Sex      <- as.factor(fea$Sex)
     fea$Embarked <- as.factor(fea$Embarked)
     return(fea)
 }

trainDataNoSurvived=dataprepatation(trainData)
trainDatawithSurvived=cbind(trainDataNoSurvived,trainData$Survived)

fit <- cforest(as.factor(trainData$Survived) ~ Pclass + Sex + Age + Parch+Fare+Embarked,
               data = dataprepatation(trainData), 
               controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, dataprepatation(testData), OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = testData$PassengerId, Survived = Prediction)
 write.csv(submit, file = "firstsubmiisionforTitanic.csv", row.names = FALSE)


