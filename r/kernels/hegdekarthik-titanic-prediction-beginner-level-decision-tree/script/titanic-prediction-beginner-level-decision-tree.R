## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Clear the workspace
rm(list = ls())

#load packages
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Read the data
trainTitanic = read.csv( "../input/train.csv", header = TRUE)
testTitanic = read.csv( "../input/test.csv", header = TRUE)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(trainTitanic)
str(testTitanic)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Fill Survived column of test data to NA to make it homogeneous with the test data
testTitanic$Survived = NA


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Combine the data
completeTitanic = rbind(trainTitanic, testTitanic)
head(completeTitanic)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sapply(completeTitanic[,-c(2)], function(x){sum(is.na(x))})
sapply(completeTitanic[,-c(2)], function(x){sum(x == "")})


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
completeTitanic$Fare[is.na(completeTitanic$Fare)] = median(completeTitanic$Fare, na.rm = TRUE)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Fill the 'Embarked' with the mode, which is 'S'
completeTitanic$Embarked[completeTitanic$Embarked == ""] <- 'S'


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a new variable which contains siblings, spouse and individuals
completeTitanic$FamilySize <- completeTitanic$SibSp + completeTitanic$Parch + 1


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Process the data to split the Titles in the name
completeTitanic$Title <- sapply(as.character(completeTitanic$Name), FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
completeTitanic$Title <- sub(' ', '', completeTitanic$Title)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Before finding/filling the age, combine the sir names to most common ones.
completeTitanic$Title <- as.character(completeTitanic$Title)
completeTitanic$Title[completeTitanic$Title %in% c('Dona', 'Lady', 'the Countess', 'Ms', 'Mme', 'Mlle')] <- 'Mrs'
completeTitanic$Title[completeTitanic$Title %in% c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 'Rev', 'Sir')] <- 'Others'

#To check how many titles do we have now
levels(as.factor(completeTitanic$Title))



## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Check the number of people with Titles
table(completeTitanic$Title)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Fill the ages with respect to their Title

completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == 'Mr')] <- median(completeTitanic$Age[completeTitanic$Title == "Mr"],na.rm = TRUE)
completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == 'Mrs')] <- median(completeTitanic$Age[completeTitanic$Title == "Mrs"],na.rm = TRUE)
completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == 'Master')] <- median(completeTitanic$Age[completeTitanic$Title == "Master"],na.rm = TRUE)
completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == 'Miss')] <- median(completeTitanic$Age[completeTitanic$Title == "Miss"],na.rm = TRUE)
completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == 'Others')] <- median(completeTitanic$Age[completeTitanic$Title == "Others"],na.rm = TRUE)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
completeTitanic$Title[completeTitanic$Age < 18] <- 'Children'
completeTitanic$Title <- factor(completeTitanic$Title)
levels(completeTitanic$Title)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Convert the datatypes to required format

completeTitanic$Survived = factor(completeTitanic$Survived)
completeTitanic$Pclass = factor(completeTitanic$Pclass)
completeTitanic$Name = as.character(completeTitanic$Name)
completeTitanic$Title = factor(completeTitanic$Title)

#Check the summary of full data once again
summary(completeTitanic)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Now remove the redundant columns which we will not be used in the prediction algorithm
columnToDelete <- c('Cabin', 'PassengerId', 'Name','Ticket')
completeTitanic[,columnToDelete] <- NULL


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Seperate Test and Train data from the combined data
trainTitanicData <- head(completeTitanic, n = nrow(trainTitanic))
testTitanicData <- tail(completeTitanic, n = nrow(testTitanic))


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Model building and prediction part

#Using Decision Tree and K cross validation

#Use K cross validation to find the 'cp' parameter
set.seed(1)
numfold = trainControl(method = "cv", number = 10)    
cpgrid = expand.grid(.cp=seq(0.01,0.7,0.01)) 
cpVal = train(Survived~., data = trainTitanicData, method='rpart', trControl=numfold, tuneGrid=cpgrid)
cpVal

#Based on cpVal, replace cp in the decision tree model
dec_tree <- rpart(Survived~., data = trainTitanicData, cp=0.02, method = 'class')
prp(dec_tree)
pv2 <- predict(dec_tree,type = 'class')
table(trainTitanicData$Survived, pv2)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Predict and check for the train value

predictTrain <- predict(dec_tree, type='class')
actualTrain <- trainTitanicData$Survived
table(predictTrain,actualTrain)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Build a Confusion Matrix and check the different parameters
confusionMatrix(predictTrain, actualTrain)

#Check the precsion of the preicted data
precision(predictTrain, actualTrain)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Predict on Test data
predictTest <- predict(dec_tree, newdata = testTitanicData, type='class')
table(predictTest)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
newdf_ <- data.frame(PassengerId =testTitanic$PassengerId, Survived = 0)
newdf_$Survived <- predictTest

#Write the file an output file
write.csv(newdf_, 'output_file.csv', row.names = FALSE)

