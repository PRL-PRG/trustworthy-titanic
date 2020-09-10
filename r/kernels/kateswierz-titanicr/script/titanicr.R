
# This R script will run on our backend. You can write arbitrary code here!


# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)
attach(train)

train$Name <- NULL
train$Ticket <- NULL
train$Cabin <- NULL
train$PassengerId <- as.factor(train$PassengerId)
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
summary(train$Age)

miss_age <- train[is.na(Age),]

#predict age
age <- lm(Age~train$Pclass+train$Survived+train$SibSp)
summary(age)
pred <- predict(age,train)
summary(pred)

preds <- cbind(train[,"Age"],pred)
p <- cbind(miss_age,preds[is.na(Age),])
p$Age <- NULL
p$V1 <- NULL

library(plyr)
p <- rename(p,c("pred"="Age"))
a <- subset(train,!is.na(Age))
train <- rbind(p,a)

library(caret)
featureCols <- c("Survived","Sex","Pclass","Age")
datafiltered <- train[,featureCols]
inTrainRows <- createDataPartition(datafiltered$Survived, p=0.7,list=FALSE)
trainDataFiltered <- datafiltered[inTrainRows,]
testDataFiltered <- datafiltered[-inTrainRows,]
nrow(trainDataFiltered)/(nrow(testDataFiltered)+nrow(trainDataFiltered))
logisticRegModel <- train(Survived~.,data=datafiltered,method="glm",family="binomial")
logisticRegModel
summary(logisticRegModel)

logRegPrediction <- predict(logisticRegModel,testDataFiltered)
logRegConfMat <- confusionMatrix(logRegPrediction,testDataFiltered[,"Survived"])
logRegConfMat

library(randomForest)
rfmodel <- randomForest(trainDataFiltered[-1],trainDataFiltered$Survived,proximity=TRUE,importance=TRUE)
rfValidation <- predict(rfmodel,testDataFiltered)
rfConfMatrix <- confusionMatrix(rfValidation,testDataFiltered[,"Survived"])
rfConfMatrix