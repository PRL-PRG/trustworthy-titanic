## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret) 

train<-read.csv("../input/train.csv", 
                na.strings=c("NA", "#DIV/0!"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
training1<-subset(train, select = -c(PassengerId, Name, Ticket, Cabin))
missingcol<-data.frame(colMeans(is.na(training1)))
mean(training1$Age, na.rm=TRUE)
mean(training1$Fare,na.rm=TRUE)
training1$Age[is.na(training1$Age)] <-29.69 



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
training1$Sex<-as.factor(training1$Sex)
training1$Embarked<-as.factor(training1$Embarked)
training1$Survived<-as.factor(training1$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(training1$Pclass, training1$Fare)
hist(training1$Age)
hist(training1$SibSp)
hist(training1$Parch)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
inBuild<-createDataPartition(y=training1$Survived, p=0.8, list=FALSE)
trainData<-training1[inBuild,]
validateData<-training1[-inBuild,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1120345)
modelfit_rpart<-train(Survived~., data=trainData, method="rpart")
predict_rpart<-predict(modelfit_rpart, newdata=validateData)
result_rpart<-confusionMatrix(predict_rpart, validateData$Survived)
result_rpart


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
modelfit_rf<-train(Survived~., data=trainData, method="rf", prox=TRUE, 
                    trControl=trainControl(method = "cv", number=5))

predict_rf<-predict(modelfit_rf, newdata=validateData)
result_rf<-confusionMatrix(predict_rf, validateData$Survived)
result_rf


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
modelfit_gbm<-train(Survived~., data=trainData, method="gbm", verbose=FALSE, trControl=trainControl(method="cv", number=5))
predict_gbm<-predict(modelfit_gbm, newdata=validateData)
result_gbm<-confusionMatrix(predict_gbm, validateData$Survived)
result_gbm

