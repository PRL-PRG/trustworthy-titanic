library(tidyverse)
library(caret)
library(titanic)


dummyvar= titanic_train %>% select(Survived,Sex,Pclass,Embarked) %>% sapply(as.factor) %>% as.data.frame()


hotencode=dummyVars(~.,data=dummyvar) 
d=predict(hotencode,dummyvar) 

preimpute=titanic_train %>% select(Age,SibSp,Parch,Fare) %>% cbind(d) %>% 
  mutate(familysize=SibSp+Parch+1)

preproc=preProcess(preimpute,method = "bagImpute")
imputedata=predict(preproc,preimpute)
titanic_train$Age=imputedata[,1]
titanic_train$familysize=preimpute$familysize
traindata=titanic_train %>% select(-1,-4,-7,-8,-9,-11)
traindata$Survived=as.factor(traindata$Survived)
traindata$Pclass=as.factor(traindata$Pclass)
traindata$Sex=as.factor(traindata$Sex)
traindata$Embarked=as.factor(traindata$Embarked)
ctrl=trainControl(method="repeatedcv",number=5,repeats = 3,savePredictions=T)

fit.lda <- train(Survived~., data=traindata, method="lda", trControl=ctrl)

titanic_test$familysize=titanic_test$SibSp+titanic_test$Parch+1
titanic_test= titanic_test%>% select(2,4,5,9,11,12)
a=titanic_test

hotencode2=dummyVars(~.,data=a) 
d2=predict(hotencode2,a) 

preproc2=preProcess(d2,method = "bagImpute")
imputedata2=predict(preproc2,d2)
titanic_test$Age=imputedata2[,6]
b=titanic_test
b$Pclass=as.factor(b$Pclass)
b$Sex=as.factor(b$Sex)
b$Embarked=as.factor(b$Embarked)
b$Fare=imputedata2[,7]
b$Survived=predict(fit.lda,b)




