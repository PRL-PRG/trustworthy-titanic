
data_train=read.csv("../input/train.csv")
data_test=read.csv("../input/test.csv")


str(data_train)

data_train

data_test

data_train=data_train[,c(-4,-9,-11)]


data_train

data_train$Sex=ifelse(data_train$Sex=="male",1,0)


data_train$Embarked=as.numeric(data_train$Embarked)

m=0
a=0
k=0
for(i in 1:891)
    {
    if(is.na(data_train$Age[i])){
        k=k+1
        }
    else{
        a=a+data_train$Age[i]
        k=k+1
        }
    }
m=a/k
for(i in 1:891)
    {
    if(is.na(data_train$Age[i])){
        data_train$Age[i]=m
    }
  
}
data_train$Age=round(data_train$Age,0)
    

data_train$Fare=scale(data_train$Fare)
data_train$Age=scale(data_train$Age)

data_train

library(MASS)
classifier.lda=lda(Survived~.,data=data_train)
summary(classifier.lda)



data_test=data_test[,c(-3,-8,-10)]



data_test$Embarked=as.numeric(data_test$Embarked)
data_test$Sex=ifelse(data_test$Sex=="male",1,0)
data_test$Age=scale(data_test$Age)
data_test$Fare=scale(data_test$Fare)

data_test

m=0
a=0
k=0
for(i in 1:418)
    {
    if(is.na(data_test$Age[i])){
        k=k+1
        }
    else{
        a=a+data_test$Age[i]
        k=k+1
        }
    }
m=a/k
for(i in 1:418)
    {
    if(is.na(data_test$Age[i])){
        data_test$Age[i]=m
    }
  
}
data_test$Age=round(data_test$Age,0)
    

prediction.lda=predict(classifier.lda,newdata = data_test,type="prob")

prediction.lda$class

result=data.frame(data_test$PassengerId,prediction.lda$class)

result <- setNames(result, c("PassengerId","Survived"))

for(i in 1:418)
    {
    if(is.na(result$Survived[i]))
        {
        result$Survived[i]=0
    }
}

summary(result)

#setwd("../output")


#dir.create("/kaggle/output")

getwd()

write.csv(result, file = "gender_submission_quentin_alais.csv",row.names=FALSE)



str(result)




