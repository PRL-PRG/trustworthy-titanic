## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readr)
library(mice)
library(ROCR)


## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainOr <- read.csv("../input/train.csv")


## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(3)
trainOr1<-trainOr
ind<-sample(2,nrow(trainOr),replace=T,prob=c(0.7,0.3))
trainOr1<-trainOr[ind==1,]
testOr1<-trainOr[ind==2,]


## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainOr1<-trainOr1[,c(2,3,5,6,7,8,12)]
testOr1<-testOr1[,c(2,3,5,6,7,8,12)]


## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainOr1<-as.data.frame(unclass(trainOr1))
testOr1<-as.data.frame(unclass(testOr1))



## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cols<-colnames(trainOr1[,c(1,2)])
trainOr1[,cols] <- data.frame(apply(trainOr1[cols], 2, as.factor))

cols<-colnames(testOr1[,c(1,2)])
testOr1[,cols] <- data.frame(apply(testOr1[cols], 2, as.factor))



## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
colSums(is.na(trainOr1))
tempData <- mice(trainOr1,m=5,maxit=50,meth='pmm',seed=500)
trainOr1<-complete(tempData,1)
tempDatatest <- mice(testOr1,m=5,maxit=50,meth='pmm',seed=500)
testOr1<-complete(tempDatatest,1)


## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train.model<-glm(Survived~Sex+Pclass+Age+SibSp+Parch+Embarked,family ="binomial",data=trainOr1)


## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predtest<-predict(train.model,newdata=testOr1,type = 'response' )
pred<-prediction(predtest,testOr1$Survived)


## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
eval<-performance(pred,'acc')
plot(eval)


## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
eval1<-performance(pred,'tpr','fpr')
plot(eval1,colorize=T)
auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,digits=2)
legend(0.8,0.4,auc,title="AUC",cex=1,merge = T)


## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
cut<-slot(eval,"x.values")[[1]][max]
print(c(accuracy=acc,cutoff=cut))



## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testOr  <- read.csv("../input/test.csv")
testOr<-testOr[,c(2,4,5,6,7,11)]
testOr<-as.data.frame(unclass(testOr))
cols<-colnames(testOr)[1]
testOr[cols] <- data.frame(apply(testOr[cols],2,FUN = as.factor))
tempData <- mice(testOr,m=5,maxit=50,meth='pmm',seed=500)
testOr<-complete(tempData,1)
survivaltest<-predict(train.model,newdata=testOr,type = 'response' )
survivaltest<-as.data.frame(survivaltest)



## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
survivalpred=rep(0,418)
survivalpred[survivaltest>0.64]=1
survivalpred<-as.data.frame(survivalpred)


