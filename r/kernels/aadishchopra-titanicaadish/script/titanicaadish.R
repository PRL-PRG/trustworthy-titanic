library(readr)
# reading the data
#trainOr <- read_csv("C:/Users/Aadish/Downloads/trainOr.csv")
trainOr <- read.csv("../input/train.csv")
testOr  <- read.csv("../input/test.csv")

#View(trainOr)

# divide the training data into testing and training
set.seed(3)


trainOr1<-trainOr
ind<-sample(2,nrow(trainOr),replace=T,prob=c(0.7,0.3))
trainOr1<-trainOr[ind==1,]
testOr1<-trainOr[ind==2,]
# removing columns which are not required in analysis

trainOr1<-trainOr1[,c(2,3,5,6,7,8,12)]
#View(trainOr1)

testOr1<-testOr1[,c(2,3,5,6,7,8,12)]

str(trainOr1)
#using unclass to convert characters into numeric
trainOr1<-as.data.frame(unclass(trainOr1))
testOr1<-as.data.frame(unclass(testOr1))

#check the attribute type by using str again

str(trainOr1)

# converting integer into facto (Why ?   because sibsp parch  p class survived)
# i think sibsp and parch should not be disturbed.The reason is
# there can be any number of family members.Why to convert into categorical 
cols<-colnames(trainOr1[,c(1,2)])
trainOr1[,cols] <- data.frame(apply(trainOr1[cols], 2, as.factor))


cols<-colnames(testOr1[,c(1,2)])
testOr1[,cols] <- data.frame(apply(testOr1[cols], 2, as.factor))


colSums(is.na(trainOr1))
# using multiple imputation chained equation method to substitute missing values

library(mice)
tempData <- mice(trainOr1,m=5,maxit=50,meth='pmm',seed=500)
trainOr1<-complete(tempData,1)
c("#1E90FF", "#FFFFFF", "#FFFFFF")

tempDatatest <- mice(testOr1,m=5,maxit=50,meth='pmm',seed=500)
testOr1<-complete(tempDatatest,1)
#View(testOr1)


#running logistic regression to build a model
train.model<-glm(Survived~Sex+Pclass+Age+SibSp+Parch+Embarked,family ="binomial",data=trainOr1)
library(ROCR)


# predicting on the test data to evaluate our build model
predtest<-predict(train.model,newdata=testOr1,type = 'response' )
head(predtest)


predtest1<-predict(train.model,newdata=testOr1,type = 'terms' )
head(predtest1)

tab<-table(testOr1$Survived)

 
pred<-prediction(predtest,testOr1$Survived)
# pred object has many attributes
pred


table(slot(pred,'labels'))

eval<-performance(pred,'acc')
eval
plot(eval)


eval1<-performance(pred,'tpr','fpr')
plot(eval1,
     colorize=T)



max<-which.max(slot(eval,"y.values")[[1]])
max
acc<-slot(eval,"y.values")[[1]][max]
acc
cut<-slot(eval,"x.values")[[1]][max]
cut
print(c(accuracy=acc,cutoff=cut))
abline(a=0,b=1)

#Area under the curve

auc<-performance(pred,"auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc,digits=2)
legend(0.8,0.4,auc,title="AUC",cex=1,merge = T)




# importing test data
library(readr)
#testOr<- read_csv("C:/Users/Aadish/Downloads/test.csv")
#View(testOr)
#Clean the data on which testing has to be done

testOr<-testOr[,c(2,4,5,6,7,11)]

testOr<-as.data.frame(unclass(testOr))
str(testOr)
testOr[,1]
testOr<-as.data.frame(unclass(testOr))
cols<-colnames(testOr)[1]
cols
testOr[cols] <- data.frame(apply(testOr[cols],2,FUN = as.factor))

survivaltest<-predict(train.model,newdata=testOr,type = 'response' )


