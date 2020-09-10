
traindata<-read.csv("../input/train.csv",header = TRUE, colClasses=c("numeric", "factor", "factor","character", "factor", "numeric","numeric", "numeric", "character", "numeric","character","factor"))
testdata<-read.csv("../input/test.csv",header = TRUE, colClasses=c("numeric",  "factor","character", "factor", "numeric","numeric", "numeric", "character", "numeric","character","factor"))

summary(traindata)
summary(testdata)

# fun_Age<-function(x){
#   if(is.na(x)==TRUE) 
#     {x=28} 
#   return(x)
# }
# 
# traindata$Age<-sapply(traindata$Age,fun_Age)

fun_Fare<-function(x){
  if(is.na(x)==TRUE) 
  {x=14.454} 
  return(x)
}

testdata$Fare<-sapply(testdata$Fare,fun_Fare)

traindata$Survived<-factor(traindata$Survived,levels = c("0","1"))
traindata$Pclass<-factor(traindata$Pclass,levels = c("1","2","3"))
traindata$Sex<-factor(traindata$Sex,levels = c("female","male"))
traindata$Embarked<-factor(traindata$Embarked,levels = c("","C","Q","S"))

testdata$Pclass<-factor(testdata$Pclass,levels = c("1","2","3"))
testdata$Sex<-factor(testdata$Sex,levels = c("female","male"))
testdata$Embarked<-factor(testdata$Embarked,levels = c("","C","Q","S"))




library(gplots)
library(ROCR)
library(randomForest)
set.seed(5000)
train.rf<-randomForest(Survived~Sex+Fare+Pclass+SibSp+Parch, data = traindata,na.action=na.omit,cutoff=c(0.616,0.384))

importance(train.rf)
train.rf

tmpROC=cbind(traindata$Survived,train.rf$votes)
head(tmpROC)

pred.auc.rf.train=prediction(tmpROC[,3], tmpROC[,1])
perf.rf.train = performance(pred.auc.rf.train,'auc',"tpr","fpr")
perf.rf.train@y.values[[1]]




pred.rf=predict(train.rf,testdata,type="prob") 

my_submission=cbind(PassengerId=testdata$PassengerId,Survived=pred.rf[,2])
summary(my_submission)

for(i in 1:418){
  if(my_submission[i,2]>=0.384)my_submission[i,2]=1 else my_submission[i,2]=0
  }

write.csv(my_submission,file="my_submission_7.csv",row.names=FALSE,quote = FALSE)