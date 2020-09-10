## ----global_options, include=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(fig.width=5, fig.height=4)


## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls()) # Clear the variables

library(tree); library(ggplot2);library(reshape2); library(randomForest); library(e1071); library(caret) # Load the relevant libraries
library(tidyverse)


## ----data_load-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train<-read.csv("../input/train.csv", header=T)
test<-read.csv("../input/test.csv", header=T)


## ----fix_vars, echo=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let's dig in further into the data.
summary(train)


## ---- echo=F, warning=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Compute a histogram of `PClass`
p<-ggplot(data=train, aes(train$Pclass, fill=as.factor(train$Survived))) + 
  geom_histogram(binwidth=0.5)+
  labs(title="Histogram of Pclass")+
labs(x="Passenger Class")

p<-p+scale_fill_discrete(name = "Survived")
p


## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Pclass<-as.factor(train$Pclass)
test$Pclass<-as.factor(test$Pclass)


## ---- echo=F, warning=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p<-ggplot(data=train, aes(train$Age, fill=as.factor(train$Survived))) + 
  geom_histogram(binwidth=15)+
  labs(title="Histogram of Age")+
labs(x="Age")

p<-p+scale_fill_discrete(name = "Survived")
p


## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
m<-mean(train$Age,na.rm=T)
train$Age[is.na(train$Age)]<-m
test$Age[is.na(test$Age)]<-m

cat_age<-function(age){
  z<-character(length(age))
  for(i in 1:length(age)){
  if(age[i]<18){z[i]<-"under18"}
  else if(age[i]<23){z[i]<-"18_23"}
  else if(age[i]<28){z[i]<-"23_28"}
    else if(age[i]<34){z[i]<-"28_34"}
    else if(age[i]<44){z[i]<-"34_44"}
  else if(age[i]<81){z[i]<-"41_81"}}
  return(z)}

AgeCat<-cat_age(train$Age)
train$AgeCat<-as.factor(AgeCat)
# train$AgeCat

AgeCat<-cat_age(test$Age)
test$AgeCat<-as.factor(AgeCat)
# test$AgeCat

#### Plotting
p<-ggplot(data=train, aes(train$AgeCat, fill=as.factor(train$Survived))) + 
  geom_bar()+
  labs(title="Histogram of Categorical Age")+
labs(x="Age")

p<-p+scale_fill_discrete(name = "Survived")
p



## ---- echo=F, warning=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p<-ggplot(data=train, aes(train$Sex, fill=as.factor(train$Survived))) + 
  geom_bar()+
  labs(title="Bar Chart of Sex")+
labs(x="Sex")

p<-p+scale_fill_discrete(name = "Survived")
p


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Sex<-as.factor(train$Sex)
test$Sex<-as.factor(test$Sex)


## ---- echo=F, warning=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p<-ggplot(data=train, aes(train$Fare, fill=as.factor(train$Survived))) + 
  geom_histogram(binwidth=50)+
  labs(title="Histogram of Fare")+
labs(x="Fare")

p<-p+scale_fill_discrete(name = "Survived")
p


## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test$Fare[is.na(test$Fare)]<-mean(train$Fare)


## ---- echo=F, warning=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p<-ggplot(data=train, aes(train$SibSp, fill=as.factor(train$Survived))) + 
  geom_histogram(binwidth=0.5)+
  labs(title="Histogram of Sibling and Spouse")+
labs(x="SibSp")

p<-p+scale_fill_discrete(name = "Survived")
p


## ---- echo=F, warning=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p<-ggplot(data=train, aes(train$Parch, fill=as.factor(train$Survived))) + 
  geom_histogram(binwidth=0.5)+
  labs(title="Histogram of Parent and Child")+
labs(x="Parch")

p<-p+scale_fill_discrete(name = "Survived")
p


## ---- echo=F, warning=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
FamSize<-train$Parch+train$SibSp+1
train$FamSize<-FamSize

FamSize1<-test$Parch+test$SibSp+1
test$FamSize<-FamSize1

p<-ggplot(data=train, aes(train$FamSize, fill=as.factor(train$Survived))) + 
  geom_histogram(binwidth=0.5)+
  labs(title="Histogram of Family Size")+
labs(x="FamSize")

p<-p+scale_fill_discrete(name = "Survived")
p


## ----echo=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
catfam<-function(x){
  z<-character(length(x))
  for(i in 1:length(x)){
  if(x[i]==1){z[i]<-"alone"}
  else if(x[i]<5){z[i]<-"small"}
  else if (x[i]>=5){z[i]<-"large"}}
  return(z)
}

FamSizeCat<-catfam(train$FamSize)
train$FamSizeCat<-as.factor(FamSizeCat)

FamSizeCat<-catfam(test$FamSize)
test$FamSizeCat<-as.factor(FamSizeCat)

p<-ggplot(data=train, aes(train$FamSizeCat, fill=as.factor(train$Survived))) + 
  geom_bar()+
  labs(title="Bar Chart of Categorical Family Size")+
labs(x="Categorical Family Size")

p<-p+scale_fill_discrete(name = "Survived")
p


## ---- echo=F, warning=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Embarked[train$Embarked==""]<-"S"
test$Embarked[test$Embarked==""]<-"S"

train$Embarked<-as.factor(train$Embarked)
test$Embarked<-as.factor(test$Embarked)

p<-ggplot(data=train, aes(train$Embarked, fill=as.factor(train$Survived))) + 
  geom_bar()+
  labs(title="Bar Chart of Port of Embarkation")+
labs(x="Embarked")

p<-p+scale_fill_discrete(name = "Survived")
p


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1011)
n_folds<-10 # We want 10 folds
indx<-sample(1:nrow(train),replace=F) # Randomly shuffle the rows in the dataframe
fold<-(indx%%n_folds)+1 # Calculate the modulo to base 10 and add 1. This will return a number between 1 and 10. This number will be its fold.

train$fold<-fold


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
err_cv<-numeric(n_folds)
err_train<-numeric(n_folds)
for(i in 1:n_folds){
fit<-glm(Survived~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train[train$fold!=i,],family="binomial")

fitted<-predict.glm(fit, type="response")
fitted1<-ifelse(fitted>0.5,1,0)
t1<-sum(diag(table(train$Survived[train$fold!=i],fitted1)))/sum(table(train$Survived[train$fold!=i],fitted1))

fitted.values2<-predict.glm(fit,newdata = train[train$fold==i,],type="response")
fitted.values3<-ifelse(fitted.values2>0.5,1,0)
t2<-sum(diag(table(train$Survived[train$fold==i],fitted.values3)))/sum(table(train$Survived[train$fold==i],fitted.values3))
err_train[i]<-t1
err_cv[i]<-t2
}


## ----echo=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
err_cv<-numeric(n_folds)
err_train<-numeric(n_folds)
for(i in 1:n_folds){
fit<-glm(Survived~Pclass+Sex+AgeCat+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train[train$fold!=i,],family="binomial")

fitted<-predict.glm(fit, type="response")
fitted1<-ifelse(fitted>0.5,1,0)
t1<-sum(diag(table(train$Survived[train$fold!=i],fitted1)))/sum(table(train$Survived[train$fold!=i],fitted1))


fitted.values2<-predict.glm(fit,newdata = train[train$fold==i,],type="response")
fitted.values3<-ifelse(fitted.values2>0.5,1,0)
t2<-sum(diag(table(train$Survived[train$fold==i],fitted.values3)))/sum(table(train$Survived[train$fold==i],fitted.values3))
err_train[i]<-t1
err_cv[i]<-t2}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
err_cv<-numeric(n_folds)
err_train<-numeric(n_folds)
for(i in 1:n_folds){
fit<-tree(Survived~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train[train$fold!=i,])

fitted<-predict(fit, train[train$fold!=i,])
fitted1<-ifelse(fitted>0.5,1,0)
t1<-sum(diag(table(train$Survived[train$fold!=i],fitted1)))/sum(table(train$Survived[train$fold!=i],fitted1))


fitted.values2<-predict(fit, train[train$fold==i,])
fitted.values3<-ifelse(fitted.values2>0.5,1,0)
t2<-sum(diag(table(train$Survived[train$fold==i],fitted.values3)))/sum(table(train$Survived[train$fold==i],fitted.values3))
err_train[i]<-t1
err_cv[i]<-t2
}


## ----echo=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
err_cv<-numeric(n_folds)
err_train<-numeric(n_folds)
for(i in 1:n_folds){
fit<-tree(Survived~Pclass+Sex+AgeCat+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train[train$fold!=i,])

fitted<-predict(fit, train[train$fold!=i,])
fitted1<-ifelse(fitted>0.5,1,0)
t1<-sum(diag(table(train$Survived[train$fold!=i],fitted1)))/sum(table(train$Survived[train$fold!=i],fitted1))


fitted.values2<-predict(fit, train[train$fold==i,])
fitted.values3<-ifelse(fitted.values2>0.5,1,0)
t2<-sum(diag(table(train$Survived[train$fold==i],fitted.values3)))/sum(table(train$Survived[train$fold==i],fitted.values3))
err_train[i]<-t1
err_cv[i]<-t2
}
# print(paste("Training Accuracy is",as.character(round(mean(err_train),3))))
# print(paste("Classification Accuracy is",as.character(round(mean(err_cv),3))))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit<-randomForest(as.factor(Survived)~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train)

plot(fit,main="Error rates with numerical Age")
legend('top', colnames(fit$err.rate), col=1:3,fill=1:3,horiz=T)


## ----echo=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit<-randomForest(as.factor(Survived)~Pclass+Sex+AgeCat+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train)

plot(fit,main="Error rates with categorical Age")
legend('top', colnames(fit$err.rate), col=1:3,fill=1:3,horiz=T)


## ----echo=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
err_cv<-numeric(n_folds)
err_train<-numeric(n_folds)
for(i in 1:n_folds){
fit<-svm(as.factor(Survived)~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train[train$fold!=i,])

fitted1<-predict(fit, train[train$fold!=i,])
# fitted1<-ifelse(fitted>0.5,1,0)
t1<-sum(diag(table(train$Survived[train$fold!=i],fitted1)))/sum(table(train$Survived[train$fold!=i],fitted1))


fitted.values3<-predict(fit, train[train$fold==i,])
# fitted.values3<-ifelse(fitted.values2>0.5,1,0)
t2<-sum(diag(table(train$Survived[train$fold==i],fitted.values3)))/sum(table(train$Survived[train$fold==i],fitted.values3))
err_train[i]<-t1
err_cv[i]<-t2
}



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tunedModel <- tune.svm(Survived~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,  data = train,gamma = seq(0.04,2,0.04), cost = seq(2,6,2),kernel = "radial") 
 
gamma1<-tunedModel$best.parameters[1]
  cost1<-tunedModel$best.parameters[2]


## ---- include=F------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
y1<-round(tunedModel$best.performance,3)
tunedModel <- tune.svm(Survived~Pclass+Sex+AgeCat+FamSizeCat+Embarked+Fare+SibSp+Parch,  data = train,gamma = seq(0.04,0.2,0.04), cost = seq(2,6,2),kernel = "radial") 
 
gamma2<-tunedModel$best.parameters[1]
  cost2<-tunedModel$best.parameters[2]
y2<-round(tunedModel$best.performance,3)


## ----echo=F----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)

err_cv<-numeric(n_folds)
err_train<-numeric(n_folds)

for(i in 1:n_folds){
gbmFit1 <- train(as.factor(Survived)~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,  data = train[train$fold!=i,], method = "gbm", trControl = fitControl,verbose = FALSE)
gbm_dev <- predict(gbmFit1, train[train$fold!=i,],type= "prob")[,2]
gbm_dev1<-ifelse(gbm_dev<0.5,0,1)
t1<-sum(diag(table(train$Survived[train$fold!=i],gbm_dev1)))/sum(table(train$Survived[train$fold!=i],gbm_dev1))

gbm_dev <- predict(gbmFit1, train[train$fold==i,],type= "prob")[,2]
gbm_dev1<-ifelse(gbm_dev<0.5,0,1)
s1<-sum(diag(table(train$Survived[train$fold==i],gbm_dev1)))/sum(table(train$Survived[train$fold==i],gbm_dev1))

err_cv[i]<-s1;err_train[i]<-t1}


## ---- echo=F---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fitControl <- trainControl(method = "repeatedcv", number = 4, repeats = 4)

err_cv<-numeric(n_folds)
err_train<-numeric(n_folds)

for(i in 1:n_folds){
gbmFit1 <- train(as.factor(Survived)~Pclass+Sex+AgeCat+FamSizeCat+Embarked+Fare+SibSp+Parch,  data = train[train$fold!=i,], method = "gbm", trControl = fitControl,verbose = FALSE)
gbm_dev <- predict(gbmFit1, train[train$fold!=i,],type= "prob")[,2]
gbm_dev1<-ifelse(gbm_dev<0.5,0,1)
t1<-sum(diag(table(train$Survived[train$fold!=i],gbm_dev1)))/sum(table(train$Survived[train$fold!=i],gbm_dev1))

gbm_dev <- predict(gbmFit1, train[train$fold==i,],type= "prob")[,2]
gbm_dev1<-ifelse(gbm_dev<0.5,0,1)
s1<-sum(diag(table(train$Survived[train$fold==i],gbm_dev1)))/sum(table(train$Survived[train$fold==i],gbm_dev1))

err_cv[i]<-s1;err_train[i]<-t1}


## ---- echo=F, eval=F-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## ### 1. Logistic Regression
## # Logistic Regression (Numerical Age)
## fit1a<-glm(Survived~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train[train$fold!=i,],family="binomial")
## fitted1a<-predict.glm(fit1a,newdata=test, type="response")
## fitted1a<-ifelse(fitted1a>0.5,1,0)
## subs<-data.frame(cbind(test$PassengerId,fitted1a))
## colnames(subs)<-c("PassengerId","Survived")
## write.csv(subs,"LogReg_1a.csv",row.names=F)
## 
## # Logistic Regression (Categorical Age)
## fit1b<-glm(Survived~Pclass+Sex+AgeCat+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train[train$fold!=i,],family="binomial")
## fitted1b<-predict.glm(fit1b,newdata=test, type="response")
## 
## fitted1b<-ifelse(fitted1b>0.5,1,0)
## subs<-data.frame(cbind(test$PassengerId,fitted1b))
## colnames(subs)<-c("PassengerId","Survived")
## write.csv(subs,"LogReg_1b.csv",row.names=F)
## 
## #### 2. Decision Tree
## ### Tree (Numerical Age)
## fit2a<-tree(Survived~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train)
## fitted2a<-predict(fit2a, test)
## fitted2a<-ifelse(fitted2a>0.5,1,0)
## subs<-data.frame(cbind(test$PassengerId,fitted2a))
## colnames(subs)<-c("PassengerId","Survived")
## write.csv(subs,"Tree_2a.csv",row.names=F)
## 
## ### Tree (Categorical Age)
## fit2b<-tree(Survived~Pclass+Sex+AgeCat+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train)
## fitted2b<-predict(fit2b, test)
## fitted2b<-ifelse(fitted2b>0.5,1,0)
## subs<-data.frame(cbind(test$PassengerId,fitted2b))
## colnames(subs)<-c("PassengerId","Survived")
## write.csv(subs,"Tree_2b.csv",row.names=F)
## 
## #### 3. Random Forests
## ### Random Forest (Numerical Age)
## levels(test$Embarked) <- levels(train$Embarked)
## 
## fit3a<-randomForest(as.factor(Survived)~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train)
## fitted3a<-predict(fit3a, test)
## # fitted3a<-ifelse(fitted3a>0.5,1,0)
## subs<-data.frame(test$PassengerId,fitted3a)
## colnames(subs)<-c("PassengerId","Survived")
## write.csv(subs,"Forest_3a.csv",row.names=F)
## 
## ### Random Forest (Categorical Age)
## fit3b<-randomForest(as.factor(Survived)~Pclass+Sex+AgeCat+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train)
## fitted3b<-predict(fit3b, test)
## # fitted3b<-ifelse(fitted3b>0.5,1,0)
## subs<-data.frame(test$PassengerId,fitted3b)
## colnames(subs)<-c("PassengerId","Survived")
## write.csv(subs,"Forest_3b.csv",row.names=F)
## 
## #### 4. Support Vector Machine
## ### SVM (Numerical Age)
## 
## fit4a <- svm(as.factor(Survived)~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train,gamma=gamma1,cost=cost1,kernel = "radial",type="C-classification")
## fitted4a<-as.character(predict(fit4a, test))
## 
## subs<-data.frame(cbind(test$PassengerId,fitted4a))
## colnames(subs)<-c("PassengerId","Survived")
## write.csv(subs,"SVM_4a.csv",row.names=F)
## 
## ### SVM (Categorical Age)
## fit4b <- svm(as.factor(Survived)~Pclass+Sex+AgeCat+FamSizeCat+Embarked+Fare+SibSp+Parch,data=train,gamma=gamma2,cost=cost2,kernel = "radial",type="C-classification")
## fitted4b<-as.character(predict(fit4b,test))
## 
## subs<-data.frame(cbind(test$PassengerId,fitted4b))
## colnames(subs)<-c("PassengerId","Survived")
## write.csv(subs,"SVM_4b.csv",row.names=F)
## 
## #### 5. Gradient Boosting Method
## ### Gradient Boosting (Numerical Age)
## fit5a <- train(as.factor(Survived)~Pclass+Sex+Age+FamSizeCat+Embarked+Fare+SibSp+Parch,  data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
## fitted5a <- predict(fit5a, test,type= "prob")[,2]
## fitted5a<-ifelse(fitted5a>0.5,1,0)
## subs<-data.frame(cbind(test$PassengerId,fitted5a))
## colnames(subs)<-c("PassengerId","Survived")
## write.csv(subs,"GBM_5a.csv",row.names=F)
## 
## ### Gradient Boosting (Categorical Age)
## fit5b <- train(as.factor(Survived)~Pclass+Sex+AgeCat+FamSizeCat+Embarked+Fare+SibSp+Parch,  data = train, method = "gbm", trControl = fitControl,verbose = FALSE)
## fitted5b <- predict(fit5b, test,type= "prob")[,2]
## fitted5b<-ifelse(fitted5b>0.5,1,0)
## subs<-data.frame(cbind(test$PassengerId,fitted5b))
## colnames(subs)<-c("PassengerId","Survived")
## write.csv(subs,"GBM_5b.csv",row.names=F)

