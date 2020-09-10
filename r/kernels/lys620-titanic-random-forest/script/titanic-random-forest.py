#!/usr/bin/env python
# coding: utf-8

# In[1]:


#loading library
library(caret)
library(dplyr)
library(ggplot2)
library(rpart.plot)
library(randomForest)
library(DataExplorer)


# In[2]:


#read data
training<-read.csv("../input/train.csv")
testing<-read.csv("../input/test.csv")

str(training)
summary(training)
colSums(is.na(training))
head(training)


# In[3]:


#eda
ggplot(data=training,aes(x=Pclass,fill=Sex))+geom_bar()
ggplot(data=training,aes(x=Survived,fill=Sex))+geom_bar()
ggplot(data=training,aes(x=Survived,fill=Sex))+geom_bar()
training %>% group_by(Pclass) %>% summarize(meanAge=mean(Age,na.rm=T))


# In[4]:


#Fill age
training$Age<-ifelse(is.na(training$Age)==TRUE & training$Pclass==1,38.2,
                      ifelse(is.na(training$Age)==TRUE & training$Pclass==2,29.9,
                             ifelse(is.na(training$Age)==TRUE & training$Pclass==3,25.1,training$Age)))

names(training)
trdata<-training %>% select(Survived,Pclass,Sex,Age,SibSp,Parch,Embarked)
trdata$Survived<-as.factor(trdata$Survived)


# In[7]:


fitControl<-trainControl(method="repeatedcv",number = 10,repeats = 5,search = "random")
rf_fit<-train(Survived~.,data=trdata,method="rf",trControl=fitControl,verbose=F)


# In[8]:


rf_fit
pred_model<-predict(rf_fit,trdata)
confusionMatrix(pred_model,trdata$Survived)


# In[9]:


#Fill age testing data
testing$Age<-ifelse(is.na(testing$Age)==TRUE & testing$Pclass==1,38.2,
                     ifelse(is.na(testing$Age)==TRUE & testing$Pclass==2,29.9,
                            ifelse(is.na(testing$Age)==TRUE & testing$Pclass==3,25.1,testing$Age)))


# In[10]:


#prediction
pred<-predict(rf_fit,testing)
length(pred)
testing$Survived<-pred


# In[11]:


#write csv
write.csv(testing,file="result.csv")

