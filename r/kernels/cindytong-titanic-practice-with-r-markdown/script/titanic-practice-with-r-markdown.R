## ---- message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2) 
library(dplyr) 

train <- read.csv('../input/train.csv')
test <- read.csv('../input/test.csv')
# test doesn't have outcome - Survived

str(train)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
length(train$PassengerId)
table(train$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(train$Pclass, train$Survived)
ggplot(train,aes(x=Pclass,fill=factor(Survived)))+
  geom_histogram(stat="count")

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(train$Sex, train$Survived)
ggplot(train,aes(x=Sex,fill=factor(Survived)))+
  geom_histogram(stat="count")

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train,aes(x=Age,fill=factor(Survived)))+
  geom_histogram(binwidth=2)
  
train$kid <- ifelse (train$Age<=10, "Yes", "No")
table(train$kid, train$Survived)

train$old <- ifelse (train$Age>62, "Yes", "No")
table(train$old, train$Survived)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train,aes(x=Fare,fill=factor(Survived)))+
  geom_histogram(binwidth=10)
  
library(dplyr)
train %>% 
group_by(Pclass) %>% 
summarize(n=length(Fare), mean=mean(Fare), min=min(Fare), max=max(Fare))

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train,aes(x=SibSp,fill=factor(Survived)))+
  geom_histogram(binwidth=1)
  
train$moreSibSp <- ifelse (train$SibSp>2.5, "Yes", "No")
table(train$moreSibSp, train$Survived)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train,aes(x=Parch,fill=factor(Survived)))+
  geom_histogram(binwidth=1)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train,aes(x=Embarked,fill=factor(Survived)))+
  geom_histogram(stat="count")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$single <- ifelse(train$SibSp==0 & train$Parch==0, "Yes", "No")
table(train$single, train$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(is.na(train$Age)) 
table(is.na(train$Age), train$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(Hmisc)
train$imputed_age <- with(train, impute(train$Age, mean))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caTools)
set.seed(123)
split <- sample.split(train$Survived, SplitRatio = 0.7)
train2 <- subset(train, split==TRUE)
test2  <- subset(train, split==FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logistic <- glm(train2$Survived ~ imputed_age + factor(Sex) + factor(Pclass) + kid + single + moreSibSp + factor(Pclass)*factor(Sex), 
family="binomial", data=train2)
summary(logistic)

pred <- predict(logistic, type='response', newdata=test2)

y_pred <- ifelse(pred>0.5, 1, 0)

library(caret)
confusionMatrix(as.factor(test2$Survived),as.factor(y_pred))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data_fac <- train2 %>% mutate_if(is.character, as.factor) # convert all character variables to factor
data_fac <- na.omit(data_fac)

library(randomForest)
RF <- randomForest(Survived ~ imputed_age + Sex + Pclass + kid + single + moreSibSp, data = data_fac, ntree=50)
varImpPlot(RF)
plot(RF)

test2 <- test2 %>% mutate_if(is.character, as.factor) # convert all character variables to factor
test2 <- na.omit(test2)
pred <- predict(RF, newdata=test2)

y_pred <- ifelse(pred>0.5, 1, 0)

library(caret)
confusionMatrix(as.factor(test2$Survived),as.factor(y_pred))

