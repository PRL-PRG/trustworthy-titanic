# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
# Any results you write to the current directory are saved as output.

titanic<-read.csv("../input/train.csv")
titanic_test<-read.csv("../input/test.csv")
summary(titanic)
colSums(is.na(titanic))
titanic$Age[is.na(titanic$Age)]<-mean(titanic$Age,na.rm = TRUE)
titanic_test$Age[is.na(titanic_test$Age)]<-mean(titanic_test$Age,na.rm = TRUE)
sum(titanic$Fare=="0")
titanic$Fare<-ifelse(titanic$Fare=='0',mean(titanic$Fare,na.rm=TRUE),titanic$Fare)

sum(titanic$Embarked=='')
levels(titanic$Embarked)[levels(titanic$Embarked) == ""] <- "S"

titanic$Survived<-as.factor(titanic$Survived)




library(ggplot2)
ggplot(titanic,aes(x=Age,fill=Survived))+geom_bar(position = "dodge")+facet_grid(".~Survived")
ggplot(titanic,aes(x=Age,fill=Sex,col=Sex))+geom_bar()+facet_grid(".~Survived")
ggplot(titanic,aes(x=Sex,fill=Survived))+geom_bar(position = "dodge")
ggplot(titanic,aes(fill=Sex,x=Pclass))+geom_bar(position = "dodge")+facet_grid(".~Survived")
ggplot(titanic,aes(x=Pclass,fill=Survived))+geom_bar(position = "dodge")+facet_grid(".~Sex")
ggplot(titanic,aes(y=Pclass,x=Age,col=titanic$Sex))+geom_jitter(shape=16,size=2)+facet_grid(".~Survived")

ggplot(titanic,aes(x=Embarked,fill=Survived))+geom_bar(position = "dodge")+facet_grid(".~Sex")
ggplot(titanic,aes(x=Embarked,fill=Survived))+geom_bar(position = "dodge")+facet_grid(".~Pclass")
ggplot(titanic,aes(y=Embarked,x=Age,col=Sex))+geom_jitter()+facet_grid(".~Survived")       
ggplot(titanic,aes(y=Embarked,x=Age,col=Sex))+geom_jitter(size=2)+facet_grid(".~Pclass")  


#--------------------------------------------Random Forest----------------------------------------------------------
library(randomForest)
train_result<-randomForest(Survived~Age+Pclass+Fare+Sex+Embarked,titanic,ntree=100,importance=TRUE)
train_result
varImpPlot(train_result)


train_result2<-randomForest(Survived~Age+Pclass+Fare+Sex,titanic,ntree=100,importance=TRUE)
train_result2
varImpPlot(train_result2)


train_result3<-randomForest(Survived~.-Cabin-Ticket-Name,titanic,ntree=100,importance=TRUE)
train_result3
varImpPlot(train_result3)


PassengerId<-titanic_test$PassengerId
Survived<-predict(train_result2,titanic_test)
Survived
prediction<-cbind(PassengerId,Survived)
write.csv(prediction,"survivallist.csv")
