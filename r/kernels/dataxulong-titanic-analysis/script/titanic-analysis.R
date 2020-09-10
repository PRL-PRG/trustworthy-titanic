---
title:"My first of the Titanic"
author:"Xu Mike"
date:"17 August 2017"
output:
  html_document:
    number_sections: true
    toc: true
    fig_width: 7
    fig_height: 4.5
    theme: readable
    highlight: tango
---

# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)  #use filter to select data
library(randomForest) #model

train01<-read.csv("../input/train.csv",header = TRUE)
test01<-read.csv("../input/test.csv",header = TRUE)
testsuv01<-read.csv("../input/gendermodel.csv",header = TRUE)

train02<-data.frame(train01) 
test02<-data.frame(cbind(test01,testsuv01))
test03<-data.frame(test02[c(1:11,13)])
titan<-data.frame(rbind(train02,test03))
length(titan$Embarked)
## summary(titan)         # caculate how many na in titan 

titan$Rname<-gsub("(.*,)|(\\..*)", "", titan$Name)        ##############################
# table(titan$Rname,titan$Sex)
rare<-c("Capt","Col","Don","Dona","Dr","Jonkheer","Major","Rev","the Countess")
titan<-within(titan,{
  Rname[Rname %in% "Lady"]<-"Mrs"
  Rname[Rname %in% "Mlle"]<-"Miss"
  Rname[Rname %in% "Mme"]<-"Mrs"
  Rname[Rname %in% "Ms"]<-"Miss"
  Rname[Rname %in% "Sir"]<-"Mr"
  Rname[Rname %in% rare]<-"Rarename"
})
# table(titan$Rname,titan$Sex)
titan$family<-titan$SibSp+titan$Parch
titan02<-filter(titan02,PassengerId!=62 & PassengerId!=830)
 #age 263 ,Fare 1 ,

titan02<-data.frame(titan)                           #titan use mean fill missing values of Age
mage<-mean(titan02$Age[!is.na(titan02$Age)])
titan02$Age[is.na(titan02$Age)]<-mage             #,  1309
length(titan02$Age)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

# attach(titan02)
ggplot(titan02,aes(Age,color=factor(Survived)))+
  geom_freqpoly(binwidth=2)+
  labs(x="age",y="survived count",title="(mean_age)different age and survived")

ggplot(titan02,aes(Embarked,Age))+
  labs(x='embarked',y='age',title='(mean_age)different embarked  and age  ')+
  geom_boxplot()              # 2 ; C-212 ; Q-50 ;S-782, 's' more younger than 'c'

ggplot(titan02,aes(Age,color=factor(Pclass)))+
  labs(x='age',y='pclass',title='(mean_age)different age and pclass')+
  geom_freqpoly(binwidth=2)         #### talbe(titan01$Pclass) ###every Pclass  1-284; 2-261; 3-501,

# table(titan02$Rname,titan02$Survived)
ggplot(titan02,aes(factor(Rname),Survived))+
  labs(x='Rname',y='pclass',title='(mean_age)different age and pclass')+
  geom_jitter()

ggplot(titan02,aes(Pclass))+
  geom_histogram()+
  coord_flip()+
  facet_wrap(~Survived,ncol = 1)+
  labs(x='pclass',y='pclass count',title='(mean_age)pclass and survived')     ########

titan02$Rname<-as.factor(titan02$Rname)
titan02$Pclass<-as.factor(titan02$Pclass)
ggplot(titan02,aes(Rname,fill=Pclass))+
  geom_bar(position = "dodge")+
  labs(x='pclass',y='pclass count',title='(mean_age)pclass and survived')
# table(Rname,Pclass)

ggplot(titan02,aes(Pclass,fill=Embarked))+
  geom_bar(position = "dodge")+
  facet_wrap(~Survived)+
  labs(x='pclass',y='pclass count',title='(mean_age)pclass and embarked')
# detach(titan02)

titan020<-titan02[c(1,2,3,5,6,10,12,13,14)]
summary(titan020)
titan020<-na.omit(titan020)
titan020<-data.frame(titan020)
titan020$Rname<-as.factor(titan020$Rname)
titan020$Survived<-factor(titan020$Survived,levels = c(0,1),labels = c("die","sur"))
select<-sample(1:nrow(titan020),length(titan020$Survived)*0.7)
train020<-titan020[select,]
test020<-titan020[-select,]

set.seed(730)
fit.forest02<-randomForest(Survived~Sex+Rname+Fare+Pclass+family+Embarked+Age
                           ,data=train020,importance=TRUE)
fit.forest02
plot(fit.forest02,ylim=c(0,0.3))
legend('topright',colnames(fit.forest02$err.rate),col=1:3,fill=1:3)
plot(fit.forest)

importance(fit.forest02,type=2)  #value's importance

forest.pred02<-predict(fit.forest02,test020)
forest.perf02<-table(forest.pred02,test020$Survived,
                     dnn=c("Actual","Predicted"))
forest.perf02

impor02<-importance(fit.forest02)
forest02<-data.frame(var=row.names(impor02),
                     import=round(impor02[,'MeanDecreaseGini'],2))
ggplot(forest02,aes(x=reorder(var,import),y=import,fill=import))+
  geom_bar(stat = 'identity')+
  geom_text(aes(label=import,color='red'),show.legend = FALSE)+
  theme(axis.text.x= element_text(angle =90,hjust = 0.5,vjust = 0.5))+
  labs(x='variable',y='titan02(mean_age) import')+
  coord_flip()

solution<-data.frame(PassengerID = test020$PassengerId, Survived = forest.pred02)
write.csv(solution, file = 'forest_Solution.csv', row.names = F)

''''
''''
---
# Any results you write to the current directory are saved as output.