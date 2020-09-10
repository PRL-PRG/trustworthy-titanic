## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,message=FALSE,warning=FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr) # Data Munging
library(ggplot2) # Data Visualisation
library(ggthemes) #Themes
library(corrplot) #Correlation
library(rpart) #Data modelling
library(randomForest) #Data modelling
library(pscl)
library(Deducer) # Modelling
library(Amelia) #Missing values
library(forcats)
library(rpart.plot) # Random forest
library(Hmisc)
library(VIM)
train=read.csv("../input/train.csv",header=TRUE,stringsAsFactors = FALSE)
test=read.csv("../input/test.csv",header=TRUE,stringsAsFactors = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cat("There are ",nrow(train),"rows and",ncol(train),"columns in train dataset")
cat("There are",nrow(test),"rows and",ncol(train),"columns in test dataset")
summary(train)

summary(test)


## ----fig.height=7----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missmap(train,main="Titanic Train Data-Missing Value Visualisation",col=c("red","green"),legend=FALSE)
missmap(test,main="Titanic Test Data-Missing Value Visualisation",col=c("red","green"),legend=FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic=full_join(train,test)
summary(titanic)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(is.na(titanic$Age)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
age=rpart(Age ~Pclass+Sex+SibSp+Parch+Fare+Embarked,data=titanic[!(is.na(titanic$Age)),],method="anova")
titanic$Age[is.na(titanic$Age)]=predict(age,titanic[is.na(titanic$Age),])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(is.na(titanic$Age)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic,aes(Age,fill="green"))+geom_density(alpha=0.4)+labs(x="Age",y="Count",title="Distribution of Age after imputation")+theme(legend.position="none")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cat("There is",sum(is.na(titanic$Fare)),"missing value in Fare")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
which(is.na(titanic$Fare))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic[1044,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fare=rpart(Fare ~Parch+SibSp+Sex+Pclass,data=titanic[!(is.na(titanic$Fare)),],method="anova")
titanic$Fare[(is.na(titanic$Fare))]=predict(fare,data=titanic[is.na(titanic$Fare),])
rpart.plot(fare,shadow.col="pink",box.col="gray",split.col="magenta",main="Decision Tree for Imputation")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(is.na(titanic$Fare)))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic,aes(Fare,fill="green"))+geom_density(alpha=0.4)+labs(x="Fare",y="Fare Density",title="Distribution of Fare after imputation")+theme(legend.position="none")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(titanic$Name)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Title=gsub('(.*, )|(\\..*)','',titanic$Name)
head(titanic$Title)
table(titanic$Title,titanic$Sex)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic <- titanic %>% mutate(Title = factor(Title)) %>% mutate(Title = fct_collapse(Title, "Miss" = c("Mlle", "Ms"), "Mrs" = "Mme", "Ranked" = c( "Major", "Dr", "Capt", "Col", "Rev"),"Royalty" = c("Lady", "Dona", "the Countess", "Don", "Sir", "Jonkheer")))
str(titanic$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Families= factor(ifelse(titanic$SibSp + titanic$Parch + 1> 1,"Yes","No"))
prop.table(table(titanic$Families))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(titanic$Pclass))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic=titanic %>% mutate(Survived=factor(Survived)) %>% mutate(Survived=fct_recode(Survived,"No"="0","Yes"="1"))
# train=titanic[1:891,]
# test=titanic[1:1309,]
prop.table(table(train$Survived))


## ----fig.height=6----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,],aes(Sex,fill=Survived))+geom_bar(position="fill")+theme_fivethirtyeight()+theme(legend.position="bottom",plot.title=element_text(size=15,hjust=0.5))+labs(x="Gender",y="Survival Rate",title="Survival by Gender")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(titanic$Pclass)
ggplot(titanic[1:891,],aes(Pclass,fill=Survived))+geom_bar(position="fill")+facet_wrap(~Sex)+theme_fivethirtyeight()+theme(legend.position="bottom",plot.title=element_text(size=15,hjust=0.5))+labs(x="Passenger Class",y="Survival Rate",title="Survival by Passenger Class Vs Gender")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,],aes(Title,fill=Survived))+geom_bar(position="fill")+facet_wrap(Pclass~Sex)+theme_few()+theme(legend.position="bottom",plot.title=element_text(size=15,hjust=0.5),plot.subtitle = element_text(size=10),axis.text.x=element_text(angle=90))+labs(x="Title",y="Survival Rate",title="Survival by Title Vs Gender",subtitle="Visualizing by Passenger Class")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,],aes(Embarked,fill=Survived))+geom_bar(position="fill")+theme_few()+theme(legend.position="bottom",plot.title=element_text(size=15,hjust=0.5))+labs(x="Title",y="Survival Rate",title="Survival by Embarkment")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic =titanic %>% mutate(Embarked=ifelse(Embarked=="",names(which.max(table(titanic$Embarked))),Embarked))
ggplot(titanic[1:891,],aes(Embarked,fill=Survived))+geom_bar(position="fill")+theme_few()+theme(legend.position="bottom",plot.title=element_text(size=15,hjust=0.5))+labs(x="Title",y="Survival Rate",title="Survival by Embarkment")+facet_wrap(Pclass~Sex,scales="free")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,],aes(Families,fill=Survived))+geom_bar(position="fill")+theme_few()+theme(legend.position="bottom",plot.title=element_text(size=15,hjust=0.5))+labs(x="With Family or Not",y="Survival Rate",title="Chance of Survival if travelled with Family")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,],aes(Survived,Age,fill=Sex))+geom_boxplot()+theme(legend.position="bottom",plot.title=element_text(size=15,hjust=0.5))+labs(x="Survived",y="Age",title="Median age of Survival")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(titanic$Cabin)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Deck=factor(sapply(titanic$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
str(titanic$Deck)
table(is.na(titanic$Deck)) #297 missing values
round(prop.table(table(titanic$Deck,titanic$Survived))*100,2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)
titanic$Deck=with(titanic,impute(Deck,'random'))
ggplot(titanic[1:891,],aes(Deck,fill=Survived))+geom_bar(position="fill")+theme(legend.position="bottom",plot.title=element_text(size=15,hjust=0.5))+labs(x="Deck",y="Survival Rate",title="Survival by Deck")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic=titanic %>% mutate(FamilySize=SibSp+Parch+1) %>% mutate(Type=ifelse(FamilySize==1,"Single",ifelse(FamilySize>=3,"Large","2 People")))
ggplot(titanic[1:891,],aes(Type,fill=Survived))+geom_bar(position="fill")+theme(legend.position="bottom",plot.title=element_text(size=15,hjust=0.5))+labs(x="FamilyType",y="Survival Rate",title="Survival by FamilySize")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
aggr(titanic,prop=FALSE,combined=TRUE,sortVars=TRUE,sortCombs=TRUE,numbers=TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic = titanic %>% mutate(Type=factor(Type)) %>% mutate(Embarked=factor(Embarked)) %>% mutate(Sex=factor(Sex))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train=titanic[1:891,]
test=titanic[892:1309,]
names(train)
str(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rfmodel=randomForest(factor(Survived) ~ Pclass+Sex+Age+Fare+Embarked+Title+Deck+FamilySize+Type+SibSp+Parch,data=train,importance=TRUE)
print(rfmodel)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(rfmodel, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
varImpPlot(rfmodel)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
variable=c("Pclass","Sex","Age","Fare","Embarked","Title","Deck","FamilySize","Type","SibSp","Parch")
tunedrfmodel=tuneRF(x=train[,variable],y=as.factor(train$Survived),mtryStart = 3,ntreeTry = 100,stepFactor = 2,improve=0.001,trace=FALSE,plot=FALSE,doBest = TRUE,nodesize=200,importance=TRUE)
varImpPlot(tunedrfmodel)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainpredict=table(predict(tunedrfmodel),train$Survived)
caret::confusionMatrix(trainpredict)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test$Survived=NULL
titanicpred=predict(tunedrfmodel,test,OOB=TRUE,type="response")
titanicpred=ifelse(titanicpred=="No",0,1)
solution=data.frame(PassengerID=test$PassengerId,Survived=titanicpred)
write.csv(solution,file="submission.csv",row.names=F)

