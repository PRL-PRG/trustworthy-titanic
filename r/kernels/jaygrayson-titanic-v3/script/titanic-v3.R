
## Exploratory analysis of Titanic data set
## Jason M. Grayson
## Started April 24, 2017

## This is analysis of Titanic survival. Prior biases come mainly from movies and books I have read about the sinking of Titanic (Titanic, A Night To Remember). My hypothesis  is that survival is a function ultimately of distance to the lifeboats and then some admission criteria, essentially it was a race to the lifeboats and then there was a decision point at the boat. Everything else relates to those two things.

## First clear up workspace so only Titanic data is present
rm(list=ls())

## Set working directory
setwd("../input")

## Load up some libraries that may be used for analysis
library(Hmisc)
library(mice)

## Lets read in the training and test data
titanic_kaggle<-read.csv(file="train.csv")
titanic_test<-read.csv(file = "test.csv")

## We need to check for gaps throughout the data
md.pattern(titanic_kaggle)
md.pattern(titanic_test)

## I am not going to impute Embarkment, it is 2 out of 891. Maybe you could look at fares but that seems spurious. So I will cut out 2.
titanic_kaggle<-subset(titanic_kaggle, Embarked !="")


## Because test lacks survived add it as NA
titanic_test$Survived<-NA

## Now lets link it up
tt_full<-rbind(titanic_kaggle,titanic_test)

## Lets examine the full data for NAs
md.pattern(tt_full)

## Because of Kaggle rules, we need all 418 entries, so will impute Age and Fare gaps as median
tt_full$Fare<-impute.default(tt_full$Fare,fun=median)
tt_full$Age<-impute.default(tt_full$Age,fun=median)

## Check again for gaps/NA
md.pattern(tt_full)

## Okay only survival has a gap, so some decisions about variables can be made now.

## I am going to make some judgement calls about what to chop out. The goal of this analysis is to predict survival. I will remove some variables that I hypothesize are not going to influence survival such as name and ticket number and cabin. I do think cabin mattered but only as a measure of distance from survival (lifeboat) and socioeconomic status. Also we will remove passenger ID.
tt_full<-tt_full[,-c(1,4,9,11)]

## We need to make variables appropiate classes
tt_full$Survived<-as.factor(tt_full$Survived)
tt_full$Pclass<-as.factor(tt_full$Pclass)
tt_full$SibSp<-as.factor(tt_full$SibSp)
tt_full$Parch<-as.factor(tt_full$Parch)
tt_full$Age<-as.numeric(tt_full$Age)
tt_full$Fare<-as.numeric(tt_full$Fare)

## okay here we will split the data back into training and testing
titanic_training<-tt_full[1:889,]
titanic_test<-tt_full[890:1307,]

## Call training tk to minimize confusion
tk<-titanic_training


## set seed and select training and test sets
set.seed(1234)
train<-sample(nrow(tk),0.7*nrow(tk))
titanic_train<-tk[train,]
titanic_val<-tk[-train,]


## Note random forest, code is derived from R in action code

## Now do random forest
library(randomForest)
titanic_train<-na.exclude(titanic_train)
fit.forest<-randomForest(Survived~.,data=titanic_train,importance=TRUE,ntree=5000)
fit.forest

