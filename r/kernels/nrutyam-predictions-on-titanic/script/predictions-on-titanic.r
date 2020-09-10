
#Loading all packages
library(dplyr)
library(plyr)
library(randomForest)
library(bit64)
library(Amelia)
library(e1071)
options(scipen=999)

#Accessing the datasets from Kaggle
train<-read.csv('../input/train.csv')
test<-read.csv('../input/test.csv')

#Checking the structures
str(train)
str(test)

sapply(train,summary)
sapply(test,summary)

# *** Str output shows that Test dataset has one variable missing, i.e Survived *** #
#Hence, let's create the variable Survived in test and assign missing values to it as of now
test$Survived<-NA

# *** Sapply with summary gives missing values information in all variables *** # 
#To verify for any missing data
missmap(train)
missmap(test)

#Missing values are reported in both the datasets for different variables
#In order to handle them we need to combine both the datasets and then treat blank values
#While treating blank values, we will exclude Survived as this is yet to be predicted for test
both<-rbind(train,test)

sapply(both,summary)

#Treating NAs in Age variable
aggregate(Age~Sex,both,mean)
#_____________________________#
#       Sex    Age            #
#       female 28.68709       #
#       male   30.58523       #
#_____________________________#
#Replacing missing ages by mean of the gender
for (i in 1:nrow(both)){
  if(both$Sex[i]=="male" && is.na(both$Age[i])==TRUE){
    both$Age[i]<-30.58
  }
  else if(both$Sex[i]=="female" && is.na(both$Age[i])==TRUE){
    both$Age[i]<-28.68
  }
  else {
    both$Age[i]<-both$Age[i]
  }
}

#Treating blanks in Embarked variable
#PassengerId(62,830) with missing info in Embarked belong to Pclass 1 & have paid an amount of 80
aggregate(Fare~Pclass+Embarked,both,range)
#Output indicates that for Pclass-1, an amount of 80 is accepted for boarding points 'C' & 'S'
#To select one among these 2 options, lets check for the frequency of each boarding points
table(both$Embarked)
#_____________________#
#      C   Q   S      #
#     270 123 914     #
#_____________________#
#Output indicates 'S' has the highest value
#Replacing missing Embarked values by the most occurring level
both$Embarked[c(62, 830)]<-'S'

#Treating NA in Fare variable
#PassengerId(1044) with missing info in Fare belongs to Pclass 3
aggregate(Fare~Pclass,both,median)
#___________________________#
#     Pclass    Fare        #
#      1       60.0000      # 
#      2       15.0458      #
#      3       8.0500       #
#___________________________#
#Replacing missing Fare by median of the Pclass
both$Fare[c(1044)]<-8.05

#Checking blank values in Cabin variable
t=count(both, 'Cabin')
t
1014/1309
# *** Output shows Cabin data missing for 1014 records, which is 77% of the total observations *** #
#We don't have enough information to predict for this variable, so we are not treating it#

#Changing data types in both
both$Ticket<-as.character(both$Ticket)
both$Name<-as.character(both$Name)

missmap(both)

# *** Let's look into all variables and try to derive new variables for our analysis *** #

# *** Women and children were given first preference to use lifeboat *** #
#We have no direct information whether the passenger was a child or adult
#Hence this has to be derived. We can take the help of the variable Age
#If 0, means not a child; 1, means a child
#Generic formula applied if age < 18 then considered as child
both$IsChild<-ifelse(both$Age < 18,1,0)

# *** Checking SibSp & Parch *** #
#As per description: No of siblings / spouses aboard the Titanic is denoted by SibSp
#As per description: No of parents / children aboard the Titanic is denoted by Parch
#Using these fields, let's find out the family size of each passenger
both$Fsize<-both$SibSp+both$Parch+1
table(both$Fsize)
#Further classifying using newly derived variable Fsize
both$Fcat <- 'Large'
both$Fcat[both$Fsize==1] <- 'Single'
both$Fcat[both$Fsize > 1 & both$Fsize <= 4] <- 'Small'

# *** Checking Name *** #
head(both$Name)
both$Title<-sapply(both$Name, FUN=function(x) {strsplit(x,split='[,.]')[[1]][2]})
both$Title<-gsub(' ', '', both$Title)
table(both$Title)
#Titles with very low counts to be collated into one object
lowcnt_title <- c('Dona', 'Lady', 'Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer','theCountess')
#Classifying further
both$Title[both$Title %in% c('Ms', 'Mlle')] <- 'Miss'
both$Title[both$Title == 'Mme'] <- 'Mrs'
both$Title[both$Title %in% lowcnt_title] <- 'Others'
table(both$Title)

# *** Naives Bayes approach used for our prediction, here independent variables should be factor in nature *** #
#Keeping this into picture, we will convert Fare & Age into different categories for analysis

# *** Categorizing Fare *** #
both$Fare2<-'30+'
both$Fare2[both$Fare < 30 & both$Fare >= 20] <- '20-30'
both$Fare2[both$Fare < 20 & both$Fare >= 10] <- '10-20'
both$Fare2[both$Fare < 10] <- '<10'

# *** Categorizing Age *** #
both$Age2<-'Above 50'
both$Age2[both$Age <= 50 & both$Age > 40] <- '41-50'
both$Age2[both$Age <= 40 & both$Age > 30] <- '31-40'
both$Age2[both$Age <= 30 & both$Age > 20] <- '21-30'
both$Age2[both$Age <= 20 & both$Age > 10] <- '11-20'
both$Age2[both$Age <= 10] <- 'Within 10'

#Changing data types in both
both$Title<-as.factor(both$Title)
both$Fcat<-as.factor(both$Fcat)
both$SibSp<-as.factor(both$SibSp)
both$Parch<-as.factor(both$Parch)
both$Pclass<-as.factor(both$Pclass)
both$Fare2<-as.factor(both$Fare2)
both$IsChild<-as.factor(both$IsChild)
both$Age2<-as.factor(both$Age2)


# *** As missing values treatment & feature engineering are done, let's split the datasets *** #
data_train<-both[1:891,]
data_test<-both[892:1309,]

#In this challenge, we have been asked to complete the analysis of what sorts of people were likely to survive. 
#In particular, to apply the tools of machine learning to predict which passengers survived the tragedy.
#We will apply Naive Bayes & SVM techniques
#First to find out the count of people survived
table(data_train$Survived)
#Taking percentage
prop.table(table(data_train$Survived))
# *** Output shows only 38% Survived the tragedy, meaning a majority of people on board died *** # 
#Keeping this into picture, lets assume all died in data_test dataset
data_test$Survived<-0

#Let's find out the count of people survived based on their gender
table(data_train$Sex,data_train$Survived)
#Taking percentage
prop.table(table(data_train$Sex,data_train$Survived),1)
# *** Output indicates that gender of the person really affected the chances of survival *** #

#Checking with Sex & IsChild together
t=count(data_train, c('Survived','IsChild', 'Sex'))
t
# *** Output still shows gender an important variable for depicting survival rate *** #

#Checking with Embarked variable
boxplot(data_train$Survived~data_train$Embarked)
table(data_train$Embarked,data_train$Survived)
prop.table(table(data_train$Embarked,data_train$Survived),1)
# *** Output says people departed from point C have a better survival rate *** #

#Checking with Pclass variable
boxplot(data_train$Survived~data_train$Pclass)
table(data_train$Pclass,data_train$Survived)
prop.table(table(data_train$Pclass,data_train$Survived),1)
# *** Output says people with Pclass-1 have a better survival rate *** #

#Checking no of survived persons with Pclass & Embarked
aggregate(Survived~Pclass+Embarked,data_train,sum)

#Checking no of survived persons with Pclass, Sex & Embarked
aggregate(Survived~Pclass+Sex+Embarked,data_train,sum)
# *** Female has better chances of survival, so let's assume all females in data_test survived *** #
data_test$Survived<-ifelse(data_test$Sex=="female",1,0)

#Changing Survived variable into factor
data_train$Survived<-as.factor(data_train$Survived)
data_test$Survived<-as.factor(data_test$Survived)

###Creating Our Model with Naive Bayes method
data_1<- data_train[,c("Survived","Pclass","Sex","SibSp","Parch","Fare2","Embarked","Fcat","Title","Age2")]
data_test1<-data_test[,c(2,3,5,7,8,12,15,16,17,18)]
nb_model<-naiveBayes(Survived~.,data=data_1)
nb_model

#Predicting Survival rate in test dataset
nb_test_predict <- predict(nb_model,data_test1[,-1])

#Confusion matrix
table(pred=nb_test_predict,true=data_test1$Survived)

#Determining accuracy from confusion matrix
mean(nb_test_predict==data_test1$Survived)
#____________________________Accuracy rate from Naive Bayes approach is 0.8923445_________________________#

###Checking Our Model with SVM method
svm_model<-svm(Survived~.,data=data_1)
svm_model

#Predicting Survival rate in test dataset
svm_test_predict <- predict(svm_model,data_test1[,-1])

#Confusion matrix
table(pred=svm_test_predict,true=data_test1$Survived)

#Determining accuracy from confusion matrix
mean(svm_test_predict==data_test1$Survived)
#____________________________Accuracy rate from SVM approach is 0.9712919___________________________________#


# *** Building the output dataset with SVM & exporting it into csv *** #
MyOutput<-data.frame(PassengerId=data_test$PassengerId,Survived=svm_test_predict)
write.csv(MyOutput,"MyOutput.csv",row.names = FALSE)


