
# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

#set the working directory
setwd("C:/Users/sanjukta/Desktop/Kaggle/Titanic")
# Any results you write to the current directory are saved as output.
#########################################################
#load the below library before execution

library(rpart)
library(rpart.plot)
library(ggplot2)
library(caret)
library(randomForest)
#load titanic train and test data
titanic <- read.csv("train.csv",stringsAsFactors = FALSE)
titanic_test <- read.csv("test.csv",stringsAsFactors = FALSE)

#load the test data in R

#CHECK IF THE PASSENGER ID IS UNIQUE
length(unique(titanic$PassengerId))
#all passenger ids are unique

length(unique(titanic_test$PassengerId))
#all passenger ids are unique

head(titanic)
str(titanic)
head(titanic_test)
str(titanic_test)
#field cabin has lot of spaces or missing values in both training and test data 

length(which(titanic$Cabin==""))
#so 687 ROWS has column cabin as spaces which is far more than 50%, hence better to remove the variable cabin

length(which(titanic_test$Cabin==""))
#so 327 ROWS has column cabin as spaces which is far more than 50%, hence better to remove the variable cabin
titanic1 <- titanic[,-11]
titanic_test1 <- titanic_test[,-10]

#training dataset
#divide the variable Name into First name,Surname and Title
titanic1$title <- gsub('(.*, )|(\\..*)', '', titanic1$Name)

#strsplit(titanic1$Name,",",fixed = FALSE)[[]][[1]]
#strsplit(titanic1$Name,"Mr.| Miss.|Mrs.|Master|Ms.",fixed = FALSE)[[1]][[2]]
titanic1$surname <- sapply(titanic1$Name,function(x) strsplit(x,split = ",",fixed = FALSE)[[1]][[1]])
#titanic1$FirstName <- sapply(titanic1$Name,function(x) strsplit(x,"Mr.| Miss.|Mrs.|Master|Ms.",fixed = FALSE)[[1]][[2]])



#repeat the same process for test data 
#divide the variable Name into First name,Surname and Title
titanic_test1$title <- gsub('(.*, )|(\\..*)', '', titanic_test1$Name)
titanic_test1$surname <- sapply(titanic_test1$Name,function(x) strsplit(x,split = ",",fixed = FALSE)[[1]][[1]])

#check for missing values in training set
sum(is.na(titanic1))

which(is.na(titanic1[,1]))
which(is.na(titanic1[,2]))
which(is.na(titanic1[,3]))
which(is.na(titanic1[,4]))
which(is.na(titanic1[,5]))
which(is.na(titanic1[,6]))
which(is.na(titanic1[,7]))
which(is.na(titanic1[,8]))
which(is.na(titanic1[,9]))
which(is.na(titanic1[,10]))
which(is.na(titanic1[,11]))

#only column 6 contains NA
#check for missing values in test set
sum(is.na(titanic_test1))
which(is.na(titanic_test1[,5]))
which(is.na(titanic_test1[,9]))

#86 rows of age column and 1 row of fare column is missing in test data 

#total family size is sum of sibsp and parch plus the passenger himself

titanic1$familysize <- titanic1$SibSp + titanic1$Parch + 1

titanic_test1$familysize <- titanic_test1$SibSp + titanic_test1$Parch + 1





#check the levels of Pclass,Title
levels(as.factor(titanic1$Pclass))
levels(as.factor(titanic_test1$Pclass))
#both the training and test data contain same levels
levels(as.factor(titanic1$title))
levels(as.factor(titanic_test1$title))

#combine title like Don,Sir,Rev,Jonkheer to Sir and Dona,Lady,Countess,the Countess,Mlle into Lady

for(i in 1:nrow(titanic1)){
  if(titanic1[i,12]== 'Don'){
    titanic1[i,12] <- "Sir"
  }else if( titanic1[i,12]== 'Sir'){
    titanic1[i,12] <-  "Sir" 
  }else if( titanic1[i,12]== 'Rev' ){
    titanic1[i,12] <-  "Sir"
  }else if( titanic1[i,12]=='Jonkheer' ){
    titanic1[i,12] <-  "Sir"
  }else{
    next
  }
}

for( i in 1:nrow(titanic1)){
  if(titanic1[i,12]== 'Dona'){
    titanic1[i,12] <- "Lady"
  }else if( titanic1[i,12]== 'Lady'){
    titanic1[i,12] <-  "Lady" 
  }else if( titanic1[i,12]== 'Countess' ){
    titanic1[i,12] <-  "Lady"
  }else if( titanic1[i,12]=='the Countess' ){
    titanic1[i,12] <-  "Lady"
  }else if( titanic1[i,12]=='Mlle' ){
    titanic1[i,12] <-  "Lady"
  }else{
    next
  }
}

#repeat the same process with test data 
for(i in 1:nrow(titanic_test1)){
  if(titanic_test1[i,11]== 'Don'){
    titanic_test1[i,11] <- "Sir"
  }else if( titanic_test1[i,11]== 'Sir'){
    titanic_test1[i,11] <-  "Sir" 
  }else if( titanic_test1[i,11]== 'Rev' ){
    titanic_test1[i,11] <-  "Sir"
  }else if( titanic_test1[i,11]=='Jonkheer' ){
    titanic_test1[i,11] <-  "Sir"
  }else{
    next
  }
}

for( i in 1:nrow(titanic_test1)){
  if(titanic_test1[i,11]== 'Dona'){
    titanic_test1[i,11] <- "Lady"
  }else if(titanic_test1[i,11]== 'Lady'){
    titanic_test1[i,11] <-  "Lady" 
  }else if( titanic_test1[i,11]== 'Countess' ){
    titanic_test1[i,11] <-  "Lady"
  }else if( titanic_test1[i,11]=='the Countess' ){
    titanic_test1[i,11] <-  "Lady"
  }else if( titanic_test1[i,11]=='Mlle' ){
    titanic_test1[i,11] <-  "Lady"
  }else{
    next
  }
}
#lets check the title level again 
levels(as.factor(titanic1$title))
#the titles Don,Sir,Rev,Jonkheer are converted to Sir and Dona,Lady,Countess,the Countess,Mlle are converted  into Lady
levels(as.factor(titanic_test1$title))
#the titles Don,Sir,Rev,Jonkheer are converted to Sir and Dona,Lady,Countess,the Countess,Mlle are converted  into Lady


#convert the required variable to factors in training data

titanic1$Survived <- as.factor(titanic1$Survived)
titanic1$Pclass <- as.factor(titanic1$Pclass)
titanic1$Sex <- as.factor(titanic1$Sex)
titanic1$Embarked <- as.factor(titanic1$Embarked)
titanic1$title <- as.factor(titanic1$title)

#convert the required variable to factors in test data

titanic_test1$Pclass <- as.factor(titanic_test1$Pclass)
titanic_test1$Sex <- as.factor(titanic_test1$Sex)
titanic_test1$Embarked <- as.factor(titanic_test1$Embarked)
titanic_test1$title <- as.factor(titanic_test1$title)


str(titanic_test1)
str(titanic1)
##############################################################################################
#decision tree to predict age value in training set 
##############################################################################################
 #take a subset of titanic1 for all columns except Name,title,surname AND TICKET
regress1 <- titanic1[,-c(1,4,9,13)]

levels(regress1$Survived)

#train is regress11 except those record which contain age as NA
train <- regress1[-which(is.na(regress1$Age)),]
test <- regress1[which(is.na(regress1$Age)),]

install.packages("rpart")

tree_age <- rpart(Age~., data=train)
prp(tree_age)

age_pred <- predict(tree_age,test[,-4])

test$age_pred <- age_pred

#add the age value to the main dataframe titanic1

titanic1[which(is.na(titanic1[,6])),6] <- age_pred

#lets check missing value again
sum(is.na(titanic1))


##############################################################################################
#decision tree to predict age value in test set
##############################################################################################
#take a subset of titanic1 for all columns except Name,title,surname AND TICKET
regress2 <- titanic_test1[,-c(1,3,8,12)]




#use regress1 for decision tree modelling for predicting age value

#train is regress11 except those record which contain age as NA
train2 <- regress2[-which(is.na(regress2$Age)),]
test2 <- regress2[which(is.na(regress2$Age)),]


tree_test <- rpart(Age~., data=train2)
prp(tree_test)

age_pred2 <- predict(tree_test,test2[,-3])

test2$age_pred <- age_pred2

#add the age value to the main dataframe titanic1

titanic_test1[which(is.na(titanic_test1[,5])),5] <- age_pred2

#lets check missing value again
sum(is.na(titanic_test1))


#Fare value missing,lets substitute Fare with it's mean value
mean(titanic_test1$Fare,na.rm=TRUE)
titanic_test1[which(is.na(titanic_test1$Fare)),9] <- 35.63
######################################################################################################################
#EDA Part of training data 
######################################################################################################################
str(titanic1)


ggplot(titanic1,aes(x=Survived,fill=factor(Sex))) + geom_bar()
#male death rate is much more than that of females and females survival rate is higher than males
ggplot(titanic1,aes(x=Survived,fill=factor(Pclass)))+ geom_bar()
#Pclass 3 has maximum death rate , Pclass 1 has maximum surviva rate
ggplot(titanic1,aes(x=Age,y=Fare)) + geom_point(aes(col=factor(Survived))) 
ggplot(titanic1,aes(x=title,fill=factor(Survived)))+ geom_bar()
#survival rate  highest for MISS, after that Mr and Mrs. Death rate highest for Mr.,followed by Miss and Mrs.
ggplot(titanic1,aes(x=familysize,fill=factor(Survived))) + geom_bar() 
#most of the passengers have family size 1,2,3. rest followed by 4,5,6 are very few in numbers
#Death rate highest for individual people of family size 0 followed by 1 and 2
#some death rate are also seen for higher size family members witrh 6,7,8,9
ggplot(titanic1,aes(x=Embarked,fill=factor(Survived)))+ geom_bar()
#Embarked S : highest death rate
#Embarked S : highest survival rate but death rate are mush more in numbers
#Embarked C and Q death and survival are almost equal in ratio
Boxplot(titanic$Age~titanic$Survived)
Boxplot(titanic$Fare~titanic$Survived)
#people of higher class having high fare have higher survival rate 
  ######################################################################################################################
#end of EDA Part of training data 
######################################################################################################################
#Modelling using Decision Trees 
#delete the unneccessary columns name,surname and Ticket
str(titanic1)
set.seed(123)

indices <- sample(1:nrow(titanic1),0.7*nrow(titanic1))
regresst1 <- titanic1[indices,]
testR1 <- titanic1[-indices,]
#delete the Name,surname,Ticket from regresst1

regresst1 <- regresst1[,-c(4,9,13)]
testR1 <- testR1[,-c(4,9,13)]

tree_survive <- rpart(Survived~.,data=regresst1,method = "class")
prp(tree_survive)

predict_survive <- predict(tree_survive,testR1[,-2],type = "class")



confusionMatrix(predict_survive,testR1$Survived,positive = "1")
#          Reference
#Prediction   0   1
#0 156  31
#1   11  70

#Accuracy : 0.8433
#Sensitivity : 0.693 
# Specificity : 0.934



#randome forest model
library(randomForest)
random1 <- randomForest(Survived~.,data=regresst1,mtry=9,ntree=600,do.trace=2, proximity=FALSE,na.action = na.omit)


 plot(random1)


#looking at the plot lets take ntree as 200

random2 <- randomForest(Survived~.,data=regress1,mtry=6,ntree=270,do.trace=2, proximity=FALSE,na.action=na.omit)

plot(random2)
pred_surv <- predict(random2,testR1[,-2])

table(pred_surv,testR1$Survived)
confusionMatrix(pred_surv,testR1$Survived,positive = "1")
###################################################################

#Prediction   0   1
#0 158   4
#1   8  98

#Accuracy : 0.9552          
#95% CI : (0.9231, 0.9767)
     

#Kappa : 0.9057          
#Mcnemar's Test P-Value : 0.3865          

#Sensitivity : 0.9608          
#Specificity : 0.9518          
#Pos Pred Value : 0.9245          
#Neg Pred Value : 0.9753          
#Prevalence : 0.3806          
#Detection Rate : 0.3657          
#Detection Prevalence : 0.3955          
#Balanced Accuracy : 0.9563          

#'Positive' Class : 1               
#use the model random2 for test data titanic_test1
regress2$Survived <- c(0,1)
regress2$Survived <- as.factor(regress2$Survived)
colnames(regress2$Survived)[1] <- "Pclass"
regress2 <- rbind(regress1[1,],regress2)
regress2 <- regress2[-1,]
pred_survive <- predict(random2,regress2[,-1])
##
titanic_test1$Survived <- 0
titanic_test1 <- titanic_test1[,c(1,14,2,3,4,5,6,7,8,9,10,11,12,13)]
titanic_test1 <- rbind(titanic1[1,],titanic_test1)
titanic_test1 <- titanic_test1[-1,]
pred_survive2 <- predict(random2,titanic_test1[,-2])
titanic_test1$Survived <- 0
titanic_test1$Survived <- pred_survive2
write.csv(titanic_test1,"titanic_test2.csv")



