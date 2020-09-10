list.files("../input")
library(ggplot2)
library(visdat)
library(dplyr)
library(randomForest)
library(caret)
library(stringr)
library(rpart)


Train=read.csv('../input/train.csv', header=T, na.strings = c("", 'NA'))
Test=read.csv('../input/test.csv', header=T, na.strings = c("", 'NA'))
gender_submission=read.csv('../input/gender_submission.csv', header=T, na.strings = c("", 'NA'))
Titanic=bind_rows(Train, Test)

#Checking for NA

vis_miss(Titanic)

#Feature Engineering

#Getting first letter of each cabin names

Impute_cabin=function(cabin){
  cabin_letter=substring(cabin, 1, 1)
  cabin_letter[is.na(cabin_letter)]<-'U'
  return(cabin_letter)
}

Titanic['Cabin']<-Impute_cabin(Titanic$Cabin)
Titanic['Cabin']<-as.factor(Titanic$Cabin)

#Creating a variable family size

Titanic['Family.size']=Titanic$Parch+Titanic$SibSp
Titanic['Family.size']<-as.numeric(Titanic$`Family.size`)

#Creating additional variable on whether a person is single, has a small or large family size

Titanic['Single']=if_else(Titanic$`Family.size`<2, 1, 0)
Titanic['Small_Family']=if_else(Titanic$`Family.size`==2, 1, 0)
Titanic['Large_Family']=if_else(Titanic$`Family.size`>2, 1, 0)

# Extracts passenger titles
Titanic$title <- str_sub(Titanic$Name, str_locate(Titanic$Name, ",")[ , 1] + 2, str_locate(Titanic$Name, "\\.")[ , 1] - 1)

# combines some passenger titles
male_noble_names <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Rev", "Sir")
Titanic$title[Titanic$title %in% male_noble_names] <- "male_noble"

female_noble_names <- c("Lady", "Mlle", "Mme", "Ms", "the Countess")
Titanic$title[Titanic$title %in% female_noble_names] <- "female_noble"

Titanic$title<-as.factor(Titanic$title)


#Imputing missing values in the age column, Fare and embarked

Titanic['Pclass']<-as.factor(Titanic$Pclass)

Titanic%>%ggplot(aes(Pclass, Age, fill=Sex))+geom_boxplot()+
  ggtitle("Passenger Age in terms of Pclass and Gender")

#Impute Age and fare

Titanic$Age[is.na(Titanic$Age)] <- with(Titanic, ave(Age, Pclass, title, 
                                                     FUN = function(x) median(x, na.rm=TRUE)))[is.na(Titanic$Age)]

Titanic$Age[is.na(Titanic$Age)]<-median(Titanic$Age, na.rm = TRUE)
Titanic$Fare[is.na(Titanic$Fare)]<-median(Titanic$Fare, na.rm = TRUE)
Titanic$Embarked[is.na(Titanic$Embarked)]<-"S"

Titanic['Survived']<-as.factor(Titanic$Survived)

vis_miss(Titanic)

#Splitting the model to training and testing set

Titanic=Titanic%>%select(-'Name', -'PassengerId', -'Ticket')

# Set a random seed
set.seed(754)

Train=Titanic[1:891,]
Test=Titanic[892:1309,]

#Creating the model

Forest=randomForest(Survived~.-Survived,data=Train,ntree=100)
round(importance(Forest), 2)

#Making our predictions

Pred=predict(Forest, Test)
table(Pred, gender_submission$Survived)

confusionMatrix(Pred, as.factor(gender_submission$Survived),positive = "1")
precision=posPredValue(Pred, as.factor(gender_submission$Survived),positive = "1")
recall=sensitivity(Pred, as.factor(gender_submission$Survived),positive = "1")
F1=(2*precision*recall)/(precision+recall)

#Creating our final output

gender_submission['Predictions']<-Pred
write.csv(gender_submission, file = "Titanic_Output",row.names=FALSE)

