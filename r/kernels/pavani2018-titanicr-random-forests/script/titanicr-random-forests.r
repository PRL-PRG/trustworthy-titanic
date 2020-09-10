
library(tidyverse) # metapackage with lots of helpful functions
list.files(path = "../input")

library(tidyverse) # metapackage with lots of helpful functions
list.files(path = "../input")
titanic.train<-read.csv(file = "../input/train.csv",stringsAsFactors = FALSE,header =TRUE)
titanic.test<-read.csv(file = "../input/test.csv",stringsAsFactors = FALSE,header =TRUE)

titanic.train$IstrainSet<-TRUE
titanic.test$IstrainSet<- FALSE

#dupnames<-as.character(titanic.full[which(duplicated(as.character(titanic.full$Name))),"Name"])

titanic.test$Survived<-NA
##combine the datasets
titanic.full<- rbind(titanic.train,titanic.test)
##clean the data files
##replace the blank values of ages in both data sets by median value
median(titanic.train$Age)
##NA.RM to remove null values
table(titanic.full$Embarked)
##what are those null values
titanic.full[titanic.full$Embarked == '',"Embarked"]

titanic.full[titanic.full$Embarked == '',"Embarked"]<-'S'

##Now replace blanks of age by median
library(stringr)
##for(Name in titanic.full)
#Title1<-str_extract(titanic.full$Name,"[,] [A-z][a-z].")
#Title2<-str_extract(titanic.full$Name,"[,] [A-z][a-z][a-z].")
Title<-str_extract(titanic.full$Name," ([A-Za-z]+)[.]" )
titanic.full$Title<-Title

#Assign each of these title values to a variable and find median
##According to title u shud find the median of each group like Mr. Mis MrsTitle
##titanic.full$Age
# 
# agecluster <-aggregate(titanic.full$Age, by=list(Title),FUN= median, na.rm=TRUE)
#  if(Title %in% agecluster$Group.1)
#    is.na(titanic.full$Age)<-agecluster$x
#****INSTALL DPLYR PACKAGE BEFORE RUNNING GROUPBY***#
#install.packages("dplyr")
library(dplyr)
#titanic.full <- titanic.full %>% group_by(titanic.full$Title) %>% mutate(num_avg = median(Age,na.rm= T))
#titanic.full$Age[is.na(titanic.full$Age)] <- titanic.full$num_avg[is.na(titanic.full$Age)]
# age.median<-median(titanic.full$Age,na.rm=TRUE)
# titanic.full[is.na(titanic.full$Age),"Age"]<-age.median

##CHECHKOUT FOR MISSING VALUES OF FARE AND REPLACE THEMT
##i am taking all the non outlier values in a variable by comparing it with upper whisker
upper.whisker<-boxplot.stats(titanic.full$Fare)$stats[5]
nonoutlier<-titanic.full$Fare<upper.whisker
##titanic.full[nonoutlier,]
titanic.full$Embarked<-as.factor(titanic.full$Embarked)

titanic.full$Name<-as.factor(titanic.full$Name)

titanic.full$Sex<-as.factor(titanic.full$Sex)

titanic.full$Title<-as.factor(titanic.full$Title)

titanic.full$Ticket<-as.factor(titanic.full$Ticket)

#Assign each of these title values to a variable and find median
##According to title u shud find the median of each group like Mr. Mis MrsTitle
##titanic.full$Age

#this is working good the above as.factor()

#implement is factor and that code for

##now write the formula for fare equation it depends on all the other cols

titanic.full$FamilySize <- titanic.full$SibSp + titanic.full$Parch + 1
table(titanic.full$FamilySize)
titanic.full$FamilySizelabel <- sapply(1:nrow(titanic.full), function(x) 
  ifelse(titanic.full$FamilySize[x]==1, "Single", 
         ifelse(titanic.full$FamilySize[x]>4, "Large", "Small")))

titanic.full$FamilySizelabel<-as.factor(titanic.full$FamilySizelabel)





#install.packages("rpart")
library(rpart)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize+Ticket,
                data=titanic.full[!is.na(titanic.full$Age),], 
                method="anova")
titanic.full$Age[is.na(titanic.full$Age)] <- predict(Agefit,titanic.full[is.na(titanic.full$Age),])

fare.eq = "Fare~Pclass+Sex+Parch+Age+SibSp+Embarked+Title+FamilySizelabel"
fare.model<-lm(
  
  formula = fare.eq,
  data=titanic.full[nonoutlier,]
)

##Now predict the new data on trained model so whats the new data???
##new data
##only those rows has missing vals of fare
fare.row<-titanic.full[
  
  is.na(titanic.full$Fare),
  c("Pclass","Sex","Age","Embarked","SibSp","Parch","Title","FamilySizelabel")
  ]
fare.predictions<-predict(fare.model,newdata = fare.row)

titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.predictions
##one missing value
#fare.median<-median(titanic.full$Fare,na.rm=TRUE)
#titanic.full[is.na(titanic.full$Fare),"Fare"]<-fare.median




##Survived column caluclation for test dataset

# Survived.eq<-"Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+FamilySize+FamilySizelabel+Title"
# survive.formula<-as.formula(Survived.eq)














##Use as.factor() function for categorical variables like Sex Name and Embarked

titanic.full$Embarked<-as.factor(titanic.full$Embarked)

titanic.full$Name<-as.factor(titanic.full$Name)

titanic.full$Sex<-as.factor(titanic.full$Sex)

titanic.full$FamilySizelabel<-as.factor(titanic.full$FamilySizelabel)
titanic.full$Title<-as.factor(titanic.full$Title)

titanic.full$Ticket<-as.factor(titanic.full$Ticket)

#Now split the dataset to two types of data sets
titanic.train<-titanic.full[titanic.full$IstrainSet == TRUE,]
titanic.test<-titanic.full[titanic.full$IstrainSet == FALSE,]
##make survived column as afactor

titanic.train$Survived<-as.factor(titanic.train$Survived)

##Survived column caluclation for test dataset

# Survived.eq<-"Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+FamilySize+FamilySizelabel+Title"
# survive.formula<-as.formula(Survived.eq)

##INSTALL THE RANDOM FOREST AND APPLY LIBRARY() FUNCTION
# install.packages("randomForest")
# 
# library(randomForest)
# 
# ##using random forest

## **Survived column caluclation for test dataset**

Survived.eq<-"Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+FamilySize+FamilySizelabel+Title"
survive.formula<-as.formula(Survived.eq)
titanic.model<-randomForest(formula=survive.formula,data = titanic.train,ntree =2000,mtry = 3,nodesize = 0.01*nrow(titanic.test))

##ntree means number of trees and mtry is number of variables in a split,nod of nodes is 1 percent of total train set

#features.eq<-"Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+FamilySize+FamilySizelabel+Title+Ticket"

##now predit the test data from train data

Survived <- predict(titanic.model,titanic.test, OOB=TRUE, type = "response")


##Survived column caluclation for test dataset

##Survived <- predict(titanic.model,newdata = titanic.test)

##Passenger Id and Survivied columns write to a CSV file and submit

PassengerId<-titanic.test$PassengerId
output.df<-as.data.frame(PassengerId)
output.df$Survived<-Survived

##YOU CAN CHECK YOU RESULTS BY HISTOGRAM

##ggplot(titanic.full,aes(x= Pclass,fill = factor(Survived))) + geom_bar()

write.csv(output.df,file="titanic_kaggle_13.csv",row.names = FALSE)

##row.names is FALSE because to prevent writing those numbers into thecsv file

