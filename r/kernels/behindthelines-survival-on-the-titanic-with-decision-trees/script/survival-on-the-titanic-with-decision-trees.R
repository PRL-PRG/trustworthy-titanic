# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(rpart)
library(rpart.plot)


# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.library(ggplot2)


dat=read.csv("../input/train.csv")

colSums(is.na(dat))

summary(dat)
str(dat)

# variable showing total family size
dat$familySize=dat$SibSp+dat$Parch
ggplot(dat,aes(x=familySize,fill=Survived==1))+geom_bar(position="fill")+ggtitle("Survival rate by Family Size(discrete)")+ylab("Prob of survival")

#converting family size into categorical based on survival rates as single,small,medium,large
dat$familySize=ifelse(dat$familySize>6,"large",ifelse(dat$familySize>3,"medium",ifelse(dat$familySize>0,"small","single")))
ggplot(dat,aes(x=familySize,fill=Survived==1))+geom_bar(position="fill")+ggtitle("Survival rate by Family Size(categorical)")+ylab("Prob of survival")

# deriving variable title from name
nam=dat$Name
head(nam)
title=gsub("(.*,)|(\\..*)","",nam)
head(title)
unique(title)
table(title)
dat=data.frame(dat,title)

dat$title=gsub("Mme","Mrs",dat$title)
dat$title=gsub("Mlle","Miss",dat$title)
dat$title=gsub("Ms","Miss",dat$title)
dat$title=gsub("Lady","hon",dat$title)
dat$title=gsub("Sir","hon",dat$title)
dat$title=gsub("Don","hon",dat$title)
dat$title=gsub("Jonkheer","hon",dat$title)
dat$title=gsub("the Countess","hon",dat$title)
dat$title=gsub("Capt","hon",dat$title)
dat$title=gsub("Col","hon",dat$title)
dat$title=gsub("Major","hon",dat$title)
dat$title=gsub("Dr","hon",dat$title)

ggplot(dat,aes(x=title,fill=Survived==1))+geom_bar(position = "fill")+ggtitle("Survival rate by Title")+ylab("Prob of survival")



# missing value imputation for variable 'embarked' by subtituting for most common port of embarkation
index=which(is.na(dat$Embarked))
index
dat$Embarked[index]="S"

dat$Survived=as.factor(dat$Survived)
dat$Pclass=as.factor(dat$Pclass)


# Survival modeling on tarining dataset with decision tree algorithm

modd=rpart(Survived~.,data = dat[,-c(1, 4, 7, 8, 9, 11)],method="class")
printcp(modd)
modd1=prune(modd,cp=0.01)
prp(modd1)
modd1


# reading the test dataset 

test=read.csv("../input/test.csv")

str(test)
test$Pclass=as.factor(test$Pclass)

# creating variable familysize in test dataset
test$familySize=test$SibSp+test$Parch
test$familySize=ifelse(test$familySize>6,"large",ifelse(test$familySize>3,"medium",ifelse(test$familySize>0,"small","single")))
colSums(is.na(test))

# missing value imputation for fare
ind=which(is.na(test$Fare))
test$Pclass[ind]
aggregate(Fare~Pclass,test,mean)
test$Fare[ind]=12.4597


# creating variable title in test dataset
nam1=test$Name
head(nam1)
title=gsub("(.*,)|(\\..*)","",nam1)
head(title)
unique(title)
table(title)
test=data.frame(test,title)
unique(test$title)
test$title=gsub("Mme","Mrs",test$title)
test$title=gsub("Mlle","Miss",test$title)
test$title=gsub("Ms","Miss",test$title)
test$title=gsub("Lady","hon",test$title)
test$title=gsub("Sir","hon",test$title)
test$title=gsub("Dona","hon",test$title)
test$title=gsub("Jonkheer","hon",test$title)
test$title=gsub("the Countess","hon",test$title)
test$title=gsub("Capt","hon",test$title)
test$title=gsub("Col","hon",test$title)
test$title=gsub("Major","hon",test$title)
test$title=gsub("Dr","hon",test$title)


#predicting survival using the decision tree model
Survived=predict(modd1,type = "class",newdata = test)
Survived
PassengerId=test$PassengerId
result=data.frame(PassengerId,Survived)

write.csv(result,"Submission_Fin.csv",row.names = F)

