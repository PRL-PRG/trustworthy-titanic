
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

titanic_test<- read.delim("../input/test.csv", header = T, sep = ",", stringsAsFactors = T)
str(titanic_test)
summary(titanic_test)
head(titanic_test)

titanic_train<- read.delim("../input/train.csv", header = T, sep = ",", stringsAsFactors = T)
str(titanic_train)
summary(titanic_train)
head(titanic_train)

titanic_gender<- read.delim("../input/gender_submission.csv", header = T, sep = ",", stringsAsFactors = T)
head(titanic_gender)

library(tidyr)

#identical(titanic_gender$PassengerId, titanic_test$PassengerId)
#titanic_merge<- merge(titanic_test, titanic_gender, all.x = T)
#summary(titanic_merge)
#head(titanic_merge)
library(dplyr)


###--- DATA CLEANING ---###
lapply(titanic_train, function(x) length(unique(x)))

#removing columns which are completely unique, not needed for the EDA
titanic_train<- titanic_train[,-which(lapply(titanic_train, function(x) length(unique(x))) == 891)]

#checking for NA and fixing
sum(is.na(titanic_train))
sapply(titanic_train, function (x) sum(is.na(x)))
#only Age column has NAs
summary(titanic_train$Age) #cannot assume to convert all NAs to either 0 or median of the Age values at this point

#checking for NaN and fixing
sapply(titanic_train, function (x) sum(is.nan(x)))

#checking for NULL and fixing
sapply(titanic_train, function (x) sum(is.null(x)))


#checking for standards of each of the columns
sapply(titanic_train, class)
titanic_train$Age<- as.integer(titanic_train$Age) #converting Age column to integer as we had age in decimals

#filtering only the survived passengers
titanic_train_survived<- titanic_train[which(titanic_train$Survived == 1),]
str(titanic_train_survived)
summary(titanic_train_survived)
head(titanic_train_survived)

#deriving young vs adult
titanic_train_survived$young<- ifelse(titanic_train_survived$Age <= 18, 1, 0)

#removing unwanted rows and columns for this EDA
titanic_train_survived<- titanic_train_survived[,-c(1, 7:9)] #removing survived, tickets, fare and cabin  columns
sum(is.na(titanic_train_survived$Age))
titanic_train_survived<- titanic_train_survived[-which(is.na(titanic_train_survived$Age)),]



###--- DATA ANALYSIS ---###
#pclass
plot(table(titanic_train_survived$Pclass), main = "survivors / pclass", xlab = "pclass", ylab = "count", col = "green")

#sex
barplot(table(titanic_train_survived$Sex), main = "male and female survivors proposition", ylab = "count", col = c("red", "blue"), ylim = c(0, 220))

#age
hist(titanic_train_survived$Age, main = "survivor age and proposition", xlab = "age range", labels = T, col = "darkmagenta")
boxplot(titanic_train_survived$Age)
titanic_train_survived<- titanic_train_survived[-which(titanic_train_survived$Age > 60),]

#embarked
barplot(table(titanic_train_survived$Embarked), main = "survivors proposition w.r.t embarkment", ylab = "count")
subset(titanic_train_survived, Pclass == 1 & Sex == "female" & SibSp == 0 & Parch == 0 & young == 0)
summary(subset(titanic_train_survived, Pclass == 1 & Sex == "female" & SibSp == 0 & Parch == 0 & young == 0))
#converting 2 blank embarked points to C, since C has more freq w.r.t to the age > 30
summary(subset(titanic_train_survived, Pclass == 1 & Sex == "female" & SibSp == 0 & Parch == 0 & young == 0 & Age > 30))
titanic_train_survived[which(titanic_train_survived$Embarked == ""), 6]<- "C"

summary(titanic_train_survived)
barplot(table(titanic_train_survived$Embarked), main = "survivors proposition w.r.t embarkment", ylab = "count", col = c("red", "blue", "green"))

#young vs adult
barplot(table(titanic_train_survived$young))
barplot(table(titanic_train_survived$young), main = "adult vs young survivors proposition", ylab = "count", col = c("red", "blue"), ylim = c(0, 250), names.arg = c("adult", "young"))


##segmented univariate analysis
#by sex
titanic_train_survived_male<- titanic_train_survived[titanic_train_survived$Sex == "male",]
titanic_train_survived_female<- titanic_train_survived[titanic_train_survived$Sex == "female",]
barplot(table(titanic_train_survived_male$Pclass), ylim = c(0,50)) #Pclass1 and 3 have almost equal number of male survivors
barplot(table(titanic_train_survived_female$Pclass),  ylim = c(0,100)) #Pclass1 has the most number of female survivors, followed by Pclass 2 and 3
hist(titanic_train_survived_male$Age, labels = T) #Age between 20-40 has the more male survivors
hist(titanic_train_survived_female$Age,labels = T, breaks = 8) #Age between 20-40 has the more female survivors

#by pclass
titanic_train_survived_class1<- titanic_train_survived[titanic_train_survived$Pclass == 1,]
titanic_train_survived_class2<- titanic_train_survived[titanic_train_survived$Pclass == 2,]
titanic_train_survived_class3<- titanic_train_survived[titanic_train_survived$Pclass == 3,]
barplot(table(titanic_train_survived_class1$Sex)) #female had more chance of being a survivor in Pclass1
barplot(table(titanic_train_survived_class2$Sex)) #female had more chance of being a survivor in Pclass1
barplot(table(titanic_train_survived_class3$Sex)) #female had more chance of being a survivor in Pclass1
hist(titanic_train_survived_class1$Age, labels = T) #Age between 20-40 has the more male and female survivors
hist(titanic_train_survived_class2$Age, labels = T) #Age between 20-40 has the more male and female survivors
hist(titanic_train_survived_class3$Age, labels = T) #Age between 0-30 has the more male and female survivors

#by young
titanic_train_survived_adult<- titanic_train_survived[titanic_train_survived$young == 0,]
titanic_train_survived_young<- titanic_train_survived[titanic_train_survived$young == 1,]
barplot(table(titanic_train_survived_adult$Pclass)) #adults in Pclass1 had the most chance of survival
barplot(table(titanic_train_survived_young$Pclass)) #youngs in Pclass3 had the most chance of survival, followed by Pclass2 and 1
barplot(table(titanic_train_survived_adult$Sex)) #adults female had the most chance of survival
barplot(table(titanic_train_survived_young$Sex)) #youngs female had the most chance of survival
hist(titanic_train_survived_adult$Age, labels = T) #adults 20-40 age had most chance of survival
hist(titanic_train_survived_young$Age, breaks = 6, labels = T) #youngs 0-5 age had most chance of survival


##Bivariate analysis
#Pclass and Sex
library(ggplot2)
ggplot(titanic_train_survived, aes(titanic_train_survived$Pclass, fill = titanic_train_survived$Sex))+geom_bar()

#Pclass and Age
ggplot(titanic_train_survived, aes(x = factor(titanic_train_survived$Pclass), y= titanic_train_survived$Age))+geom_boxplot()
ggplot(titanic_train_survived, aes(x = factor(titanic_train_survived$Pclass), y= titanic_train_survived$Age))+geom_boxplot()+theme_classic()

#Pclass and young
ggplot(titanic_train_survived, aes(x = factor(titanic_train_survived$Pclass), fill= titanic_train_survived$Embarked, col = factor(titanic_train_survived$young)))+geom_bar()

#Sex and Age
ggplot(titanic_train_survived, aes(x = factor(titanic_train_survived$Sex), y= titanic_train_survived$Age))+geom_boxplot()+theme_gray()
length(which(titanic_train_survived$Sex == "female"))
length(which(titanic_train_survived$Sex == "male"))

#Sex and young
ggplot(titanic_train_survived, aes(x = factor(titanic_train_survived$Sex), y= titanic_train_survived$Age, fill= factor(titanic_train_survived$young)))+geom_boxplot()+theme_gray()
ggplot(titanic_train_survived, aes(x = factor(titanic_train_survived$Sex), fill= factor(titanic_train_survived$young)))+geom_bar(position = "dodge")+theme_gray()
       
#inferrence on Ananlysis:
       #Embarkked == Southampton, passangers who had embarkked from Southampton had a higher chance of survival, could be cos of filling the front or forward places in the ship and easy to escape
       #sex == female had more chance of survival
       #Age >= 20 and <40 had a greater changce of survival
       #Pcalss == 1 had the most chance of survival
       #young == 0 (adult), adults had a greater chance of survival
       
#combination of all 5 (mentioned above) parameters have the best chance of the survival.
#combination of 3-4 parameters have the good chance of the survival.
#combination of 1-2 parameters have the least change of the survival


