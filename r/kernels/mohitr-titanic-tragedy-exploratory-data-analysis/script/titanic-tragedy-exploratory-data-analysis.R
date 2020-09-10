## ---- message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(knitr)
library(kableExtra)

titanic_train<-read.csv('../input/train.csv')
titanic_test<-read.csv('../input/test.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_test$Survived <- NA
titanic <- rbind(titanic_train, titanic_test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(titanic) %>% kable("html") %>%
  kable_styling()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(titanic) #1309 obervations & 12 variables


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(titanic) #Survived, Age & Fare have NA values


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
outlierKD <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1, "n")
  cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
  cat("Mean of the outliers:", round(mo, 2), "n")
  m2 <- mean(var_name, na.rm = T)
  cat("Mean without removing outliers:", round(m1, 2), "n")
  cat("Mean if we remove outliers:", round(m2, 2), "n")
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
outlierKD(titanic_train,Fare)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
outlierKD(titanic_train,SibSp)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
outlierKD(titanic_train,Parch)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
checkNA <- function(x){sum(is.na(x))/length(x)*100} 
sapply(titanic,checkNA)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sapply(titanic_train,checkNA)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
checkMissing <- function(x){sum(x=="")/length(x)*100}
sapply(titanic,checkMissing)


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#1. Age: Replacing NA values in Age with mean 
#titanic[is.na(titanic$Age),6] <- mean(titanic$Age)
titanic$Age[is.na(titanic$Age)] <- round(mean(titanic$Age, na.rm = TRUE))
titanic_train$Age[is.na(titanic_train$Age)] <- round(mean(titanic_train$Age, na.rm = TRUE))

#2. Embarked: Replacing Empty Embarked with most common value 'S'
titanic_train$Embarked <- replace(titanic_train$Embarked, which(titanic_train$Embarked==""), 'S')

#3. Cabin: Not replacing with anything as Cabin values are unique


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Title <-  gsub("^.*, (.*?)\\..*$", "\\1", titanic_train$Name)
titanic_train$Title <- as.factor(Title)
table(Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic_train$FamilyCount <-titanic_train$SibSp + titanic_train$Parch + 1 
titanic_train$FamilySize[titanic_train$FamilyCount == 1] <- 'Single' 
titanic_train$FamilySize[titanic_train$FamilyCount < 5 & titanic_train$FamilyCount >= 2] <- 'Small' 
titanic_train$FamilySize[titanic_train$FamilyCount >= 5] <- 'Big' 
titanic_train$FamilySize=as.factor(titanic_train$FamilySize)
table(titanic_train$FamilySize)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1.Changing names of few categorical variables for interpretability 
titanic_train$Survived <- ifelse(titanic_train$Survived==1,"Yes","No")
titanic_train$Survived <- as.factor(titanic_train$Survived)

titanic_train$Embarked <- ifelse(titanic_train$Embarked=="S","Southampton", 
                              ifelse(titanic_train$Embarked=="C","Cherbourg", "Queenstown"))
titanic_train$Embarked <- as.factor(titanic_train$Embarked)

# 2.Converting categorical variables from int to factor
# i) Pclass
titanic_train$Pclass <- as.factor(titanic_train$Pclass)

# ii) SibSp
titanic_train$SibSp <- as.factor(titanic_train$SibSp)

# iii) Parch
titanic_train$Parch <- as.factor(titanic_train$Parch)



## ---- results=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
temp<-subset(titanic_train,titanic_train$Survived=="Yes")
(nrow(temp)/nrow(titanic_train))*100

## ---- echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train,aes(x=as.factor(Survived))) + geom_histogram(stat="count") + geom_text(stat='count',aes(label=..count..),vjust=-.2) + ylab("Number of Passengers") + xlab("Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(titanic$Age)
d <- density(titanic$Age)
plot(d,main="Passenger Age Distribution",xlab="Age",ylab="Frequency",col="blue")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train, aes(x=Sex,fill=Survived))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
ylab("Number of Passengers")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train) + geom_freqpoly(mapping = aes(x = Age, color = Survived), binwidth = 2.5) +
ylab("Frequency")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train) + geom_freqpoly(mapping = aes(x = Fare, color = Survived), binwidth = 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train, aes(x=Pclass,fill=Survived))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
ylab("Number of Passengers") + xlab("Passenger Class")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train, aes(x=Embarked,fill=Survived))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
ylab("Number of Passengers")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train, aes(x=SibSp,fill=Pclass))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
ylab("Number of Passengers") + xlab("Number of Siblings")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train, aes(x=SibSp,fill=Survived))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
ylab("Number of Passengers")+xlab("Number of Siblings/Spouse")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train, aes(x=Parch,fill=Pclass))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
ylab("Number of Passengers") + xlab("Number of Parents/Children")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train, aes(x=Parch,fill=Survived))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
ylab("Number of Passengers") + xlab("Number of Parents/Children")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train, aes(x=Title,fill=Survived))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
ylab("Number of Passengers") + xlab("Passenger Name Title") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic_train, aes(x=FamilySize,fill=Survived))+ geom_bar(position = "dodge") + geom_text(stat='count',aes(label=..count..),position = position_dodge(0.9),vjust=-0.2) +
ylab("Number of Passengers") + xlab("Family Size")

