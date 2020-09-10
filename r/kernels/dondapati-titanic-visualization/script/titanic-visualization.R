# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(plyr)
library(tidyverse)
library(data.table)
library(magrittr)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input")

# Any results you write to the current directory are saved as output.
titanic_train <- read_csv('../input/train.csv')
titanic_test <- read_csv('../input/test.csv')
str(titanic_train)


ggplot(titanic_train,mapping = aes(x = Age,y = Sex))+
  geom_jitter(aes(colour = factor(Survived)))+
  facet_wrap(~Pclass)+
  theme(legend.title = element_blank())+
  scale_fill_discrete(name = "Survived")+scale_x_continuous(name="Age",limits=c(0, 81))+
  labs(x = "Age", y = "Sex", title = "Pclass vs Sex vs Age vs Survived")


ggplot(data = titanic_train,mapping = aes(Fare,Pclass))+
  geom_jitter(aes(colour  = factor(Survived)))+
  scale_x_continuous(name = "Fare",limits = c(0,270),breaks = c(0,20,40,60,80,100,120,140,160,180,200,220,240,260,280))+
  scale_fill_discrete(name = "Survived")+
  facet_grid(~Sex)+
  theme(legend.title = element_blank())+
  labs(x = "Fare",y = "Pclass",title = "Fare Vs. Pclass for Genderwise")


ggplot(data = titanic_train,mapping = aes(x = Age,y = Pclass))+
  geom_jitter(aes(colour = factor(Survived)))+
  theme(legend.title = element_blank())+
  scale_fill_discrete(name = "Survived")+
  facet_grid(~Sex)+
  labs(x = "Age",y = "Pclass" ,title = "Age Vs. Pclass for Genderwise")
  
ggplot(data = titanic_train,mapping = aes(x = factor(Embarked),y = Pclass))+
  geom_jitter(aes(colour = factor(Survived)))+
  facet_grid(~Sex)+
  theme(legend.title = element_blank())+
  labs(x = "Eambarked", y ="Pclass",title = "Eambarked Vs. Pclss Per Genderwise")


# Here missing values in Embarked column so Doing the some additional analysis.
# Finding the missing Values in Embarked column

titanic_train[which(!complete.cases(titanic_train$Embarked)),]

 # By observing the above the  missing is following the some pattern  gender is Female and Pclass is '1'
# and Survived is '1' so based on the previous graphs i observe that 
# Replacing the missing valus with. 



  
ggplot(data=subset(titanic_train,Sex == "female"& Pclass == 1 & Survived == 1),mapping = aes(x = factor(Embarked),y= factor(Pclass)))+
  geom_jitter(aes(colour = factor(Survived)))+
  theme(legend.title = element_blank())+
  labs(x = "Emabarked" ,y = "Pclass")

# Based on the above graph replacing the NA values with "S"
titanic_train[c(62,830),'Embarked'] <- "S"

# After Replacing the values the updated Graph 
ggplot(data = titanic_train,mapping = aes(x = factor(Embarked),y = Pclass))+
  geom_jitter(aes(colour = factor(Survived)))+
  facet_grid(~Sex)+
  theme(legend.title = element_blank())+
  labs(x = "Eambarked", y ="Pclass",title = "Updated Eambarked Vs. Pclss Per Genderwise")


# View(titanic_train)
# Cleaning The Cabin Column and Making bargraph to understanding the missing values.
titanic_train$Cabin <- substr(titanic_train$Cabin,start = 1,stop = 1)

ggplot(data = titanic_train,mapping = aes(x = Cabin,y = ..count..,fill = Cabin))+
  geom_bar(stat = "count",position = "dodge")+
  facet_grid(Sex~Survived)+
  geom_text(stat = "count",aes(label = ..count..),vjust = -0.5)+
  labs(x = "Cabin",y = "Frequency",title = "Cabin Vs.Gender")
  
# Data cleaning and mauplation

Mis_Var <- sapply(titanic_train[,-1],function(x) round(prop.table(table(is.na(x)))*100,2))%>% 
  ldply(.,data.frame) %>% 
  set_colnames(c('Variable','Is.Missing','Freq'))%>%
  mutate(Is.Missing  = ifelse(Is.Missing =='TRUE','Yes','No'))%>%
  ggplot(.,mapping = aes(x = Is.Missing,y = Freq,colour = Variable,,fill = Variable))+
  geom_histogram(stat = 'identity')+facet_grid(~Variable)+labs(title = 'Missing variable Percentage')+
  geom_text(aes(label = Freq),position = position_dodge(width = 1),vjust = -.5)

Mis_Var

# From the graph observe that 'Age,Cabin,Embarked' columns are having the missing values.
table(complete.cases(titanic_train$Age))
# Here total 177 missing values in Age column

age.na <- titanic_train[is.na(titanic_train$Age),]
age.non.na <- titanic_train[!is.na(titanic_train$Age),]

# first finding any  pattern is following in the age.na table if any thing was find out then identify the 
# perticular pattern in non missing table. Then replace the missing values in a pattern characterstics.

# So for that plot the variables and understanding missing pattern.

ggplot(data = age.na,mapping = aes(x = factor(Embarked),y = Pclass))+
  geom_jitter(aes(colour = factor(Sex)))+
  facet_grid(~Survived)+
  theme(legend.title = element_blank())+
  labs(x = "Eambarked", y ="Pclass",title = "age.na table Eambarked Vs. Pclss Per Genderwise")

# from the graph observe that the combination of
# Pclass is 2,3 and Survived is o and Embarked is (S,C) of data is more
# Missing so finding same combination in age.non.na table   
# and replace mean of the Age variable.
# here SEX column is having the  different age distribution so based on that replace the 
# Missing age column.

ggplot(data = age.non.na,mapping = aes(x = factor(Embarked),y = Pclass))+
  geom_jitter(aes(colour = factor(Sex)))+
  facet_grid(~Survived)+
  theme(legend.title = element_blank())+
  labs(x = "Eambarked", y ="Pclass",title = "age.non.na table Eambarked Vs. Pclss Per Genderwise")


# For understanding the age column
ww <- titanic_train%>%select(Age,Sex)%>%group_by(Sex)%>%summarise(Age.mean=mean(Age,na.rm = T))
ggplot(data = titanic_train,mapping = aes(Age,colour = Sex))+geom_density(position = "stack")+
  geom_vline(data = ww,aes(xintercept=Age.mean,color=Sex), linetype="dashed", size=1)




# converting into data.table and Replacing missing Age column with mean value.
titanic_train_dt <- data.table(titanic_train)
titanic_train_dt[is.na(titanic_train_dt$Age)&Sex =='female','Age'] <- 
  titanic_train_dt[Pclass %in% c(2,3) & Survived ==0 & Embarked %in%c('S','C')&Sex =='female',round(mean(Age,na.rm = T))]
titanic_train_dt[is.na(titanic_train_dt$Age)&Sex =='male','Age'] <- 
  titanic_train_dt[Pclass %in% c(2,3) & Survived ==0 & Embarked %in%c('S','C')&Sex =='male',round(mean(Age,na.rm = T))]


