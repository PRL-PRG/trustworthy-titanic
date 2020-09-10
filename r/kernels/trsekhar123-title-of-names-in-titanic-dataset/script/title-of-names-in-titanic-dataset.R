library(dplyr)
library(caret)

t<-list.files(path = "../input")
t
titanic<-read.csv("../input/train.csv")# read the csv training dataset
summary(titanic)#get the summary of dataset

tit<-as.vector(regmatches(x = titanic$Name,gregexpr("([A-Za-z]+\\w\\.)+",text = titanic$Name))) #extract the titles of the Names
tit<-unlist(tit, use.names=FALSE)#unlist the list to a vector

titanic$title<-as.data.frame(tit,stringsAsFactors = TRUE)#create a new column for title and append the column

summary(titanic$title)




