# Using Regression approach

trainds<-read.csv("../input/train.csv",header = T,na.strings = c(""))


library(Amelia)

# A quick check to find out if there are any null values
# as per the graph it is apparent cabin and age have null values

missmap(trainds,main="Missing Values vs observed")


head(trainds)

# I am trying to fill the Null data with some values. If the cabin is found to be significant will 
# add a value which is most common rather then any random value
trainds$Cabin[is.na(trainds$Cabin)] <-  "G6"
trainds$Age[is.na(trainds$Age)] <-mean(trainds$Age,na.rm=T)


# a check to see if all the null values are replaced
missmap(trainds,main="Missing Values vs observed")



set.seed(124)
names(trainds)

traindata<-trainds[sample(1:nrow(trainds),800,replace =TRUE),] 
head(traindata)

testdata<-trainds[-traindata$PassengerId,]

head(testdata)

nrow(traindata)
nrow(testdata)

names(traindata)

mylogit<-glm(Survived~Pclass+Sex+Age, data=traindata,family=binomial(link="logit"))


summary(mylogit)


# 1-pchisq(459.44,396)
1-pchisq(mylogit$deviance,mylogit$df.residual)

# created a prediction model

newdata1<-testdata[c("Pclass","Sex","Age")]
newdata1$results<-predict(mylogit,newdata=newdata1,type='response')


newdata1$results <- ifelse(newdata1$results > 0.5,1,0)


misClasificError <- mean(newdata1$results != testdata$Survived)
print(paste('Accuracy',1-misClasificError))




# using Decision-Tree approach

trainds<-read.csv("../input/train.csv",header = T,na.strings = c(""))


library(Amelia)
library(party)


# A quick check to find out if there are any null values
# as per the graph it is apparent cabin and age have null values

missmap(trainds,main="Missing Values vs observed")


head(trainds)

set.seed(124)

traindata<-trainds[sample(1:nrow(trainds),800,replace =TRUE),] 


testdata<-trainds[-traindata$PassengerId,]
actualresult<-testdata$Survived

# I am trying to fill the Null data with some values. If the cabin is found to be significant will 
# add a value which is most common rather then any random value
trainds$Cabin[is.na(trainds$Cabin)] <-"G6"
trainds$Age[is.na(trainds$Age)] <-mean(trainds$Age,na.rm=T)


titanic.tree<-ctree(Survived~Pclass+Sex+Age+SibSp+Parch+Cabin+Embarked,data=trainds,control=ctree_control(mincriterion=0.85))

plot(titanic.tree)

testdatam1<-testdata[,c("Pclass","Sex","Age","SibSp","Parch","Cabin","Embarked")]

predictedresults<-predict(titanic.tree,testdatam1)

predictedresults <- ifelse(predictedresults > 0.5,1,0)


misClasificError <- mean(predictedresults!= actualresult)
print(paste('Accuracy',1-misClasificError))


# removing some non-significant nodes as per regression and checking if there is any difference in the result
# ******************************************************************
titanic.tree<-ctree(Survived~Pclass+Sex+Age,data=trainds)


testdatam1<-testdata[,c("Pclass","Sex","Age")]

predictedresults<-predict(titanic.tree,testdatam1)

predictedresults <- ifelse(predictedresults > 0.5,1,0)


misClasificError <- mean(predictedresults!= actualresult)
print(paste('Accuracy',1-misClasificError))


# ******************************************************************


