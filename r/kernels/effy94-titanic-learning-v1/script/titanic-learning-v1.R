## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library('dplyr')
library('knitr')
library('ggplot2')
library('randomForest')

# Load the data
titanic.train<-read.csv("../input/train.csv", stringsAsFactors = F, na.strings=c(""," ","NA"))
titanic.test<-read.csv("../input/test.csv", stringsAsFactors = F,na.strings=c(""," ","NA"))
Survived<-rep(0,nrow(titanic.test))
titanic.test<-cbind(titanic.test[1],Survived,titanic.test[,2:11])
titanic.data<-rbind(titanic.train,titanic.test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(titanic.train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
colnames(titanic.data[colSums(is.na(titanic.data))>0])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
embark.miss.index<-which(is.na(titanic.data$Embarked))
data1<-titanic.data[-embark.miss.index,]
fare.miss.index<-which(is.na(data1$Fare))
data1<-data1[-fare.miss.index,]
ggplot(data = data1, mapping = aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fare.miss.index<-which(is.na(titanic.data$Fare))
# Embarked place for missing
fare.miss.embark<-titanic.data$Embarked[fare.miss.index]
fare.miss.class<-titanic.data$Pclass[fare.miss.index]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.data$Fare[fare.miss.index]<-mean(titanic.data$Fare[which(titanic.data$Pclass=='3' & titanic.data$Embarked == 'S')], na.rm = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fare1<-titanic.data$Fare[which(is.na(titanic.data$Embarked))[1]]
fare2<-titanic.data$Fare[which(is.na(titanic.data$Embarked))[2]]
class1<-titanic.data$Pclass[which(is.na(titanic.data$Embarked))[1]]
class2<-titanic.data$Pclass[which(is.na(titanic.data$Embarked))[2]]
embark.index<-which(is.na(titanic.data$Embarked))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.data$Embarked[embark.index[1]]<-'C'
titanic.data$Embarked[embark.index[2]]<-'C'                      


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extract the title
titanic.data$Title<-sapply(titanic.data$Name, function(x){trimws(strsplit(x,split = '[,.]')[[1]][2])})
table(titanic.data$Title, titanic.data$Sex)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Assign the french speaking title and "Ms." to according groups
rare.status<-c('Capt','Col','Don','Dona','Dr','Jonkheer','Lady','Major','Rev','Sir','the Countess')
titanic.data$Title[titanic.data$Title=='Mlle']<-'Miss'
titanic.data$Title[titanic.data$Title=='Mme']<-'Mrs'
titanic.data$Title[titanic.data$Title=='Ms']<-'Miss'
titanic.data$Title[titanic.data$Title %in% rare.status]<-"Noble"
table(titanic.data$Title, titanic.data$Sex)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = titanic.data[1:891,], mapping = aes (x = Title, fill = factor(Survived)))+
  geom_bar(position = 'stack')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mean.age.group<-aggregate(Age ~ Title, titanic.data, median)
ggplot(data = titanic.data, mapping = aes(x = Title, y = Age, fill = Title)) + geom_boxplot()+
  stat_summary(fun.y=median, colour="darkred", geom="point", shape=18, size=3,show_guide = FALSE) + 
  geom_text(data = mean.age.group, aes(label = Age, y = Age + 4))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.data$Age[which(titanic.data$Title=='Master')]<-4
titanic.data$Age[which(titanic.data$Title=='Miss')]<-22
titanic.data$Age[which(titanic.data$Title=='Mr')]<-29
titanic.data$Age[which(titanic.data$Title=='Mrs')]<-35
titanic.data$Age[which(titanic.data$Title=='Noble')]<-47.5


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.data$surname<-sapply(titanic.data$Name, function(x){trimws(strsplit(x,split = '[,.]')[[1]][1])})
titanic.data$size<-titanic.data$SibSp+titanic.data$Parch+1
titanic.data$faminfo<-paste(titanic.data$size,titanic.data$surname, sep="_")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = titanic.data[1:891,], mapping = aes(x = size, fill = factor(Survived)))+
  scale_x_continuous(breaks=c(1:11))+
  geom_bar(position = 'stack')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.data$sizeD[titanic.data$size == 1]<-'small'
titanic.data$sizeD[titanic.data$size > 1 & titanic.data$size < 5]<-'medium'
titanic.data$sizeD[titanic.data$size > 4]<-'large'
mosaicplot(table(titanic.data[1:891,]$sizeD, titanic.data[1:891,]$Survived), main='Family Size by Survival', shade=TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.data$child[titanic.data$Age < 18]<-1
titanic.data$child[titanic.data$Age >= 18]<-0
titanic.data$child<-as.factor(titanic.data$child)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)

titanic.data<-transform(titanic.data, Survived = as.factor(Survived),
                                        Sex = as.factor(Sex),
                                        Embarked = as.factor(Embarked),
                                        Title = as.factor(Title),
                                        sizeD = as.factor(sizeD))
titanic.train<-titanic.data[1:891,]
titanic.test<-titanic.data[892:nrow(titanic.data),]
rf.model<-randomForest(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+sizeD+child,data = titanic.train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(rf.model)
legend('topright', colnames(rf.model$err.rate), col=1:3, fill=1:3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
varImpPlot(rf.model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
importance <- importance(rf.model)
varImportance<-data.frame(Variable = rownames(importance),Importance = round(importance[,'MeanDecreaseGini'], digits = 2))
ggplot(varImportance, aes(x = reorder(Variable, Importance), 
    y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  labs(x = 'Variables') +
  coord_flip() 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.predit<-predict(rf.model,titanic.test)
result<-data.frame(PassengerId = titanic.test$PassengerId, Survived = titanic.predit, row.names = titanic.test$PassengerId)

