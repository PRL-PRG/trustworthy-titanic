# Preparation

library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(mice)
library(randomForest)
library(caret)

titanictrain <- read.csv("../input/train.csv", header = TRUE,  stringsAsFactors = FALSE)
titanictest <- read.csv("../input/test.csv", header = TRUE,  stringsAsFactors = FALSE)


titanic=bind_rows(titanictrain,titanictest)  # combining train and test datasets


# Feature Engineering I

titanic$Surname=gsub('(,.*)','',titanic$Name)               # New feature Surname

titanic$Title=gsub('(.*, )|(\\..*)','',titanic$Name)        # New feature Title
table(titanic$Title)

titanic[titanic$Title %in% c("Ms","Mlle"),]$Title="Miss"    # Combining titles belonging to same class
table(titanic$Title)

titanic[!(titanic$Title %in% c("Miss","Mr","Mrs","Master")),]$Title="Special"  # Combining unique titles into one class
table(titanic$Title)

titanic$FamilySize=titanic$SibSp+titanic$Parch+1            # New feature Family Size

factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Survived')

titanic[factor_vars] <- lapply(titanic[factor_vars], function(x) as.factor(x))

str(titanic)


# Dealing with Missing Values

summary(titanic)

# Count of missing values
# Fare - 1
# Embarked - 2
# Age  - 263

#   Missing Fare

sum(is.na(titanic$Fare))
pcl<-titanic[which(is.na(titanic$Fare)),]$Pclass
pcl           # Missing Fare is of Pclass 3
      
fillfare<-mean(titanic[!is.na(titanic$Fare) & titanic$Pclass==pcl,]$Fare)
fillfare      # Filling missing Fare with mean of Fare of Travellers of Pclass 3

titanic[is.na(titanic$Fare),]$Fare<-fillfare  # Mean = 13.30289
sum(is.na(titanic$Fare))


#   Missing Embarked

sum(is.na(titanic$Embarked))
titanic[is.na(titanic$Embarked),]
titanic[is.na(titanic$Embarked),]$Fare   #  Both missing Embarked values had Fare 80

#   Plotting a Boxplot Fare corresponding Embarked to get the Embarking variable most probable to get Fare 80

plot=ggplot(titanic[!is.na(titanic$Embarked),], aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
     geom_boxplot() +
     geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2) +
     theme_few()

titanic[is.na(titanic$Embarked),]$Embarked
sum(is.na(titanic$Embarked))

# Missing Age using mice package

sum(is.na(titanic$Age))
md.pattern(titanic)

# Better visualisation of missing values
library(VIM)
aggr_plot=aggr(titanic, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

temp_titanic=mice(titanic[,!names(titanic) %in% c('PassengerId','Name','Ticket','Cabin','FamilySize','Surname','Survived')],method = "rf",m=5,maxit = 50,seed=502)
complete_titanic=complete(temp_titanic)

# To see if the imputed values has the same distribution as the original
densityplot(temp_titanic)
par(mfrow=c(1,2))
hist(titanic$Age,freq=F,col="Red")
hist(complete_titanic$Age,freq=F,col="Blue")

titanic$Age=complete_titanic$Age
sum(is.na(titanic$Age))

# Feature Engineering II

# Creating new features from the imputed missing values 

titanic$Child[titanic$Age>=18]=0      
titanic$Child[titanic$Age<18]=1
titanic$Child=as.factor(titanic$Child)   # New feature Child which indicate whether or not is the person a child

# Prediction

# Split the data into train set and a test set

train=titanic[1:891,]
test=titanic[-(1:891),]

# Logistic Regression

glm.fit=glm(factor(Survived)~Pclass+Sex+Age+Embarked+SibSp+Parch+Fare+FamilySize+Child+Title,train,family = binomial,maxit=100)
summary(glm.fit)
pred=predict(glm.fit,test,type = "response")
glm.pred=rep(0,418)
glm.pred[pred>0.5]=1
glm.solution <- data.frame(PassengerID = test$PassengerId, Survived = glm.pred)
glm.solution
write.csv(file = "GLMSolution.csv",glm.solution,row.names = F)

# Random Forest

rf.fit=randomForest(factor(Survived)~Pclass+Sex+Age+Embarked+SibSp+Parch+Fare+FamilySize+Child+Title,train,importance=TRUE)
summary(rf.fit)
rf.pred=predict(rf.fit,test)
rf.solution <- data.frame(PassengerID = test$PassengerId, Survived = rf.pred)
rf.solution
write.csv(file = "RFSolution.csv",rf.solution,row.names = F)

# Prediction using CARET Package

# Split the data into train set and a test set

tran=titanic[1:891,]
test=titanic[-(1:891),]


fit.control=trainControl(method = "repeatedcv",number=5,repeats = 5)

# Logistic Regression

glm.fit=train(y=tran$Survived,x=tran[,c("Pclass","Sex","Age","Fare","FamilySize","Title","Embarked","SibSp","Parch","Child")],method="glm",trControl=fit.control)
print(glm.fit)

glm.pred=predict.train(glm.fit,test)
glm.solution <- data.frame(PassengerID = test$PassengerId, Survived = glm.pred)
glm.solution
write.csv(file = "GLMSolutionCaret.csv",glm.solution,row.names = F)


# Random Forest

rf.fit=train(y=tran$Survived,x=tran[,c("Pclass","Sex","Age","Fare","FamilySize","Title","Embarked","SibSp","Parch","Child")],method="rf",trControl=fit.control)
print(rf.fit)

rf.pred=predict.train(rf.fit,test)
rf.solution <- data.frame(PassengerID = test$PassengerId, Survived = rf.pred)
rf.solution
write.csv(file = "RFSolutionCaret.csv",rf.solution,row.names = F)
