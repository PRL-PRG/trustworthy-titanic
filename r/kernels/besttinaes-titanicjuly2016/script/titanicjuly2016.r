

# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train.titanic <- read.csv("../input/train.csv")
test.titanic  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
names(test.titanic)
names(train.titanic)
dim(train.titanic)
str(test.titanic)
str(train.titanic)
train.titanic$Pclass<-as.factor(train.titanic$Pclass)
#redefine value class
train.titanic$Survived<-factor(train.titanic$Survived)
train.titanic$Pclass<-factor(train.titanic$Pclass)
test.titanic$Pclass<-factor(test.titanic$Pclass)
#compare train and text data
summary(train.titanic$Pclass)/length(train.titanic$Pclass)
summary(test.titanic$Pclass)/length(test.titanic$Pclass)
summary(train.titanic$Sex)/length(train.titanic$Sex)
summary(train.titanic$Age)
summary(test.titanic$Age)
summary(train.titanic$Fare)
summary(test.titanic$Fare)






par(mfrow=c(2,2))
barplot(table(train.titanic$Survived,train.titanic$Pclass),xlab="Class",ylab="counts",col=c("darkblue","green"),legend=c("Died","Survived"))
barplot(table(train.titanic$Survived,train.titanic$Sex),xlab="Gender",,ylab="counts",col=c("darkblue","green"),legend=c("Died","Survived"))
hist(train.titanic$Age,xlab="Age", main="")
hist(train.titanic$Age[which(train.titanic$Survived==1)],xlab="Age", main="", col="green",add=T)
hist(train.titanic$Fare,xlab="Fare",main="",breaks=50)
hist(train.titanic$Fare[which(train.titanic$Survived==1)],xlab="Fare",main="",breaks=50,col="green",add=T)

library("party")
train.titanic$Embarked[train.titanic$Embarked==""] = NA
train.titanic$Embarked = droplevels(train.titanic$Embarked)
str(train.titanic$Embarked)
model<-ctree(Survived ~Pclass+Sex+Age+Fare+Embarked+SibSp,data=train.titanic)
model
summary(model)
plot(model, type="extended")


pred<-predict(model,test.titanic)
summary(pred)
