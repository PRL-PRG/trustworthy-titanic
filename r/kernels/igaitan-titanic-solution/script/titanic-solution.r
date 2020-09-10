
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(randomForest)
library(e1071)
library(tree)
library(rpart)
library(caret)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

train_df <- read.csv('../input/train.csv')
test_df <- read.csv('../input/test.csv')
#View(train_df)
#View(test_df)
summary(train_df)
summary(test_df)

smp_size<-floor(.5*nrow(train_df))
set.seed(123)

train_ind<-sample(seq_len(nrow(train_df)),size=smp_size)
train_1<-train_df[train_ind, ]
test_1<-train_df[-train_ind, ]

#PassengerId arbitrary assigned, not correlated with Survival.
#Pclass, more people died in 3rd class.
#Name, not corrolated.
#Sex, more women survived.
#Age, more young people died.
#SibSp, not used firstly.
#Parch, not used firstly.
#Ticket, not correlated.
#Fare, could be correlated.
#Cabin, ???
#Embarked, more people that embarked in Cherbourg died.

ggplot(train_df, aes(Pclass,Age)) + geom_point(aes(color=Survived))

ggplot(train_df, aes(Sex,Age)) + geom_point(aes(color=Survived))

ggplot(train_df, aes(Ticket,Age)) + geom_point(aes(color=Survived))

ggplot(train_df, aes(Fare,Age)) + geom_point(aes(color=Survived))

train_1 <- subset(train_1, !is.na(Age))
ggplot(train_1, aes(Age,SibSp)) + geom_point(aes(color=Survived))

fol <- formula(Survived ~ Pclass + Sex + Age + Fare )
model <- rpart(fol, method="class", data=train_1)
print(model)

prediction <- predict(model,test_1,type = "class")
result=test_1[,2]==as.character(prediction)
accuracy=sum(result)/nrow(test_1)
accuracy

#Made confusion matrix
table(pred = prediction, true = test_1$Survived)

prediction_2 <- predict(model,test_df,type = "class")
submission <- data.frame(test_df[,1],prediction_2)
colnames(submission) <-  c("PassengerId","Survived")
submission

write.csv(submission ,file = "submission.csv",row.names=F)
