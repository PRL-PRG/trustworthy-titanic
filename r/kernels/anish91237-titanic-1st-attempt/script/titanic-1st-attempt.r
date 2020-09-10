
# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

import os
print(os.listdir("../input"))

# Any results you write to the current directory are saved as output.

test <- read.csv("../input/test.csv")
train <- read.csv("../input/train.csv")

str(train)
summary(train)
table(train$Survived)
prop.table(table(train$Survived))
test$Survived<-rep(0,418)
prop.table(table(train$Sex,train$Survived),1)
table(train$Sex,train$Survived)
train$Child<-0
train$Child[train$Age<18]<-1
aggregate(Survived~Child+Sex,data = train,FUN = sum)
aggregate(Survived~Child+Sex,data = train,FUN = function(x){sum(x)/length(x)})
aggregate(Survived~Pclass+Sex,data = train,FUN = function(x){sum(x)/length(x)})
train$Farecategory <- '30+'
train$Farecategory[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Farecategory[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Farecategory[train$Fare < 10] <- '<10'
aggregate(Survived~Farecategory+Pclass+Sex,data = train,FUN = function(x){sum(x)/length(x)})
library(rpart)
#class is used to get categorical variables while anova is used for continuous variable 
fit<-rpart(Survived~Pclass+Sex+Age+Fare+SibSp+Parch+Embarked,
           data = train,
           method = 'class')
plot(fit)
text(fit)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "titanicdtree.csv", row.names = FALSE)




