# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

#import numpy as np # linear algebra
#import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#from subprocess import check_output
#print(check_output(["ls", "../input"]).decode("utf8"))

# Any results you write to the current directory are saved as output.
library(randomForest)
library(dplyr)

##loading data and check data
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
test$Survived <- NA
full <- rbind(train, test)



####feature engineer
#1.sex
prop.table(table(train$Sex, train$Survived),1)

#2.Title
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Title)

rare <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
          'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare]  <- 'Rare'
table(full$Title)

#Embarked
full$Embarked[full$Embarked==''] = 'S'

#Family size
full$Fsize <- full$SibSp + full$Parch + 1
full$FsizeD[full$Fsize == 1] <- 'Single'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'Small'
full$FsizeD[full$Fsize > 4] <- 'Big'


full$Pclass = factor(full$Pclass)
full$Sex = factor(full$Sex)
full$Embarked = factor(full$Embarked)
full$Title = factor(full$Title)
full$FsizeD = factor(full$FsizeD)



train1 <- full[1:891,]
test1 <- full[892:1309,]

set.seed(1234)
rf <- randomForest(factor(Survived)~ Pclass + Sex + Embarked + Title + FsizeD, data = train1, ntree = 100)
varImpPlot(rf)
plot(rf)

prediction <- predict(rf, test1)
result <- data.frame(PassengerID = test1$PassengerId, Survived = prediction)

write.csv(result, file="Titanic.csv", row.names = F)