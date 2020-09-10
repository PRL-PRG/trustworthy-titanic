
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.


library(caret)
library(reshape2)

test <- read.csv("../input/test.csv",header=TRUE)
train <- read.csv("../input/train.csv",header=TRUE)

##############################################################
# Check for missing values in our data set (empty or NA):
# The melt function takes data in wide format and stacks a set of columns into a single column of data
train.missingData.vector <- apply(train[, -2], 2, function(x) sum(is.na(x) | x==""))
#train.missingData.vector

train.missingData.column <- melt(train.missingData.vector)
#train.missingData.column

# cbind  show only missing data :::::::::::::
#cbind(row.names(train.missingData.column)[train.missingData.column$value>0], 
#      train.missingData.column[train.missingData.column$value>0,],
#      NROW(train))
                                  
train$Age[is.na(train$Age)] <- mean(train$Age, na.rm=TRUE)
#train$Age[is.na(train$Age)]


# We can assume that the most common City 'S' for Southampton is the most probable value so we update the Embarked to fix the 2 unknown values.
#table(train$Embarked)
train$Embarked[which(is.na(train$Embarked) | train$Embarked=="")] <- 'S'

##############################################################
#"Pclass" "Name" "Sex" "Age" "SibSp" "Parch" "Ticket" "Fare" "Cabin" "Embarked"
fol <- formula(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked)

library(rpart)
decisionTreeModel <- rpart(fol, method='class', data=train)
# plots decision tree
#plot(decisionTreeModel, main="Classification Tree of titanic")
#text(decisionTreeModel, use.n=TRUE, all=TRUE, cex=0.8)                                  

prediction <- predict(decisionTreeModel, test, type="class") # type="class" outputs the highest probability class

test$Survived <- as.numeric(as.numeric(prediction) == 1)
# https://www.kaggle.com/dansbecker/submitting-from-a-kernel                                  
write.csv(test[,c("PassengerId", "Survived")],"submission.csv", row.names=F)

