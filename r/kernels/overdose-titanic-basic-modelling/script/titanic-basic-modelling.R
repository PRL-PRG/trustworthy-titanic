# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

# titanic script
# v0.1
# 10/05/2017

# load the data, test and training sets
train = read.csv('../input/train.csv', header=T, sep=',')
test = read.csv('../input/test.csv', header=T, sep=',')
# view the training data summary
summary(train)

# baseline, nobody survived
table(train$Survived)
# calculate accuracy
549/(549+342)
# 0.6161616 - 61.6%

# baseline submission save to a dataframe with two columns: PassengerId and Survived (prediction)
#submission <- data.frame(PassengerID = test$PassengerId, Survived = 0)
# write the solution to file
#write.csv(submission, file = 'submission.csv', row.names = F)

##############################################################
## build basic glm model
#mod = glm(Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked, data=train, family="binomial")
#summary(mod)
## test out of sample error - score data, calc accuracy
#predictions = ifelse(predict(mod, newdata=test, type="response") > 0.5, 1, 0)
#predictions[is.na(predictions)] = 0
## view accuracy ?

## generate submission file
#submission <- data.frame(PassengerID = test$PassengerId, Survived = predictions)

## write the solution to file
#write.csv(submission, file = 'submission.csv', row.names = FALSE)

##############################################################
# build basic tree model
library(rpart)
mod = rpart(Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked, data=train, method="class")
summary(mod)

# make some predictions - score data
predictions = predict(mod, newdata=test, type="class")

# generate submission file
submission <- data.frame(PassengerID = test$PassengerId, Survived = predictions)

# write the solution to file
write.csv(submission, file = 'submission_tree_basic.csv', row.names = FALSE)

