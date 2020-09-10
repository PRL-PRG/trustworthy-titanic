
# load in packages we'll use
library(tidyverse) # utility functions
library(rpart) # for regression trees
library(randomForest) # for random forests
library(dplyr)
library(ggplot2)

train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
test <- read_csv("../input/test.csv")


summary(train)
summary(test)

# there are two methods to replace NA below is another method but I'm using the other option
# another method to replace NA
#age.mean <- mean(train$Age, na.rm=TRUE)
#train$Age[is.na(train$Age)] = age.mean

# fix the train data due to NA issue
avg <- median(train$Age, na.rm = TRUE)
train$Age <- replace(train$Age,is.na(train$Age),avg)

# fix the test data due to NA issue
avg <- median(test$Age, na.rm = TRUE)
test$Age <- replace(test$Age,is.na(test$Age),avg)

# there is also one NA under FARE for test data and this will be fixed as well.
avg <- median(test$Fare, na.rm = TRUE)
test$Fare <- replace(test$Fare,is.na(test$Fare),avg)


library(gridExtra)
library(grid)

# tidy one rows with no embarkation point and just put them to C ( 2 records only)
subset(train,!(Embarked %in% c('C','Q','S')))
train$Embarked[!(train$Embarked %in% c('C','Q','S'))] <- 'C'

# ggplot using geom_bar for some of the features
a <- ggplot(train, aes(x = factor(Sex), fill = factor(Survived))) + geom_bar(position='dodge')   
b <- ggplot(train, aes(x = factor(Pclass), fill = factor(Survived))) + geom_bar(position='dodge')   
c <- ggplot(train, aes(x = factor(Parch), fill = factor(Survived))) + geom_bar(position='dodge')   
d <- ggplot(train, aes(x = factor(SibSp), fill = factor(Survived))) + geom_bar(position='dodge')   
e <- ggplot(train, aes(x = factor(Embarked), fill = factor(Survived))) + geom_bar(position='dodge')   

grid.arrange(a,b,c,d,e, ncol=2)



ggplot(train, aes(x = factor(Embarked), fill = factor(Survived))) + geom_bar(position='dodge')  + facet_grid(Pclass~.)

train$AgeGrp[train$Age<=12] <- 'Kids'
train$AgeGrp[train$Age>12 & train$Age<=55] <- 'Adults'
train$AgeGrp[train$Age>55] <- 'Seniors'
train$AgeGrp <- factor(train$AgeGrp)

prop.table(table(train$AgeGrp))*100

train %>% filter(AgeGrp != 'Adults') %>% ggplot(aes(x = factor(Sex), fill = factor(Survived))) + geom_bar(position='dodge') + facet_grid(AgeGrp~Pclass)

# group to determine family/single
train$status <- 'Family'
train$status[train$Parch==0 & train$SibSp==0] <- 'Single'
train$status <- factor(train$status)

# work out the composition of the data
a <- prop.table(table(train$status)) * 100
b <- table(train$status)
rbind(Pctg = a,Count = b)

train %>% ggplot(aes(x = factor(status), fill = factor(Survived))) + geom_bar(position='dodge') + facet_grid(.~Pclass)

# Machine learning one using 10 fold validation
library(caret)
train$Survived <- factor(train$Survived)


# create 10 fold validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# Random Forest
set.seed(7)
fit.rf <- train(Survived ~ Sex + Pclass + AgeGrp + status + Fare, data=train, method="rf", metric=metric, trControl=control)

# based on the summary above, I decided to use random forest as my model
# let's see confusion matrix against training data to see how accurate it is
# put the result to another field called Survived2 and you prob will see accuracy around 84%, roughly about the same with 10 fold testing above

train$Survived2 <- predict(fit.rf,train)
confusionMatrix(train$Survived2,train$Survived)


test$AgeGrp[test$Age<=12] <- 'Kids'
test$AgeGrp[test$Age>12 & test$Age<=55] <- 'Adults'
test$AgeGrp[test$Age>55] <- 'Seniors'
test$AgeGrp <- factor(test$AgeGrp)


test$status <- 'Family'
test$status[test$Parch==0 & test$SibSp==0] <- 'Single'
test$status <- factor(test$status)
test$status <- factor(test$status)

summary(test)

# now let's apply the model to test data
test$Survived <-  predict(fit.rf,test)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "titanic3.csv", row.names = FALSE)
