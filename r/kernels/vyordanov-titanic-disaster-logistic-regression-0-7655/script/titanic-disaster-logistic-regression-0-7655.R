## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----datasets--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train = read.csv("../input/train.csv", stringsAsFactors = F)
test = read.csv("../input/test.csv", stringsAsFactors = F)


## ----structure train-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Survived = as.factor(as.character(train$Survived))
train$Pclass = as.factor(as.character(train$Pclass))
train$Sex = as.factor(as.character(train$Sex))
train$Embarked = as.factor(as.character(train$Embarked))

test$Pclass = as.factor(as.character(test$Pclass))
test$Sex = as.factor(as.character(test$Sex))
test$Embarked = as.factor(as.character(test$Embarked))


## ----summary train---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(train)


## ----subset with missing Age-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missing = subset(train, is.na(Age))
summary(missing)


## ----histogram 1-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)

ggplot() + 
    geom_histogram(data = train, mapping = aes(x = Fare), fill = "blue", bins = 5) +
    geom_histogram(data = missing, mapping = aes(x = Fare), fill = "red", bins = 5) + 
    facet_grid(.~ Pclass)


## ----Pclass table for missing----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(missing$Pclass, missing$Sex)


## ----imputation, message=FALSE, warning=FALSE, results='hide'--------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(mice)
imputed = complete(mice(train,m=5,maxit=50,meth='pmm',seed=500))


## ----Age distribution after imputation, warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
    geom_histogram(data = imputed, mapping = aes(x = Age), fill = "red", bins = 10) +
    geom_histogram(data = train, mapping = aes(x = Age), fill = "black", bins = 10, alpha = 0.4) +
    facet_grid(. ~ Sex)


## ----dummy model-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(train$Survived)


## ----exploratory 1, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
    geom_histogram(data = imputed, mapping = aes(x = Survived, fill = factor(Sex)), alpha = 0.8, stat="count")


## ----exploratory 2---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
    geom_histogram(data = imputed, mapping = aes(x = Age, fill = factor(Survived)), bins = 10) +
    facet_grid(. ~ Sex)


## ----exploratory 3, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
    geom_histogram(data = imputed, mapping = aes(x = Pclass, fill = factor(Survived)), stat="count") +
    facet_grid(. ~ Sex)


## ----exploratory 4, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
    geom_histogram(data = imputed, mapping = aes(x = SibSp, fill = factor(Survived)), stat="count") +
    facet_grid(. ~ Sex)


## ----exploratory 5, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
    geom_histogram(data = imputed, mapping = aes(x = Parch, fill = factor(Survived)), stat="count") +
    facet_grid(. ~ Sex)


## ----exploratory 6, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
    geom_histogram(data = imputed, mapping = aes(x = Embarked, fill = factor(Survived)), stat="count") +
    facet_grid(. ~ Sex)


## ----model1 training, warnings=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
imputed = subset(imputed, select = -c(PassengerId))
model1 = glm(Survived ~ Sex + Pclass + Embarked + SibSp, data = imputed, family=binomial)
predict1 = predict(model1, type="response")

a = table(imputed$Survived, predict1 >= 0.5)

TP = a[2,2] # true positives
TN = a[1,1] # true negatives
FP = a[1,2] # false positives
FN = a[2,1] # false negatives

sensitivity = TP/(TP+FN)
specificity = TN/(TN+FN)
accuracy = (TN + TP)/(TN + TP + FP + FN)


## ----model1 testing, warnings=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction <- predict(model1, newdata=test, type = "response")
solution <- data.frame(PassengerID = test$PassengerId, Survived = round(prediction, 0))
write.csv(solution, file = 'model1_Solution.csv', row.names = F)

