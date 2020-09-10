## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Libraries and Data, result = "hide", message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------

##making sure all packages are already installed
if( !("dplyr" %in% installed.packages()) ) {
    install.packages("dplyr")
}
if( !("ggplot2" %in% installed.packages()) ) {
    install.packages("ggplot2")
}
if( !("tidyr" %in% installed.packages()) ) {
    install.packages("tidyr")
}
if( !("missForest" %in% installed.packages()) ) {
    install.packages("missForest")
}
if( !("corrplot" %in% installed.packages()) ) {
    install.packages("corrplot")
}

##loading neccessary installed packages
library(tidyr)
library(ggplot2)
library(dplyr)
library(missForest)
library(corrplot)


##downloading Kaggle datasets in the programm are giving me authintication errors.
##These files were manually downloaded
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)


## ---- First Look-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(train)
str(test)


## ---- Impute Data, message = FALSE, result = "hide"------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## must remove the character variables with more than 53 unique values to avoid error.
charColRemove <- c("Name", "Ticket", "Cabin")
trainImputed <- train %>% select(-one_of(charColRemove))
testImputed <- test %>% select(-one_of(charColRemove))

##the rest of the character variables (less than 53 unique values) are converted to factors.
trainImputed <- trainImputed %>% mutate_if(is.character, as.factor)
testImputed <- testImputed %>% mutate_if(is.character, as.factor)

##run missForest function to impute remaining data.
trainImputed <- missForest(trainImputed)
testImputed <- missForest(testImputed)


##Here I should put the original data frame, including char vars, back together!
newTrain <- trainImputed$ximp %>% mutate(PassengerId = train$PassengerId, Name = train$Name, Ticket = train$Ticket, Cabin = train$Cabin)


## ----Exp Analysis Plot 1---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

qplot(Age, as.numeric(Survived), data = trainImputed$ximp, facets = . ~ Sex, geom = c("point", "smooth"))



## ----Exp Analysis Plot 2---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
qplot(Age, ..count.., data = trainImputed$ximp, fill = factor(Survived), facets = . ~ Pclass, geom = c("histogram"))




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainCorr <- trainImputed$ximp %>% mutate_if(is.factor, as.integer)
corrplot(cor(trainCorr))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

final <- glm(formula = Survived ~ Pclass + Sex + Age + SibSp, family = "binomial", data = newTrain)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(final)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

t1 <- table(train$Sex, train$Survived)
t1



## ----First Model Accuracy--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## 233 women survived which our model would predict correctly
## 468 men died which our model would predict correctly
(t1[3] + t1[2])/nrow(train)

##sensitivity would start with 233 because only women could have been successfully predicted to survive.
t1[3]/(t1[3]+t1[1])

##specificity would start with 468 because only men could have been successfully predicted dead.
t1[2]/(t1[2]+t1[4])


## ----Log Model Accuracy----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predTrain <- predict(final, type = "response")
t2 <- table(train$Survived, predTrain >= 0.5)

(t2[1] + t2[4])/nrow(train)

t2[4] / (t2[2] + t2[4])

t2[1] / (t2[3] + t2[1])

