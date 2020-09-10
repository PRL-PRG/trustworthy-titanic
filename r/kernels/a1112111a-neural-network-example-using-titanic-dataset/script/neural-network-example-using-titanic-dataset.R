
#This kernal is meant to showcase how a neural network could be built using the titanic dataset as an example
#Once the neural network is functioning, we then have a base against which to measure our improvement
#as we begin the process of feature engineering - see 'What's in a NAme' kernal - excellent source


#load libraries 

library(dplyr)
library(neuralnet)
library(randomForest)

#reading training and test set in

full <- read.csv('../input/train.csv', stringsAsFactors = F)
full2 <- read.csv('../input/test.csv', stringsAsFactors = F)

# Because this is an old dataset, I will assume you all know where the missing values
# are, and also have an opinion on how best to deal with them

sapply(full,function(x) sum(is.na(x)))
sapply(full2,function(x) sum(is.na(x)))

#Let's select the variables we are interested in

variables <- c('Survived','Pclass','Sex','Age','Embarked',
                 'Fare','Parch','SibSp')
full <- full[variables]
variables2 <- c('Pclass','Sex','Age','Embarked',
                 'Fare','Parch','SibSp')
full2 <- full2[variables2]

#let's fill in the missing values

sapply(full, function(x) sum(is.na(x)))
sapply(full2, function(x) sum(is.na(x)))


full$Age[is.na(full$Age)] <- mean(full$Age,na.rm=T)
full$Fare[is.na(full$Fare)] <- mean(full$Fare,na.rm=T)
full2$Age[is.na(full2$Age)] <- mean(full2$Age,na.rm=T)
full2$Fare[is.na(full2$Fare)] <- mean(full2$Fare,na.rm=T)

#let's have a closer look at Embarked - around 70% have S
count(full, Embarked)
count(full2, Embarked)

full$Embarked[is.na(full$Embarked)] <- 'S'
full2$Embarked[is.na(full2$Embarked)] <- 'S'

sapply(full, function(x) sum(is.na(x)))
sapply(full2, function(x) sum(is.na(x)))

full$Embarked <- as.factor(full$Embarked)
full$Sex <- as.factor(full$Sex)
full2$Embarked <- as.factor(full2$Embarked)
full2$Sex <- as.factor(full2$Sex)

#Ok sweet, no nulls
#Let's use random forest to see which variables are most important

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + Embarked +
                         Fare + Parch + SibSp,
                         data = full)

importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

#now we see the variables we want. SO let's divide into a training and test set

chosen_variables <- c('Survived','Pclass','Sex','Age',
               'Fare','SibSp')
full_prepped <- full[chosen_variables]
chosen_variables2 <- c('Pclass','Sex','Age',
               'Fare','SibSp')
full_prepped2 <- full2[chosen_variables2]

#let's split back into the train and test set
train <- full_prepped[1:800,]
test <- full_prepped[801:891,]


#the neuralnet packages requires input in the form of a matrix, otherwise it doesn't seem to work

m <- model.matrix( 
  ~ Survived + Pclass + Sex+ Age + 
    Fare + SibSp,data = full_prepped)#train)

nn <- neuralnet( 
  Survived ~ Pclass + Sexmale+ Age + 
    Fare + SibSp, 
  data=m, hidden=2, threshold=0.01, linear.output = F)

#now we do the same thing to the test set to test the nn
m1 <- model.matrix( 
  ~ + Pclass + Sex+ Age + #Survived + Pclass + Sex+ Age + 
    Fare + SibSp,data = full_prepped2)#test)

#Compute the nn
#res <- neuralnet::compute(nn, m1[,c("Pclass","Sexmale","Age", 
#    "Fare","SibSp")])
res <- neuralnet::compute(nn, m1[,c("Pclass","Sexmale","Age", 
    "Fare","SibSp")])

#take result and put it into the global environment, and then join back to the test set.

pred_train = round(res$net.result)
print(pred_train)
#test_final <- cbind(test,pred_train)

#test_final$correct[test_final$Survived == test_final$pred_train ] <- 1

#let's see what portion our nn got correct ~ 85%. not bad for a dirty quick version. next step - feature engineering!
#paste(sum(test_final$correct, na.rm = TRUE)/91)

