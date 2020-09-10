## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(caret)
library(randomForest)
library(FSelector)
library(dplyr)
library(data.table)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Getting data

set.seed(1)
#training = data.table(read.csv("train.csv", stringsAsFactors = F))
#testing = data.table(read.csv("test.csv", stringsAsFactors = F))
training = data.table(read.csv('../input/train.csv', stringsAsFactors = F))
testing = data.table(read.csv('../input/test.csv', stringsAsFactors = F))

str(training)
str(testing)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Replace Age NA values with -1
training[is.na(Age), Age := -1]
testing[is.na(Age), Age := -1]

## Replace Embarked NA values with S
training[Embarked == "", Embarked := "S"]
testing[Embarked == "", Embarked := "S"]

## Replace Fare NA values with 0
training[is.na(Fare), Fare := 0]
testing[is.na(Fare), Fare := 0]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
training[, PassengerId := NULL][, Name := NULL][, Cabin := NULL][, Ticket := NULL]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
training[, Survived := as.factor(Survived)]


## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Train the model with the best 5 features

weights <- random.forest.importance(Survived ~ ., training, importance.type = 1)
print(weights)
subsetWeights <- cutoff.k(weights, 5)
modelFormula <- as.simple.formula(subsetWeights, "Survived")
modelFormula

tControl <- trainControl(method = "cv", number = 5)
modelrfWeights <- train(form = modelFormula, data = training, method = "rf", trControl = tControl, verbose = FALSE)



## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Predict the results and check accuracy
resultrfWeights <- predict(modelrfWeights, testing)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = testing$PassengerId, Survived = resultrfWeights)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)


