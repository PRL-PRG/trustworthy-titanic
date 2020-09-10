# KIT: What sorts of people were likely to survive?

# Apply the tools of machine learning to predict which passengers survived the tragedy
library("rpart")
library("caret")
library("randomForest")
library("plyr")
library("e1071")
library("ranger")
library("RColorBrewer")
library("party")
library("klaR")
library("dplyr")
library("ggplot2")
library("C50")
library("arm")
library("DMwR")
library("tidyr")
library("randomGLM")
library("usdm")
library("psych")
library("mice")
library("VIM")
library("Hmisc")
workingDir <- getwd()
list.files(path = workingDir)


## Load Titanic data to variable and remove NAs
titanicTrain <- read.csv("../input/train.csv", header = TRUE)

### Correct Survived and PClass variables to factors, rather than integers
titanicTrain$Survived <- as.factor(titanicTrain$Survived)
titanicTrain$Pclass <- as.factor(titanicTrain$Pclass)

## Create test data table
titanicTest <- read.csv("../input/test.csv", header = TRUE)

### Correct Survived and PClass variables to factors, rather than integers
titanicTest$Pclass <- as.factor(titanicTest$Pclass)

## Combine sets, noting that the test data is the last 418 rows [892:1309 ,]
finalTitanicData <- bind_rows(titanicTrain, titanicTest)



### Check for NAs
NAplot <- aggr(finalTitanicData,
               col = c("navyblue", "yellow"),
               numbers = TRUE,
               sortVars = TRUE,
               labels = names(finalTitanicData),
               ylab = c("Missing data", "Pattern"))
summary(NAplot)

### Impute missing values for Age
finalTitanicData$Age <- impute(finalTitanicData$Age)

### Check for again NAs
NAplot <- aggr(finalTitanicData,
               col = c("navyblue", "yellow"),
               numbers = TRUE,
               sortVars = TRUE,
               labels = names(finalTitanicData),
               ylab = c("Missing data", "Pattern"))
summary(NAplot)

### Impute missing values for Fare
finalTitanicData$Fare <- impute(finalTitanicData$Fare)

### Check for again NAs
NAplot <- aggr(finalTitanicData,
               col = c("navyblue", "yellow"),
               numbers = TRUE,
               sortVars = TRUE,
               labels = names(finalTitanicData),
               ylab = c("Missing data", "Pattern"))
summary(NAplot)

### Split back into test and train sets with new imputed values
titanicTrain <- finalTitanicData[1:891, ]
titanicTest <- finalTitanicData[892:1309, ]



## Create variable to use in future models, defining survived as a function of other variables
fitFormula1 <- as.formula(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked)
fitFormula2 <- as.formula(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare)

## Create variable to use as a control across model trainings
myControl <- trainControl(method="repeatedcv", 
                          number = 10, 
                          repeats = 3)

## Define the evalution metric now for use with all of the model training later
myMetric <- "Accuracy"



### Create and review Bayesian Generalized Linear Model
fitNB1 <- train(fitFormula1, 
                data = titanicTrain,
                method = "bayesglm",
                trControl = myControl,
                metric = myMetric)

fitNB2 <- train(fitFormula2, 
                titanicTrain,
                method = "bayesglm",
                trControl = myControl,
                metric = myMetric)



### Create and review Random Forest model
fitRT1 <- train(fitFormula1, 
                data = titanicTrain, 
                method = "rf", 
                metric = myMetric, 
                trControl = myControl)

fitRT2 <- train(fitFormula2, 
                data = titanicTrain, 
                method = "rf", 
                metric = myMetric, 
                trControl = myControl)



## Create evaluate SVM Radial model
fitSVMRadial1 <- train(fitFormula1, 
                       data = titanicTrain, 
                       method = "svmRadial", 
                       metric = myMetric, 
                       trControl = myControl,
                       fit = FALSE)

fitSVMRadial2 <- train(fitFormula2, 
                       data = titanicTrain, 
                       method = "svmRadial", 
                       metric = myMetric, 
                       trControl = myControl,
                       fit = FALSE)


## Create and evaluate CART model
fitCART1 <- train(fitFormula1, 
                  data = titanicTrain, 
                  method = "rpart", 
                  metric = myMetric, 
                  trControl = myControl)

fitCART2 <- train(fitFormula2, 
                  data = titanicTrain, 
                  method = "rpart", 
                  metric = myMetric, 
                  trControl = myControl)



## Create evaluate C50 Tree model
fitC501 <- train(fitFormula1, 
                 data = titanicTrain, 
                 method = "C5.0", 
                 metric = myMetric, 
                 trControl = myControl)

fitC502 <- train(fitFormula2, 
                 data = titanicTrain, 
                 method = "C5.0", 
                 metric = myMetric, 
                 trControl = myControl)



### Create and review Bagged CART model
fitBaggedCART1 <- train(fitFormula1, 
                        data = titanicTrain, 
                        method = "treebag", 
                        metric = myMetric, 
                        trControl = myControl)

fitBaggedCART2 <- train(fitFormula2, 
                        data = titanicTrain, 
                        method = "treebag", 
                        metric = myMetric, 
                        trControl = myControl)



## Create evaluate k-Nearest Neighbors model
fitKNN1 <- train(fitFormula1, 
                 data = titanicTrain, 
                 method = "knn", 
                 metric = myMetric, 
                 trControl = myControl)

fitKNN2 <- train(fitFormula2, 
                 data = titanicTrain, 
                 method = "knn", 
                 metric = myMetric, 
                 trControl = myControl)



## Summary accuracy of models
modelsSummary <- resamples(list(baggedCart1 = fitBaggedCART1, baggedCART2 = fitBaggedCART2, C50v1 = fitC501, C50v2 = fitC502, CART1 = fitCART1, CART2 = fitCART2, KNN1 = fitKNN1, KNN2 = fitKNN2, NB1 = fitNB1, NB2 = fitNB2, RT1 = fitRT1, RT2 = fitRT2, SVMRadial1 = fitSVMRadial1, SVMRadial2 = fitSVMRadial2))
summary(modelsSummary)
dotplot(modelsSummary)

## Select the most accurate model tested.
fitRT2

## Generate final Titanic spreadsheet with predicted survival values
prediction <- predict(fitRT2, titanicTest)
prediction
solution <- data.frame(PassengerID = titanicTest$PassengerId, Survived = prediction)

write.csv(solution, file = 'KPSubmission.csv', row.names = F)