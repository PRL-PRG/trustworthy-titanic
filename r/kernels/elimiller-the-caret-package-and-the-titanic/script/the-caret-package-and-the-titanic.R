## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message = FALSE, warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1)
##Load packages for caret and parallel processing
library(caret)
library(parallel)
library(doParallel)

##Register cores
cluster <- makeCluster(detectCores() - 1)
registerDoParallel(cluster)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
raw.data <- read.csv("../input/train.csv", na.strings = '')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(raw.data)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##See if any rows have missing data.
sapply(raw.data, anyNA)
##See if any rows are duplicates.
any(duplicated(raw.data))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Subset data for selected columns.
surv <- as.factor(raw.data$Survived)
selected.data <- subset(raw.data, select = c('Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked'))
##Create Dummy Variables to remove factor features.
dv <- dummyVars( ~., data = selected.data)
##Read dummy variables into new data frame.
dv.data <- predict(dv, newdata = selected.data)
##Make Preprocessed object.
pp <- preProcess(dv.data, method = 'knnImpute')
##Create object with preprocessed data
impute.data <- predict(pp, dv.data)
##Load imputed data into data frame.
imputed.data <- data.frame(dv.data)
imputed.data$Age <- impute.data[,4]
imputed.data$Survived <- raw.data$Survived
##Add embarked data
imputed.data[,8:10] <- impute.data[,8:10]
imputed.data$Survived <- surv
##Find correlation of features.
cor(imputed.data[1:7])


## ----include = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Remove unneeded feature.
imputed.data <- imputed.data[,-imputed.data$Sex.male]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
featurePlot(x = imputed.data[,2:7], y = as.factor(imputed.data$Survived), plot = 'pairs', auto.key = list(column = 2))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train.model.ind <- createDataPartition(imputed.data$Survived, p = 0.8, list = FALSE)
train.model <- imputed.data[train.model.ind,]
test.model <- imputed.data[-train.model.ind,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tc <- trainControl(allowParallel = TRUE, method = 'repeatedcv', number = 4, repeats = 2)


## ----include = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train.model$Survived <- as.factor(train.model$Survived)
test.model$Survived <- as.factor(test.model$Survived)
models.used <- c('kknn1', 'kknn2', 'rpart', 'bag','boost', 'nnet', 'svmLinear', 'rf')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
system.time({
    knnModel1 <- train(Survived ~ ., data = train.model, method = 'kknn', trainControl = tc)
    knnModel2 <- train(Survived ~ ., data = train.model, method = 'kknn', k = 3, l = 2, trainControl = tc)
})


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
system.time({
    treeModel <- train(Survived ~ ., data = train.model, method = 'rpart')
})


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
system.time({
    bagModel <- train(Survived ~ ., data = train.model, method = 'AdaBag', trainControl = tc)
})


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
system.time({
    boostModel <- train(Survived ~ ., data = train.model, method = 'adaboost', trainControl = tc)
})


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
system.time({
    netModel <- train(Survived ~ ., data = train.model, method = 'nnet', trace = FALSE, trainControl = tc)
})


## ----results = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
system.time({
    svmModel <- train(Survived ~ ., data = train.model, method = 'svmLinear', trainControl = tc)
})


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

system.time({
    rfModel <- train(Survived ~ ., data = train.model, method = 'rf', trainControl = tc)
})

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
stopCluster(cluster)
registerDoSEQ()
elapsedTime <- list( 9.91, 0.61, 684.11, 50.67, 2.72, 2.05, 6.95)


## ----include = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knn1.mat <- confusionMatrix(train.model$Survived, predict(knnModel1, train.model))
knn2.mat <- confusionMatrix(train.model$Survived, predict(knnModel2, train.model))
rpart.mat <- confusionMatrix(train.model$Survived, predict(treeModel, train.model))
bag.mat <- confusionMatrix(train.model$Survived, predict(bagModel, train.model))
boost.mat <- confusionMatrix(train.model$Survived, predict(boostModel, train.model))
net.mat <- confusionMatrix(train.model$Survived, predict(netModel, train.model))
svm.mat <- confusionMatrix(train.model$Survived, predict(svmModel, train.model))
rf.mat <- confusionMatrix(train.model$Survived, predict(rfModel, train.model))
train.accuracies <- c(knn1.mat$overall[1], knn2.mat$overall[1], rpart.mat$overall[1], bag.mat$overall[1], boost.mat$overall[1], net.mat$overall[1], svm.mat$overall[1], rf.mat$overall[1])

knn1.mat <- confusionMatrix(test.model$Survived, predict(knnModel1, test.model))
knn2.mat <- confusionMatrix(test.model$Survived, predict(knnModel2, test.model))
rpart.mat <- confusionMatrix(test.model$Survived, predict(treeModel, test.model))
bag.mat <- confusionMatrix(test.model$Survived, predict(bagModel, test.model))
boost.mat <- confusionMatrix(test.model$Survived, predict(boostModel, test.model))
net.mat <- confusionMatrix(test.model$Survived, predict(netModel, test.model))
svm.mat <- confusionMatrix(test.model$Survived, predict(svmModel, test.model))
rf.mat <- confusionMatrix(test.model$Survived, predict(rfModel, test.model))
test.accuracies <- c(knn1.mat$overall[1], knn2.mat$overall[1], rpart.mat$overall[1], bag.mat$overall[1], boost.mat$overall[1], net.mat$overall[1], svm.mat$overall[1], rf.mat$overall[1])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model.metas <- data.frame(row.names = models.used, train.accuracies, test.accuracies)
model.metas
plot(model.metas$test.accuracies, main = "Accuracy of the Models", xlab = "Models", ylab = "Accuracy", ylim = c(0.7, 1), xaxt = 'n', type = 'p', pch = 15, col = 'red')
points(model.metas$train.accuracies, col = 'blue', pch = 16)
axis(1, at = 1:8, labels = models.used)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Read in test data with unknown results.
sub.data <- read.csv('../input/test.csv')
ids <- sub.data$PassengerId
##Subset data for needed features.

##Subset data for selected columns.
sub.data <- subset(sub.data, select = c('Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked'))
sub.dv <- predict(dv, newdata = sub.data)
##pp <- preProcess(sub.data, method = 'knnImpute')
sub.impute <- predict(pp, sub.dv)
sub.data <- data.frame(sub.dv)
sub.data$Age <- sub.impute[,4]
sub.data$Fare <- sub.impute[,7]
imputed.data[,8:10] <- impute.data[,8:10]
preds <- predict(bagModel, sub.data)
submit <- data.frame(PassengerId = ids, Survived = preds)
write.csv(submit, 'submission.csv', row.names = FALSE)

