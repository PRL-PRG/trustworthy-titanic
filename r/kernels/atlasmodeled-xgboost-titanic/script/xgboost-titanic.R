
# This script trains a Random Forest model based on the data,
# saves a sample submission, and plots the relative importance
# of the variables in making predictions

# Download 1_random_forest_r_submission.csv from the output below
# and submit it through https://www.kaggle.com/c/titanic-gettingStarted/submissions/attach
# to enter this getting started competition!

library(ggplot2)
#library(randomForest)
library(caret)
library(xgboost); #getNamespaceExports("xgboost")
library(Matrix)
library(data.table)
library(pROC)

sessionInfo()

set.seed(1)
train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
test  <- read.csv("../input/test.csv",  stringsAsFactors=FALSE)

extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked",
                "Survived")
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex      <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  fea$Survived <- as.factor(fea$Survived)
  return(fea)
}

features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked",
                "Survived")
                
#train <- extractFeatures(train)
train <- data.table(train[,names(train) %in% features], keep.rownames = FALSE)
#train <- train[, features, with=FALSE] 
#train$Survived <- as.factor(train$Survived)
str(train)

ggplot(train) + geom_bar(aes(x=factor(Survived)))

#rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree=100, importance=TRUE)

trainIndex <- createDataPartition(train$Survived, p = .8, list = FALSE, times = 1)
train_set <- train[trainIndex,] ; str(train_set)
test_set <- train[-trainIndex,]

Surv_train <- as.numeric(as.factor(train_set$Survived))-1
Surv_test <- as.numeric(as.factor(test_set$Survived))-1
train_features <- subset(train_set, select = -c(Survived))
test_features <- subset(test_set, select = -c(Survived))

# perform one-hot encoding
#xgb.train <- sparse.model.matrix(Survived~.-1, data = train_set); str(xgb.train)
xgb.train <- xgb.DMatrix(as.matrix(sapply(train_features, as.numeric)), label=Surv_train); str(xgb.train)
xgb.test <- xgb.DMatrix(as.matrix(sapply(test_features, as.numeric)), label=Surv_test)

output_vector = train_set[,Survived] == 1 ; str(output_vector)

model <- xgboost(data = xgb.train, max_depth = 5,
                 nrounds = 2, objective = "binary:logistic", eval_metric = "auc", maximize = TRUE)


pred <- predict(model,xgb.test)
plot.roc(Surv_test, pred, print.auc=T, print.auc.y=0.5) 


#submission <- data.frame(PassengerId = test$PassengerId)
#submission$Survived <- predict(rf, extractFeatures(test))
#write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)
