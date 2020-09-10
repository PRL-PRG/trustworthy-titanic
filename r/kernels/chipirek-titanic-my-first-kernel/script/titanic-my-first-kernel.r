
library(elasticnet)
library(caret)
library(plyr)
library(dplyr)
library(skimr)
library(RANN)
library(e1071)
library(PerformanceAnalytics)
library(outliers)
library(xgboost)
library(ranger)
library(mice)
library(DataExplorer)

set.seed(1)


training.dataset <- read.csv("../input/train.csv", stringsAsFactors=FALSE, na.strings = c("","NaN"," "))
#test.dataset  <- read.csv("test.csv",  stringsAsFactors=FALSE, na.strings = c("","NaN"," "))

str(training.dataset)

plot_missing(training.dataset)

plot_histogram(training.dataset)

plot_bar(training.dataset)

plot_correlation(training.dataset, title="Correlation Analysis")

# reclass target variable
training.dataset$Survived <- ifelse(training.dataset$Survived == '0', 'No', 'Yes')
training.dataset$Survived <- as.factor(training.dataset$Survived)

# remove the 3 id fields and the useless Cabin field
training.dataset <- training.dataset %>% 
  select(-c('PassengerId', 'Name', 'Ticket', 'Cabin')) %>%
  filter(!is.na(training.dataset$Embarked))

# convert to factors
training.dataset$Pclass <- as.factor(training.dataset$Pclass)
training.dataset$Sex <- as.factor(training.dataset$Sex)
training.dataset$Embarked <- as.factor(training.dataset$Embarked)

# impute mean age where NA
training.dataset$Age[is.na(training.dataset$Age)] <- mean(training.dataset$Age, na.rm=TRUE)

# examine cleaned dataset
str(training.dataset)

plot_missing(training.dataset)

plot_correlation(training.dataset, title="Correlation Analysis")

fit.control <- trainControl(
    method = 'cv',                   # k-fold cross validation
    number = 5,
    savePredictions = 'final',       # saves predictions for optimal tuning parameter
    classProbs = T#,                 # should class probabilities be returned
) 

# train random forest
model.rf = train(Survived ~ ., 
                 data=training.dataset,
                 method='rf', 
                 tuneLength=8, 
                 trControl=fit.control)
model.rf

# train SVM
model.svm = train(Survived ~ ., 
                  data=training.dataset, 
                  method='svmRadial', 
                  tuneLength=10, 
                  trControl=fit.control)
model.svm

# train k-nearest neighbors
model.knn = train(Survived ~ ., 
                  data=training.dataset, 
                  method='knn', 
                  tuneLength=10, 
                  trControl=fit.control)
model.knn

# train xgboost

# setup the xgb tune grid
tune.grid.for.xgb <- expand.grid(nrounds          = c(10,50),
                                 max_depth        = c(5, 10, 15), 
                                 eta              = c(0.01, 0.001, 0.0001), 
                                 gamma            = c(1, 2, 3), 
                                 colsample_bytree = c(0.4, 0.7, 1.0), 
                                 min_child_weight = c(0.5, 1, 1.5),
                                 subsample        = 1
)

model.xgb <- train(Survived ~., 
                   data=training.dataset,
                   method="xgbTree",
                   trControl=fit.control,
                   tuneGrid=tune.grid.for.xgb
                   )
model.xgb

# compare model performances using resample()
models.compared <- resamples(list(`Random Forest`=model.rf, 
                                  `SVM`=model.svm,
                                  `kNN`=model.knn,
                                  `XGBoost`=model.xgb
                                  ))

# summary of the models performances
summary(models.compared)

training.dataset <- read.csv("../input/train.csv", stringsAsFactors=FALSE, na.strings = c("","NaN"," "))

# reclass target variable
training.dataset$Survived <- ifelse(training.dataset$Survived == '0', 'No', 'Yes')
training.dataset$Survived <- as.factor(training.dataset$Survived)

# remove the 3 id fields and the useless Cabin field
training.dataset <- training.dataset %>% 
  select(-c('PassengerId', 'Name', 'Ticket', 'Cabin')) %>%
  filter(!is.na(training.dataset$Embarked))

# convert to factors
training.dataset$Pclass <- as.factor(training.dataset$Pclass)
training.dataset$Sex <- as.factor(training.dataset$Sex)
training.dataset$Embarked <- as.factor(training.dataset$Embarked)



training.dataset$FamilySize <- training.dataset$Parch + training.dataset$SibSp

training.dataset <- training.dataset %>% 
  select(-c('Parch', 'SibSp')) 


summary(training.dataset$Age)

mi <- mice(training.dataset)
training.dataset <- complete(mi)
summary(training.dataset$Age)

# train random forest
model.rf = train(Survived ~ ., 
                 data=training.dataset,
                 method='rf', 
                 tuneLength=7, 
                 trControl=fit.control)
				 
# train SVM
model.svm = train(Survived ~ ., 
                  data=training.dataset, 
                  method='svmRadial', 
                  tuneLength=7, 
                  trControl=fit.control)

# train k-nearest neighbors
model.knn = train(Survived ~ ., 
                  data=training.dataset, 
                  method='knn', 
                  tuneLength=7, 
                  trControl=fit.control)

# train xgb
model.xgb <- train(Survived ~., 
                   data=training.dataset,
                   method="xgbTree",
                   trControl=fit.control,
                   tuneGrid=tune.grid.for.xgb
                   )

# compare model performances using resample()
models.compared <- resamples(list(`Random Forest`=model.rf, 
                                  `SVM`=model.svm,
                                  `kNN`=model.knn,
                                  `XGBoost`=model.xgb
                                  ))

# summary of the models performances
summary(models.compared)



