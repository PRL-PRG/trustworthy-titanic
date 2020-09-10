# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.
###################################################################################################################################
#                                                                                                                                 #
# Titanic Dataset Survival Prediction Problem                                                                                     #
#                                                                                                                                 #
# Alamgir Mohammed, alamgirm@uwindsor.ca                                                                                          #
#                                                                                                                                 #
###################################################################################################################################

#import required packages
library(mice)   #for missing data imputation
library(e1071)  #for Support Vector Machine
library(randomForest)  #for Random Forest

#print message
print("Building model ...")
train <- read.csv('../input/train.csv', stringsAsFactors = FALSE)

#columns with factor/category data
factCols <- c("Name","Sex","Ticket","Cabin","Embarked")

# trim white spaces for factor/category columns
# and replace empty string with NA values
train[factCols] <- lapply(
                          train[factCols], 
                          function (x) {
                            x <- trimws(x)
                            x <- ifelse(x=="", NA, x)
                            })


# make a new feature based on salutation
# Mr. Miss. Mrs. Master.  etc.
train$Salutation <- lapply(train$Name, function (x) {
                     re <- regexpr("(Jonkheer[.])|(Countess[.])|(Capt[.])|(Col[.])|(Mlle[.])|(Sir[.])|(Lady[.])|(Major[.])|(Ms[.])|(Mr[.])|(Miss[.])|(Mrs[.])|(Master[.])|(Don[.])|(Rev[.])|(Dr[.])|(Mme[.])", x)
                     m  <- regmatches(x, re)
                     if (attributes(re)['match.length'] == -1)
                       m <- NA
                     
                     as.character(m)
                     }
                    ) 
# map rare salutation to a common one - possible loss of information 
# mlle -> Miss.
# Mme, Ms, Countess. Lady. -> Mrs.
# Dr. Rev., Major., Jonkeer,  -> Spl.
# Capt, Col, Don, Rev,        -> Spl.
train$Salutation <- unlist(lapply(train$Salutation, function (x) {
                            y <- sub("(Mlle[.])", "Miss.", x)
                            y <- sub("(Mme[.])|(Ms[.])|(Countess[.])|(Lady[.])", "Mrs.", y)
                            y <- sub("(Jonkheer[.])|(Capt[.])|(Col[.])|(Major[.])|(Don[.])|(Rev[.])|(Dr[.])|(Sir[.])", "Spl.", y)
                            y
                            }
                        ))
 
 
# factorize salutations
salLevels <- sort(unique(train$Salutation))
salLabels <- 1:length(salLevels)
train$Salutation <- factor(train$Salutation, levels = salLevels, labels = salLabels)
 
 
# Create a new feature FamSize = Sibling or Souse + Parent or Children + person him or herself
train$FamSize <- train$SibSp + train$Parch + 1
 
#factorize the fam size feature
train$FSizeFactor <- character(nrow(train))
train$FSizeFactor <- "med"
train$FSizeFactor[train$FamSize == 1] <- 'small'
train$FSizeFactor[train$FamSize > 4] <- 'big'
famLevels <- c("small", "med", "big")
famLabels <- 1:3
train$FamSize <- factor(train$FSizeFactor, levels = famLevels, labels = famLabels)
train$FSizeFactor <- NULL

#factorize other features
train$Survived <- factor(train$Survived, levels=c(0, 1), labels=c(0,1))
train$Pclass <- factor(train$Pclass, levels=c(1, 2, 3), labels=c(1, 2, 3))
train$Sex <- factor(train$Sex, levels=c("female", "male"), labels=c(0,1))
train$Embarked <- factor(train$Embarked, levels=c("C", "Q", "S"), labels=c(1,2,3))
 
 
# see how FamSize is related to Survival
# histogram(~train$FamSize|train$Survived==1, data=train)
# these are the features we are going to use
selFeatures <- c ("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Salutation", "FamSize" )

# select the data for chosen features
sel_train <- train[selFeatures]
 
# impute missing data
imp <- mice(sel_train, printFlag = F)

# take just the 1st set of imputed data
train_full <- complete(imp, 1)
 

# split data for train/test purpose, 80%/20%
train_size <- nrow(train_full)*0.80
# seed to make the partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(train_full)), size = train_size)
 
train_set <- train_full[train_ind, ]
test_set <- train_full[-train_ind, ]
 
# tune SVM parameters
# svm_tune <- tune('svm', train.x=train_set[,-1], train.y=train_set[,1], kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
# print(svm_tune)
 
# Build the SVM
# print("Selecting model")
svmModel <- svm(formula = train_set$Survived ~ ., data = train_set[,-1], cost = 1, gamma = .5, type = 'C-classification', kernel="radial")
svmPred  <- predict(svmModel, test_set[,-1], type='class') 

# Confusion matrix
cm1 <- table(svmPred, test_set[,1])
print(cm1)

# Build Random Forest Model
rfModel <- randomForest(x=train_set[,-1], y=train_set[,1])
rfPred <- predict(rfModel, test_set[,-1], type='class')

# Confusion matrix
cm2 <- table(rfPred, test_set[,1])
print(cm2)
plot(rfModel)
importance(rfModel)

# manually compare the confusion matrix and choose either of them

##################################################################################################################################### 
# happy with the results, both RF and SVM seem to perform similar
# now load the test data and repeat the process
# print message
print("Testing the model ...")
 
test <- read.csv('../input/test.csv', stringsAsFactors = FALSE)
 
 
# trim white spaces for non-numeric columns
# and replace empty string with NA values
test[factCols] <- lapply(
   test[factCols], 
   function (x) {
     x <- trimws(x)
     x <- ifelse(x=="", NA, x)
   })
 
 
# make a new feature based on salutation
# Mr. Miss. Mrs. Master. 
test$Salutation <- lapply(test$Name, function (x) {
   re <- regexpr("(Jonkheer[.])|(Countess[.])|(Capt[.])|(Col[.])|(Mlle[.])|(Sir[.])|(Lady[.])|(Major[.])|(Ms[.])|(Mr[.])|(Miss[.])|(Mrs[.])|(Master[.])|(Don[.])|(Rev[.])|(Dr[.])|(Mme[.])", x)
   m  <- regmatches(x, re)
   if (attributes(re)['match.length'] == -1) # avoid any salutaton that didnt occur in the train set
     m <- NA
   
   as.character(m)
 }
 ) 

# map rare salutations to a common one 
# mlle -> Miss
# Mme, Ms, Countess. Lady. -> Mrs.
# Dr. Rev., Major., Jonkeer,  -> Spl.
# Capt, Col, Don, Rev,        -> Spl.
test$Salutation <- unlist(lapply(test$Salutation, function (x) {
   y <- sub("(Mlle[.])", "Miss.", x)
   y <- sub("(Mme[.])|(Ms[.])|(Countess[.])|(Lady[.])", "Mrs.", y)
   y <- sub("(Jonkheer[.])|(Capt[.])|(Col[.])|(Major[.])|(Don[.])|(Rev[.])|(Dr[.])|(Sir[.])", "Spl.", y)
   y
 }
 ))
 
# factorize salutations
test$Salutation <- factor(test$Salutation, levels = salLevels, labels = salLabels)
 
 
# Create a new feature FamSize = Sibling or Souse + Parent or Children + person him or herself
test$FamSize <- test$SibSp + test$Parch + 1
 
# factorize the fam size feature
test$FSizeFactor <- character(nrow(test))
test$FSizeFactor <- "med"
test$FSizeFactor[test$FamSize == 1] <- 'small'
test$FSizeFactor[test$FamSize > 4] <- 'big'

test$FamSize <- factor(test$FSizeFactor, levels = famLevels, labels = famLabels)
test$FSizeFactor <- NULL
 
# factorize other features
test$Pclass <- factor(test$Pclass, levels=c(1, 2, 3), labels=c(1, 2, 3))
test$Sex <- factor(test$Sex, levels=c("female", "male"), labels=c(0,1))
test$Embarked <- factor(test$Embarked, levels=c("C", "Q", "S"), labels=c(1,2,3))
 
# selected features
selFeatures <- c ("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Salutation", "FamSize" )
 
# select the data for chosen features
sel_test <- test[selFeatures]
 
# impute missing data
imp <- mice(sel_test, printFlag = F)

# take just the 1st set of imputed data
test_full <- complete(imp, 1)
 
 #predict based on Random Forest model
 pred_rf <- predict(rfModel, test_full, type='class')
 
 #build the solution data frame
 solution <- data.frame("PassengerId" = test$PassengerId, "Survived" = pred_rf)
 
 #write the solution in desired format
 write.csv(solution, "gender_submission.csv", row.names = F, quote = F)
 
 