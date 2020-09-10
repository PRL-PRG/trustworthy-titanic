# Title: Titanic Kaggle Competition v1
# Author: Landon Gibson
# Date: 8/29/17
#--------------------------------#
# Data Import
fileurl_train <- "https://storage.googleapis.com/kaggle-competitions-data/kaggle/3136/train.csv?GoogleAccessId=competitions-data@kaggle-161607.iam.gserviceaccount.com&Expires=1504325799&Signature=ZREfojvjBxGz2%2FF4Yu1Kka3j%2BylZLrFiNNgzVOjRJk4XOpKFgV%2BK4eM0fS874l3fXVvg051nUB%2BDsCVORI%2FN3Cgt%2FyE5OLE5zpwpe%2Ff1zWD5JPS91S767g0oRCT%2FEwqfV4Dx2TwIT1dEZXVBfLVL5qlp5lIDJSOnfezs3at%2BhMpGDJUIKzbtnbA%2BLn%2FhRbtZqa%2B8SnhdmwnDOSkxA9ZYDK9QpmITb7liBRgd3DthiTNCp2ajG%2FSwnOgV8Tm%2FNefDKTMtbRgljgMzvpuQ031SomV5iBu8WmIygqxjfib6DhKP4m5t%2FahZzBYIEfY7AmBokiZDNlwtVwOivWDANIXBzA%3D%3D"
fileurl_test <- "https://storage.googleapis.com/kaggle-competitions-data/kaggle/3136/test.csv?GoogleAccessId=competitions-data@kaggle-161607.iam.gserviceaccount.com&Expires=1504325924&Signature=hL1wMTmPKV5OYjZqHckFPzc%2BWAbEp0dD4AtzxgaLQXosZaj4dtGlM2PEl0Jq0WNTAZhUIQifGweKJM6qTa8G%2FQgRyRunN%2BQa2hqocGDsQ77wi71CkmOvqKogZL0fMLnU%2Bowd7ww5OBSerpAYXY%2FC8vT5J%2BrVfMAxfXQfyO9w1as%2BV7zUia14TfplOxMJwJQGNib08T8qIpZYjyvSZZcWEvoIKPqpnZBqDqnrxGOOvm%2BDpvWL04ftIg7mIAFOlhxiJ87X%2BQOAHyv9u9HBia2lbjn6h%2BoI8HmF0o6i%2BZs6BK3EyMMEEaE1G5dv02j%2B8%2B%2F9V%2FWukLam2y%2Fx1jMs6D3wKQ%3D%3D"
trainSet <- read.delim(fileurl_train, header=T, stringsAsFactors = FALSE, sep = ",")
testSet <- read.delim(fileurl_test, header=T, stringsAsFactors = FALSE, sep = ",")
  # outcomeName <- "Survived" | trainSet[,2]
names(trainSet)
#--------------------------------#

#--------------------------------#
library(plyr)
library(dplyr)
# Data Exploration
dim(trainSet)
  # 12 Variables and 891 Passengers
# Structure
str(trainSet)
  # Mostly integers but there are a few character variables
  # Character variables that can't be recoded easily:
    # Name, Ticket, Possibly Cabin??
table(trainSet$Cabin)
  # Nope, definitly will have issues with cabin . . 
  # Would consider separating out B/C/D from the numbers
apply(trainSet[,c(-1,-4,-9)],2,table) # removing pID, name, and ticket from table command
  # Survived: Not too concerend about class imbalance (549/342)
  # PClass: possible skew on pClass though
  # Ticket: Yo, I need to definitely check why tickets match. What is ticket line??
  # Gender: way more men
  # I'll have to make a histogram of Age, also I'm concerned about NAs
  # SibSp heavy on 0 but I think I may need to link this up with age to understand it better
    # if they are > 18 then probably spouses etc, worth thinking about
  # Parch = num of parents/children aboard
    # again I want to link this with age and get more granularity here, find familes??
  # histogram of fare
  # Cabin: Could I use this to find families? Bunch of NAs but not coded as such . .

# More Detailed Dive Into Specific Variables
hist(trainSet$Age) # not bad pretty centered around 30-35
sum(is.na(trainSet))
sum(is.na(trainSet$Age)) # yup . . . looks like I got em all right here. ugh.
age_missing <- trainSet %>% filter(is.na(Age) == TRUE)


hist(trainSet$Fare) # Holy skew . . . let's look at the people who dropped 200+ on fare . .
fare_spenders <- trainSet %>% filter(Fare >= 200) # Seems legit. Gonna have to transform this
summary(trainSet$Fare) # 0s!?!?
fare_freeloaders <- trainSet %>% filter(Fare == 0)
head(fare_freeloaders)

# Skew of Continuous Variables
library(e1071)
skewness(trainSet$Age,na.rm=TRUE)
skewness(trainSet$Fare) # crazy skew


#--------------------------------#

#--------------------------------#
# Check-In
  # Okay so at this point I have a whole bunch of points and thoughts
    # 1. Fare needs to be transformed.
    # 2. Age needs to be imputed. (prediction to predict age?!?)
    # 3. I need to look into why ticket #'s match
    # 4. Consider utilizing names:
      # Breaking out first, last, and importantly TITLE
    # 5. Break out SibSp and Parch into Siblings, Spoues, Parents, Children
  # I'll continue by working through these issues before getting to prediction
#--------------------------------#

#--------------------------------#
# 1. Fare Transformation
library(e1071)
skewness(log1p(trainSet$Fare)) # pretty good! Had to use this because of 0s
trainSet$Freeride <- 0
trainSet$Freeride[trainSet$Fare == 0] <- 1 # Flaggin these people for future reference
trainSet$Fare <- log1p(trainSet$Fare)

# 2. Age Imputation via Prediction
  # I'm gonna come back to this one after I've done more feature creation

# 3. Ticket Matching Visual Inspection
# Splitting out the characters from the numbers
a = tidyr::separate(data=trainSet,Ticket,into=c('test1','test2','test3'),sep=" ",fill='right')
# Making test2 = test1 if missing(test2)
a$test4 <- 1
a$test4[is.na(a$test2) == TRUE] <- 0
# Come back to this.

# 4. Splitting Names
# Breaking First and Last (via sep=',')
names <- trainSet %>% tidyr::separate(Name,into=c('lastname','firstname'),sep=",") %>% select(PassengerId,Survived,lastname,firstname)
  # How much commonality is there across last names?
length(unique(names$lastname))
# Breaking out Titles from first name
  # First counting the number of "."
library(stringr)
names$numberofperiods <- str_count(names$firstname,"[.]")
  # Okay so some people have two, going to need to split on first instance
title_frstname <- regmatches(names$firstname, regexpr("[.]", names$firstname), invert = TRUE)
df_tf <- data.frame(matrix(unlist(title_frstname), nrow=891, byrow=T),stringsAsFactors=FALSE)
  # Bringing titles and first/middle names back into names
names$title <- df_tf$X1
names$title <- str_trim(names$title)
names$firstname <- df_tf$X2
table(names$title)
  # Some interesting bits here: 
    # Mlle is an unmarried woman and Mme is married (french)
    # Don is an honor title in spain
    # Jonkheer is a dutch title
    # Master is used for boys, usually eldest son (couldn't call mister yet)
    # Miss is for unmarried and Mrs is for married
# Breaking up First Name further (pulling out middle or nicknames)
names$firstname <- str_trim(names$firstname) # Trimming leading & trailing spaces
frstnme <- regmatches(names$firstname, regexpr(" ", names$firstname), invert = TRUE)
  # converting into unique vectors
firstname <- vector()
othernames <- vector()
for (i in 1:length(frstnme)) {
  firstname[i] <- frstnme[[i]][1]
  othernames[i] <- frstnme[[i]][2]
}
  # pluggin back into dataset
names$firstname <- firstname
names$othernames <- othernames
  # cleaining up firstnames
names$firstname <- str_trim(names$firstname)
names$firstname <- gsub("\\(", "", names$firstname)
names$firstname <- gsub("\\)", "", names$firstname)
  # Merging back into trainSet
trainSet$title <- names$title
trainSet$firstname <- names$firstname
trainSet$lastname <- names$lastname

# 5. Delving into SibSp and Parch
View(trainSet[order(trainSet$lastname,trainSet$Ticket),])
  # Looks like names in parentheses is for married women!
  # I'll come back to this when I have time
#--------------------------------#

#--------------------------------#
# Cleaing up Data: 
  # Converting Int Vars to Numeric
str(trainSet)
for (i in 1:ncol(trainSet)) {
  if (is.integer(trainSet[,i])==TRUE) {
    trainSet[,i] <- as.numeric(trainSet[,i])
  }
}
str(trainSet)
names(trainSet)
  # Dropping Unnecessary Vars
drops <- c('Name','Cabin')
trainSet <- trainSet[,(!names(trainSet) %in% drops)]
# Making Some Variables Numeric
  # http://amunategui.github.io/dummyVar-Walkthrough/
library(caret)
dmy <- dummyVars("~ Sex + title + Embarked",data=trainSet,fullRank=TRUE)
trsf <- data.frame(predict(dmy, newdata = trainSet))
trainSet_dmy <- cbind(trainSet,trsf)
drops <- c('Sex','title','Embarked')
trainSet_dmy <- trainSet_dmy[,(!names(trainSet_dmy) %in% drops)]


#--------------------------------#

#--------------------------------#
# Imputing Age via Mulitple Imputation
  # Obviously this is an area for improvement
library(missForest)
drop_char <- c('Ticket','firstname','lastname')
trainSet.mis <- trainSet_dmy[,(!names(trainSet_dmy) %in% drop_char)]
str(trainSet.mis)

set.seed(636)
# errors <- vector()
# for (i in 1:ncol(trainSet.mis)) {
#   trainSet.imp <- missForest(trainSet.mis,maxiter=10,ntree=1000,mtry=i)
#   errors[i] <- trainSet.imp$OOBerror
# } # shows that 6 is the minimum

trainSet.imp <- missForest(trainSet.mis,maxiter=10,ntree=2000,mtry=6)
trainSet.imp$OOBerror

trainSet_dmy$Age <- round(trainSet.imp$ximp$Age,0)
str(trainSet_dmy)
#--------------------------------#
# Final Setup
  # Removing Characters & pID
drop_char <- c('firstname','lastname','Ticket','PassengerId','EmbarkedQ','EmbarkedC','EmbarkedS')
trainSet_dmy <- trainSet_dmy[,(!names(trainSet_dmy) %in% drop_char)]
  # Renaming titlethe.countess
colnames(trainSet_dmy)[24] <- "titletheCountess"
#--------------------------------#
#--------------------------------#
#--------------------------------#
#--------------------------------#

#--------------------------------#
#--------------------------------#
#--------------------------------#
#--------------------------------#
# Analysis and Predictions
# http://topepo.github.io/caret/index.html
#----------------------------------#
detachAllPackages <- function() {
  
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  
  package.list <- setdiff(package.list,basic.packages)
  
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
  
}
detachAllPackages()
#install.packages("caret", dependencies = c("Imports", "Depends", "Suggests"))
#----------------------------------#
# Loading in R Java
# http://charlotte-ngs.github.io/2016/01/MacOsXrJavaProblem.html
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_144.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
require(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
sessionInfo()
#----------------------------------#
# Installing Packages
library(caret)
library(rJava)
library(plyr)
library(dplyr)
#----------------------------------#

#----------------------------------#
# Correlation Matrix
correlationMatrix <- cor(trainSet_dmy[,2:ncol(trainSet_dmy)])
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
trainSet_dmy_cor <- trainSet_dmy[,-highlyCorrelated]

# Converting outcome to "No,Yes" and factor
trainSet_dmy_cor$survived <- "No"
trainSet_dmy_cor$survived[trainSet_dmy_cor$Survived == 1] <- "Yes"
trainSet_dmy_cor$survived <- as.factor(trainSet_dmy_cor$survived)
col_rm <- which(colnames(trainSet_dmy_cor) == "Survived")
trainSet_dmy_cor <- trainSet_dmy_cor[,-col_rm]

# Splitting Data
set.seed(768)
index <- createDataPartition(trainSet_dmy_cor$Survived,p=0.8,list=FALSE)
an.trainSet <- trainSet_dmy_cor[index,]
an.testSet <- trainSet_dmy_cor[-index,]
str(an.trainSet)
#----------------------------------#
# Turning on Parallel Processing
library(doMC)
registerDoMC(cores=4)
#----------------------------------#

#----------------------------------#


#----------------------------------#
# Recursive Feature Selection Setup
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      number = 10,
                      repeats = 10,
                      verbose = FALSE,
                      allowParallel = TRUE)
outcomeName <- 'survived'
predictors <- names(an.trainSet)[!names(an.trainSet) %in% outcomeName]
# Feature Selection
selectedFeatures <- rfe(an.trainSet[,predictors],an.trainSet[,outcomeName],rfeControl=control)
selectedFeatures
predictors(selectedFeatures)
plot(selectedFeatures,type=c('g','o'))
#----------------------------------#

#----------------------------------#
# Tuning Models to . . . 
# Optimize on ROC
# Use "One SE" rule to avoid overfitting
# Utilize adaptive random tuning to improve efficiency
set.seed(643)
fitControl <- trainControl(method="repeatedcv",
                           summaryFunction=twoClassSummary,
                           number=5,
                           repeats=5,
                           classProbs=TRUE,
                           allowParallel=TRUE,
                           selectionFunction="oneSE",
                           adaptive=list(min=8,alpha=0.5,
                                         method='gls',complete=TRUE))
fitControl_noprob <- trainControl(method="repeatedcv",
                                  summaryFunction=twoClassSummary,
                                  number=5,
                                  repeats=5,
                                  allowParallel=TRUE,
                                  selectionFunction="oneSE",
                                  adaptive=list(min=8,alpha=0.5,
                                                method='gls',complete=TRUE))
# Last Step: Converting to Matrix
#----------------------------------#
#----------------------------------#
# Training Models
#----------------------------------#
# RF
set.seed(643)
ptm <- proc.time()
grid_rf <- expand.grid(mtry=c(1:10))
model_rf <- train(an.trainSet[,selectedFeatures$optVariables],an.trainSet[,outcomeName],
                  method='rf',metric="ROC",
                  trControl=fitControl,
                  tuneGrid = grid_rf,
                  preProcess=c('center','scale'),
                  verbose=FALSE)
pred_rf <- predict.train(object=model_rf,an.testSet)
cm_rf <- confusionMatrix(pred_rf,an.testSet[,outcomeName])
proc.time()-ptm
#----------------------------------#
# Naive Bayes
ptm <- proc.time()
grid_nb <- data.frame(fL=c(0.1,0.5,0.7,1,3,5,7,10,20,50), usekernel = TRUE, adjust=c(seq(1:10)))
model_nb<-train(an.trainSet[,selectedFeatures$optVariables],an.trainSet[,outcomeName],
                method='nb',metric="ROC",
                trControl=fitControl,
                tuneGrid=grid_nb,
                preProcess=c('center','scale'),
                verbose=FALSE)
pred_nb <- predict.train(object=model_nb,an.testSet)
cm_nb <- confusionMatrix(pred_nb,an.testSet[,outcomeName])
proc.time()-ptm
#----------------------------------#

#----------------------------------#
library(pROC)
roc <- function(model,auc_yn) {
  pred_prob <- predict.train(object = model,an.testSet,type="prob")
  if (auc_yn == 0) {
    model.ROC <- pROC::roc(an.testSet[,outcomeName],
                           pred_prob[,2],
                           levels=c('No','Yes'))
  } else {
    model.ROC <- pROC::roc(an.testSet[,outcomeName],
                           pred_prob[,2],
                           levels=c('No','Yes'))$auc
  }
}
auc_ci <- function(model) {
  pred_prob <- predict.train(object = model,an.testSet,type="prob")
  model.ROC <- pROC::roc(an.testSet[,outcomeName],
                         pred_prob[,2],
                         levels=c('No','Yes'))$auc
  model.ROC.CI <- pROC::ci(model.ROC)
}
# Calculating AUC & ROC values & CIs & Accuracy & Specificity
model_list <- list(rf=model_rf, nBayes=model_nb)
cm_model_list <- list(rf=cm_rf,nBayes=cm_nb)
resamp <- resamples(model_list)
model_pred_auc <- lapply(model_list,roc,auc_yn=1)
model_pred_roc <- lapply(model_list,roc,auc_yn=0)
model_pred_ci.auc <- lapply(model_list,auc_ci)
# Extracting lower and upper bounds
low.bounds <- lapply(model_pred_ci.auc,function(x) as.numeric(as.character(x)[1]))
up.bounds <- lapply(model_pred_ci.auc,function(x) as.numeric(as.character(x)[3]))
# Accuracy/Specificity/Sensitivity
class_metrics <- function(cm_model,metric=0) {
  if (is.atomic(cm_model) == TRUE) {
    if (metric==1) {
      # Accuracy
      acc <- (cm_model[1] + cm_model[4]) / sum(cm_model)
    } else if (metric==2) {
      # Specificity
      spec <- (cm_model[4]) / (cm_model[4] + cm_model[3])
    } else if (metric==3) {
      # Sensitivity
      sens <- (cm_model[1]) / (cm_model[1] + cm_model[2])
    } else {
      print('Please indicate which metric you would like. Accuracy = 1, Specificity = 2, and Sensitivity = 3. Use metric = . . .')
    }
  } else if (is.atomic(cm_model)==FALSE) {
    if (metric==1) {
      # Accuracy
      acc <- cm_model$overall[1]
    } else if (metric==2) {
      # Specificity
      spec <- cm_model$byClass[2]
    } else if (metric==3) {
      # Sensitivity
      sens <- cm_model$byClass[1]
    } else {
      print('Please indicate which metric you would like. Accuracy = 1, Specificity = 2, and Sensitivity = 3. Use metric = . . .')
    }
  }
}

model_acc <- lapply(cm_model_list,class_metrics,metric=1)
model_spec <- lapply(cm_model_list,class_metrics,metric=2)
model_sens <- lapply(cm_model_list,class_metrics,metric=3)
#----------------------------------#

#----------------------------------#
# Testing Differences Between TRAINED Models
difs <- diff(resamp,metric="ROC")
compare_models(model_nb,model_rf)

# Data table of AUC/Accuracy/and Specificity for each model on test data to compare models
test.auc <- data.frame(Model=c(names(model_list)),
                       AUC=c(as.vector(unlist(model_pred_auc))),
                       Lower.CI=c(as.vector(unlist(low.bounds))),
                       Upper.CI=c(as.vector(unlist(up.bounds))),
                       Accuracy=c(as.vector(unlist(model_acc))),
                       Specificity=c(as.vector(unlist(model_spec))),
                       Sensitivity=c(as.vector(unlist(model_sens))))
test.auc$Model <- factor(test.auc$Model, levels=test.auc$Model)
test.auc <- test.auc[order(test.auc$AUC,decreasing=TRUE),]
test.auc



