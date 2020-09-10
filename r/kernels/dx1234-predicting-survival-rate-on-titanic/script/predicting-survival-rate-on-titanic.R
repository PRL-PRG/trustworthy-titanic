
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
traindata <- read.csv("../input/train.csv")
testdata  <- read.csv("../input/test.csv")

summary(traindata)
summary(testdata)

sum(is.na(traindata))
sum(is.na(testdata))

str(traindata)
str(testdata)

#Boxplot For checking outliers present in Age.
boxplot(traindata$Age)

#Preprocessing train and test data - Converting variables into Factors and Intergers Accordingly.
traindata$Pclass<- as.factor(traindata$Pclass)
traindata$Age<-as.integer(traindata$Age)


testdata$Pclass <- as.factor(testdata$Pclass)
testdata$Age <- as.integer(testdata$Age)
testdata$Fare <- as.integer(testdata$Fare)


#Subsetting the data 
traindata <- subset(traindata,select=-c(Name,Ticket,Cabin))
testdata  <- subset(testdata,select=-c(Name,Ticket,Cabin))

#Removing Outliers
outlier <- function(dt, var) {
  var_name <- eval(substitute(var),eval(dt))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  boxplot(var_name, main="With outliers", col = "darkred")
  hist(var_name, main="With outliers", xlab=NA, ylab=NA, col = "darkred")
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  boxplot(var_name, main="Without outliers", col = "darkred")
  hist(var_name, main="Without outliers", xlab=NA, ylab=NA, col = "darkred")
  title("Outlier Check", outer=TRUE)
  na2 <- sum(is.na(var_name))
  cat("Outliers identified:", na2 - na1)
  cat("\nPropotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1))
  cat("\nMean of the outliers:", round(mo, 2))
  m2 <- mean(var_name, na.rm = T)
  cat("\nMean without removing outliers:", round(m1, 2))
  cat("\nMean if we remove outliers:", round(m2, 2))
  response <- readline(prompt="\nDo you want to remove outliers and to replace with NA? [yes/no]: ")
  if(response == "y" | response == "yes"){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    cat("Outliers successfully removed")
    return(invisible(dt))
  } else{
    cat("Nothing changed")
    return(invisible(var_name))
  }
}


#Train data visual
outlier(traindata, Age)

#Testdata visual
outlier(testdata, Age) 


#Loading Library Dmwr (function for data mining with R)
library(DMwR)
#Subsetting the variable from data and Applying Central Imputation
Ageinfo<-subset(traindata,select=c(Age))
Ageinfo<-centralImputation(Ageinfo) 

#Subsetting the variable from data and Applying Central Imputation
Fareinfo<-subset(traindata,select=c(Fare))
Fareinfo<-centralImputation(Fareinfo)

library(vegan)
#Using range method
dataStd. <- decostand(Fareinfo,"range") 
summary(dataStd.)

#Discretizing the variable
library(infotheo)
AgeBin <- discretize(Ageinfo, disc="equalwidth",nbins=4)
table(AgeBin)

#tapply usage
tapply(traindata$Age,AgeBin,min)
tapply(traindata$Age,AgeBin,max)

#Subsetting testdata for variables accordingly
traindata<-subset(traindata,select=-c(Age,Fare))

#Age and Fare Adding to original table
traindata<-data.frame(traindata,AgeBin,dataStd.)
str(traindata)


library(DMwR)
Ageinfo<-subset(testdata,select=c(Age))
Ageinfo<-centralImputation(Ageinfo) #Cenral Imputation

Fareinfo<-subset(testdata,select=c(Fare))
Fareinfo<-centralImputation(Fareinfo) #Cenral Imputation

#Standardizing the data
library(vegan)
dataStd. <- decostand(Fareinfo,"range") 
summary(dataStd.)

#Discretizing the variable
library(infotheo)
AgeBin <- discretize(Ageinfo, disc="equalwidth",nbins=4)
table(AgeBin)

#tapply usage
tapply(testdata$Age,AgeBin,min)
tapply(testdata$Age,AgeBin,max)

#Subsetting data
testdata<-subset(testdata,select=-c(Age,Fare))

#Age and Fare Adding to original table
testdata<-data.frame(testdata,AgeBin,dataStd.)
str(testdata)

#Finding correlations and covariance among Age and Fare
cov(traindata$Age,traindata$Fare)
cor(traindata$Age,traindata$Fare)

cov(testdata$Age,testdata$Fare)
cor(testdata$Age,testdata$Fare)


#We randomly select 40% of test set as a separate cross validation set. 
#The rest 60% remain as training set.

size_train <- nrow(traindata)
sample_index <- sample.int(size_train, size = floor(0.4*size_train))
str(sample_index)
crossvalidation <- traindata[sample_index,]
str(crossvalidation)
train_new <- traindata[setdiff(seq(1:size_train), sample_index), ]
str(train_new)

#Applying Logistic Regression and building model.

library(aod)
mylogit <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, family=binomial(link=logit), data=train_new)

summary(mylogit)

#To verify that the model is reasonable, we calculate the data fitting for 
#both training set and cross validation set.

predict_survive <- function(mylogit, df, prob){
  predict_s <- predict(mylogit, newdata=df, type="response")
  return(sapply(predict_s, FUN=function(x){if (x>prob) return(1) else return(0)}))
}
threshold_parameter <- function(mylogit, df, parameter_set){
  predict_err <- parameter_set
  k=0
  for (par in parameter_set){
    k <- k + 1
    predict_df <- predict_survive(mylogit, df, par)
    predict_err[k] <-  sum((predict_df - df$Survived)^2)
  }
  index <- order(predict_err)[1]
  return(parameter_set[index])
}

#warnings()
par_set <- seq(0.1,0.9,0.05)
best_prob <- threshold_parameter(mylogit,crossvalidation, par_set) 
predict_cv <- predict_survive(mylogit, crossvalidation, best_prob)
predict_cv_error <- sum((predict_cv-crossvalidation$Survived)^2)/nrow(crossvalidation)
predict_train <- predict_survive(mylogit, train_new, best_prob)
predict_train_error <- sum((predict_train-train_new$Survived)^2)/nrow(train_new)

testdata$Survived <- predict_survive(mylogit, testdata, best_prob)


final_result <- testdata[c("PassengerId","Survived")]
class(final_result)
colnames(final_result) <- c("PassengerId","Survived")
write.csv(final_result, file="submission.csv", row.names=FALSE)



#### Training Model 1: Decision Tree Model ####
library(rpart)
library(rattle)
library(rpart.plot)
library(e1071)
library(caret)

#Converting Survived to factor for model building.
train_new$Survived<- as.factor(train_new$Survived)
crossvalidation$Survived<-as.factor(crossvalidation$Survived)



#Visualizations
library(ggplot2)

#Plotting Survival Rate on traindata According to PassengerClass.
ggplot(traindata, aes(x= Pclass, fill=factor(Survived))) + 
  geom_bar(width=0.5) +
  xlab("Pclass") + 
  ylab("Total Count") + 
  labs(fill="Survived")



#Plotting Survival Rate on testdata According to PassengerClass.
ggplot(testdata, aes(x= Pclass, fill=factor(Survived))) + 
  geom_bar(width=0.5) +
  xlab("Pclass") + 
  ylab("Total Count") + 
  labs(fill="Survived")

#Plotting Survival Rate on traindata According to Age.
ggplot(traindata, aes(x= Age, fill=factor(Survived))) + 
  geom_bar(width=0.5) +
  xlab("Age") + 
  ylab("Total Count") + 
  labs(fill="Survived")

#Plotting Survival Rate on traindata According to Age.
ggplot(testdata, aes(x= Age, fill=factor(Survived))) + 
  geom_bar(width=0.5) +
  xlab("Age") + 
  ylab("Total Count") + 
  labs(fill="Survived")

#Plotting Survival Rate According to Age and sex.
ggplot(traindata, aes(x= Age, fill=factor(Sex))) + 
  geom_bar(width=0.5) +
  xlab("Age") + 
  ylab("Total Count") + 
  labs(fill="Sex")

#Plotting Survival Rate According to Age and sex.
ggplot(testdata, aes(x= Age, fill=factor(Sex))) + 
  geom_bar(width=0.5) +
  xlab("Age") + 
  ylab("Total Count") + 
  labs(fill="Sex")
str(crossvalidation)

modFit.rpart <- rpart(Survived ~.,data=train_new)
modPred.rpart <- predict(modFit.rpart, newdata=testdata, type='class')
confusionMatrix(table(modPred.rpart,testdata$Survived))

fancyRpartPlot(modFit.rpart)


final_result2 <-data.frame(PassengerId = testdata$PassengerId, Survived = modPred.rpart)
# Write solution to a csv file.
write.csv(final_result2, file = "submission2.csv", row.names = FALSE)


# Using Kfold crossvalidation
k.folds <- function(k) {
  folds <- createFolds(train_new$Survived, k = k, list = TRUE, returnTrain = TRUE)
  for (i in 1:k) {
    model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                   data = train_new[folds[[i]],], method = "class")
    predictions <- predict(object = model, newdata = train_new[-folds[[i]],], type = "class")
    accuracies.dt <- c(accuracies.dt, 
                       confusionMatrix(predictions, train_new[-folds[[i]], ]$Survived)$overall[[1]])
  }
  accuracies.dt
}

set.seed(567)
accuracies.dt <- c()
accuracies.dt <- k.folds(5)
accuracies.dt

mean.accuracies <- mean(accuracies.dt)

model.single <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                      data = train_new, method = "class")
# Apply the model to the test data
predict.test <- predict(object = model.single, newdata = testdata, type = "class")

# Create a data frame with just PassengerId and Survived 
final_result3 <- data.frame(PassengerId = testdata$PassengerId, Survived = predict.test)

# Write solution to a csv file.
write.csv(final_result3, file = "submission3.csv", row.names = FALSE)



#### Training Model 2: Random Forest Model ####
#library(randomForest)
#set.seed(2609)
#modFit.rf <- randomForest(Survived ~ ., data=train_new, ntree=20)
#modPred.rf <- predict(modFit.rf, newdata=crossvalidation, type="class")
#confusionMatrix(table(modPred.rf,testdata$Survived))


#final_result4 <-data.frame(PassengerId = crossvalidation$PassengerId, Survived = modPred.rf)
# Write solution to a csv file.
#write.csv(final_result4, file = "submission4.csv", row.names = FALSE)


#### Training Model 3: Support Vector Machine Model ####
#modFit.svm <- svm(Survived ~ ., data=train_new)
#modPred.svm <- predict(modFit.svm, newdata=testdata)
#confusionMatrix(table(modPred.svm,testdata$Survived))

#final_result5 <-data.frame(PassengerId = crossvalidation$PassengerId, Survived = modPred.svm)
# Write solution to a csv file.
#write.csv(final_result5, file = "submission5.csv", row.names = FALSE)




# Gradient boosting fitting and predicting
library(gbm)
n.trees <- 5000
gbm_fit <- gbm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,data=testdata, distribution = "bernoulli", interaction.depth = 3, n.minobsinnode = 10, n.trees = n.trees, shrinkage = 0.001, train.fraction = 0.8, verbose = TRUE)
gbm.perf(gbm_fit) # To see the variable importance
#summary(gbm_fit)
predict_gbm <- predict(gbm_fit, traindata, n.trees = gbm.perf(gbm_fit), type = "response") # Predicting on the train set to check for overfitting and best cut-off since probabilities are returned
predict_gbm2 <- predict(gbm_fit, testdata, n.trees = gbm.perf(gbm_fit), type = "response") # Predicting on the test set

# Since gbm gives a survival probability prediction, we need to find the best cut-off on the train set:
proportion <- sapply(seq(.3,.7,.01),function(step) c(step,sum(ifelse(predict_gbm<step,0,1)!=traindata$Survived)))
#dim(proportion)
predict_gbm_train <- ifelse(predict_gbm < proportion[,which.min(proportion[2,])][1],0,1) # Converting probabilities into 0 or 1 according to the best cut-off
head(predict_gbm_train)
score <- sum(traindata$Survived == predict_gbm_train)/nrow(traindata)
score

# Applying the best cut-off on the test set
predict_gbm_test <- ifelse(predict_gbm2<proportion[,which.min(proportion[2,])][1],0,1)
final_result6 <- data.frame(PassengerId = testdata$PassengerId, Survived = predict_gbm_test)

# Creating the submitting file
write.csv(final_result6, file = "submission6.csv", row.names = FALSE)


