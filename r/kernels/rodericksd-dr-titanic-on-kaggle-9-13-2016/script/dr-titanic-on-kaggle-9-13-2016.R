
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library (rpart)
library (caret)
#install.packages('rattle')
#install.packages ('rpart.plot')
#install.packages ('RColorBrewer')
#library(rattle)
library(rpart.plot)
library(RColorBrewer)
library (lattice)
library (ggplot2)



# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")


# *********************9-2-2016*************************


# Let us split train into two variables (one of 80%, the other of 20%)
#train2 <- train[1:712,]
#train2
#str(train2)

#train2cv <- train[713:891,]
#train2cv

#How many survived in the train2 data set
#Survived = 278 (39.04%), Died = 434 (60.96%), total 712 in train2 
#table(train2$Survived)
#prop.table(table(train2$Survived))

#How many males, females in train2 data set
# Females = 256 (35.96%), Males = 456 (64.04%)
#table(train2$Sex)
#prop.table(table(train2$Sex))

#How many males, females survived
# Females survived = 190 (74.22% of all women)
# Males survived = 88 (19.30% of all males)
#table(train2$Sex, train2$Survived)
#prop.table(table(train2$Sex, train2$Survived),1)

# Now, to investigate Age
# table(train2$Age,train2$Survived)


#fit <- rpart(Survived ~ Sex + Age + SibSp + Pclass + Embarked + Parch,
#        data = train2,
#       method = "class")
#rpart.plot(fit)


#*****************************9-13-2016**********************************
# First, clean up Pclass to be ordered using as.factor (so we do not get "1.5" in our decision trees)
train$Pclass <- as.factor(train$Pclass)
test$Pclass <- as.factor(test$Pclass)


#Group the Fare feature into fewer categories
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

test$Fare2 <- '30+'
test$Fare2[test$Fare < 30 & test$Fare >= 20] <- '20 - 30'
test$Fare2[test$Fare < 20 & test$Fare >= 10] <- '10 - 20'
test$Fare2[test$Fare < 10] <- '<10'

#Break up train dataframe into 5 train frames and use one of them as the cv set.  Then rotate through.

train1 <- train[1:179,]
train2 <-train[180:358,]
train3 <- train[359:537,]
train4 <- train[538:716,]
train5 <- train[717:891,]


# Setting up pairs of newtrain and cv to use on building our model.
cv1 <- train1
newtrain2345 <- rbind(train2,train3,train4,train5)
table(newtrain2345$Survived)
table(newtrain2345$Sex)
table(newtrain2345$Sex,newtrain2345$Survived)
# USe first pair of data sets (cv1 and newtrain2345).
#Construct a decision tree using rpart and examine outcomes
#Refer to this as "model 1"
fit <- rpart(Survived ~ Fare2 + Sex + Age + SibSp + Pclass + Embarked + Parch,
        data = newtrain2345,
       method = "class")
predict(fit,cv1,interval = "confidence")

#submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
#write.csv(submit, file = "theyallperish.csv", row.names = FALSE)





cv2 <- train2
newtrain1345 <- rbind(train1,train3,train4,train5)
#table(newtrain1345$Survived)
#table(newtrain1345$Sex)
#table(newtrain1345$Sex,newtrain1345$Survived)
# USe first pair of data sets (cv1 and newtrain2345).
#Construct a decision tree using rpart and examine outcomes
#Refer to this as "model 1"
#fit <- rpart(Survived ~ Fare2 + Sex + Age + SibSp + Pclass + Embarked + Parch,
#        data = newtrain1345,
#       method = "class")
#rpart.plot(fit)
#predict(fit,cv2,interval = "confidence")




cv3 <- train3
newtrain1245 <- rbind(train1,train2,train4,train5)
#table(newtrain1245$Survived)
#table(newtrain1245$Sex)
#table(newtrain1245$Sex,newtrain1245$Survived)
# USe first pair of data sets (cv3 and newtrain1245).
#Construct a decision tree using rpart and examine outcomes
#Refer to this as "model 1"
#fit <- rpart(Survived ~ Fare2 + Sex + Age + SibSp + Pclass + Embarked + Parch,
#        data = newtrain1245,
#       method = "class")
#rpart.plot(fit)
#predict(fit,cv3,interval = "confidence")




cv4 <- train4
newtrain1235 <- rbind(train1,train2,train3,train5)
#table(newtrain1235$Survived)
#table(newtrain1235$Sex)
#table(newtrain1235$Sex,newtrain1235$Survived)
# USe first pair of data sets (cv4 and newtrain1235).
#Construct a decision tree using rpart and examine outcomes
#Refer to this as "model 1"
#fit <- rpart(Survived ~ Fare2 + Sex + Age + SibSp + Pclass + Embarked + Parch,
#        data = newtrain1235,
#       method = "class")
#rpart.plot(fit)
#predict(fit,cv4,interval = "confidence")




cv5 <- train5
newtrain1234 <- rbind(train1,train2,train3,train4)
#table(newtrain1234$Survived)
#table(newtrain1234$Sex)
#table(newtrain1234$Sex,newtrain1234$Survived)
# USe first pair of data sets (cv5 and newtrain1234).
#Construct a decision tree using rpart and examine outcomes
#Refer to this as "model 1"
#fit <- rpart(Survived ~ Fare2 + Sex + Age + SibSp + Pclass + Embarked + Parch,
#        data = newtrain1234,
#       method = "class")
#rpart.plot(fit)
#predict(fit,cv5,interval = "confidence")


#At the conclusion of running this model (fit) against all 5 sets of data,
#it seems that the percentage accuracy in predicting survival is 78.77095% (median) or 79.2556% (average).
#Let us make a submission based on this model against test and see where Kaggle rates us.....


#fit <- rpart(Survived ~ Fare2 + Sex + Age + SibSp + Pclass + Embarked + Parch,
#        data = newtrain2345,
#      method = "class")

#predict(fit, test, interval = "confidence")
#prediction <- predict(fit,test,interval = "confidence")
#prediction.Survived





# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
#solution <- data.frame(PassengerId = test$PassengerId, Survived <- prediction)

#solution




# Write the solution to file
#write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)


#submission <- test(PassengerId = test$PassengerId)
#submission$Survived <- predict(fit, test, interval = "confidence")
#write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)
#submission








#Investigating Random Forests
#rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree=100, importance=TRUE)


# Here we will plot the passenger survival by class
# train$Survived <- factor(train$Survived, levels=c(1,0))
# levels(train$Survived) <- c("Survived", "Died")
# train$Pclass <- as.factor(train$Pclass)
# levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

# png("1_survival_by_class.png", width=800, height=600)
# mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
#           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
#           off=c(0), cex.axis=1.4)
#dev.off()
