
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data.
summary(train)

# plot the passenger survival by class
train$Survived <- factor(train$Survived, levels=c(1,0))
levels(train$Survived) <- c("Survived", "Died")
train$Pclass <- as.factor(train$Pclass)
levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

png("1_survival_by_class.png", width=800, height=600)
mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
dev.off()

## install.packages


## ----, warning = FALSE, message = FALSE----------------------------------
library(caret)
library(randomForest)

## ----, eval = FALSE------------------------------------------------------
## setwd("FILE PATH TO DIRECTORY")

## ----, eval = FALSE------------------------------------------------------
## setwd("~/Desktop/Titanic/")


trainSet <- read.table("train.csv", sep = ",", header = TRUE)


testSet <- read.table("test.csv", sep = ",", header = TRUE)

head(trainSet)

head(testSet)

table(trainSet[,c("Survived", "Pclass")])

## ----, warning = FALSE, message = FALSE----------------------------------
# Comparing Age and Survived: The boxplots are very similar between Age
# for survivors and those who died. 
library(fields)
bplot.xy(trainSet$Survived, trainSet$Age)
# Also, there are lots of NA's. Exclude this variable
summary(trainSet$Age)

# Comparing Age and Fare: The boxplots are much different between 
# Fare for survivors and those who died.
bplot.xy(trainSet$Survived, trainSet$Fare)
# Also, there are no NA's. Include this variable.
summary(trainSet$Fare)

## ------------------------------------------------------------------------
# Convert Survived to Factor
trainSet$Survived <- factor(trainSet$Survived)
# Set a random seed (so you will get the same results as me)
set.seed(42)
# Train the model using a "random forest" algorithm
model <- train(Survived ~ Pclass + Sex + SibSp +   
                          Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
                          data = trainSet, # Use the trainSet dataframe as the training data
                          method = "rf",# Use the "random forest" algorithm
                          trControl = trainControl(method = "cv", # Use cross-validation
                                                   number = 5) # Use 5 folds for cross-validation
               )

## ------------------------------------------------------------------------
model

## ------------------------------------------------------------------------
testSet$Survived <- predict(model, newdata = testSet)

## ------------------------------------------------------------------------
summary(testSet)

## ------------------------------------------------------------------------
testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)

## ------------------------------------------------------------------------
testSet$Survived <- predict(model, newdata = testSet)

## ------------------------------------------------------------------------
submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")