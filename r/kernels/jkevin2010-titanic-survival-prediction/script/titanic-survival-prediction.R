

# This program predicts the survival chances of Male vs Female in the Titanic Disaster
# Also we are going to test our model by predicting survival chances of child (age <16)
# we use the survival data from Titanic Disaster as our "Training" Data
# We are going to make a model from our "Training Data" and test it on the "Test Data"
# We print the Survival proportions at the end of the program


library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function


# Import the training set: train

train <- read.csv("../input/train.csv")
  
# Import the testing set: test

test <- read.csv("../input/test.csv")
  


# We check the structure of Train and Test


# Survival rates in absolute numbers
table(train$Survived)


# Survival rates in proportions
prop.table(table(train$Survived))

  
# Two-way comparison: Sex and Survived
table(train$Sex, train$Survived)


# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0

# Two-way comparison for chances of Survival of a child compare to adults
prop.table(table(train$Child, train$Survived), 1)


# Copy of test
test_one <- test

# Initialize a Survived column to 0
test_one$Survived <- 0


# Set Survived to 1 if Sex equals "female"
test_one$Survived[test_one$Sex == "female"] <- 1
