# SANG's random test 1 - I'm testing a decision tree structure 

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(tree)
# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

#summary(train) # summary data for the training data 
#summary(test) # summary data for the test data 

# training model for a classification tree

# From the logisitic regression data, we can understand that there is a correlation between 
# the fare of the titanic and the survival rate. First, trying that: 

names(train)  # Get the data columns of the titanic data 
pairs(train) # plot x by y for the entire set 




