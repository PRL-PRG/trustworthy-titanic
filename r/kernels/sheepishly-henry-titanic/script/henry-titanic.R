
# Title: Henry_Titanic
# Version: v1.0
# Date: 14th March 2017
# Aim: Use knowledge learned in the machine learning tutorial to create a working solution
# Accuracy Target: None
# Method: Random Forest

# Plan:
#   1. Clean data and fill in the blank results
#   2. Set up a prediction using random forest on "train" data
#   3. Use the prediction set up on "test" data
#   4. Save and submit results

# Section 1 - Clean data and fill in the blank results

# Read the train and test data, which  is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

names(which(colSums(is.na(train))>0))

testvector <- NA
testvector <- train$Cabin
testvector