
# This R script will run on our backend. You can write arbitrary code here!

# Load packages

#library('ggplot2') # visualization
#library('ggthemes') # visualization
#library('scales') # visualization
#library('dplyr') # data manipulation
#library('mice') # imputation
#library('randomForest') # classification algorithm

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")

# We can inspect the train data. The results of this are printed in the log tab below
#summary(train)
nrow(train)
