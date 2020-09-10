#this is simply my attempt at learning how to explore a dataset using a guide. i do not take credit for this first trial of code.

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

train <- read.csv('../input/train.csv', stringsAsFactors = F) #training data
test  <- read.csv('../input/test.csv', stringsAsFactors = F) #testing data

full  <- bind_rows(train, test) # bind training & test data

# check data
str(train)
str(test)

#import numpy as np
#import pandas as pd

#Print you can execute arbitrary python code
#train = pd.read_csv("../input/train.csv", dtype={"Age": np.float64}, )
#test = pd.read_csv("../input/test.csv", dtype={"Age": np.float64}, )
#
#Print to standard output, and see the results in the "log" section below after running your script
#print("\n\nTop of the training data:")
#print(train.head())

#print("\n\nSummary statistics of training data")
#print(train.describe())

#Any files you save will be available in the output tab below
#train.to_csv('copy_of_the_training_data.csv', index=False)