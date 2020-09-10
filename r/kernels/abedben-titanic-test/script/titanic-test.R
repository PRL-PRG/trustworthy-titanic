
# This R script will run on our backend. You can write arbitrary code here!

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

full  <- bind_rows(train, test)

dim(full)

typeof(unique(full[,1]))


#for (i in 1:dim(full)[2]) print(length((unique(full[,i]))))

