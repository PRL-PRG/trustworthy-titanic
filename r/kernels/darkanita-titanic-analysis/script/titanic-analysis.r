
#Load Libraries
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(class)
library(naivebayes)
library(rpart)
library(randomForest)
library(pROC)
#Load Data
data_train <- read.csv('../input/train.csv', stringsAsFactors = TRUE)
data_test <- read.csv('../input/train.csv', stringsAsFactors = TRUE)


