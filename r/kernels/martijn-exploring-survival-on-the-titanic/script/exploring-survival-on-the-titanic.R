## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('tutorial')


## ---- include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tutorial::go_interactive()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('http://s3.amazonaws.com/assets.datacamp.com/course/importing_data_into_r/swimming_pools.csv', stringsAsFactors = F)
head(train)


# check data
str(full)

