# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input")

# Any results you write to the current directory are saved as output.
train <- read.csv('../input/train.csv', stringsAsFactors = FALSE, header = TRUE)
test <- read.csv('../input/test.csv', stringsAsFactors = FALSE, header = TRUE)
str(train)
train$Survived
table(train$Survived)
Sex <- as.factor(train$Sex)
table(train$Sex)
prop.table(table(train$Sex, train$Survived),1)
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
table(test$Survived)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)