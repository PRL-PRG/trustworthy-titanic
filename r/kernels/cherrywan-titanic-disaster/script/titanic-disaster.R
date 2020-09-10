# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(xgboost)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")
gendermodel <- read_csv("../input/gendermodel.csv")
genderclassmodel <- read_csv("../input/genderclassmodel.csv")

summary(train)
summary(test)
summary(gendermodel)
summary(genderclassmodel)

# Any results you write to the current directory are saved as output.