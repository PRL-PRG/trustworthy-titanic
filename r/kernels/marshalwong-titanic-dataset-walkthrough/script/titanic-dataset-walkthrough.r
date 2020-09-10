
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(tidyverse)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

## File
file <- "../input/train.csv"
 
## Create connection
con <- file(description=file, open="r")

## read the first five lines
readLines(con, n = 5)
close(con)

train <- read_csv("../input/train.csv")

summary(train)

count(train)
summary(duplicated(train$PassengerId))

train %>% group_by(Survived) %>% summarise(count = n())

train %>% group_by(Pclass) %>% summarise(count = n())

library(stringr)

## max string length
train %>% str_length() %>% max()

## min string length
train %>% str_length() %>% min()

train %>% group_by(Sex) %>% summarise(count = n())

## Get the distinct levels from the Sex column
sex_summary <- train %>% group_by(Sex) %>% summarise(count = n())
sex_summary$Sex

## Convert the Sex column to a factor
train$Sex <- factor(train$Sex, levels = sex_summary$Sex)
class(train$Sex)

filter(train, is.na(Age))
