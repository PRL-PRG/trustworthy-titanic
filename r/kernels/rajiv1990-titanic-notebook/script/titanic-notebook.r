
# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

from subprocess import check_output
print(check_output(["ls", "../input"]).decode("utf8"))

# Any results you write to the current directory are saved as output.

from datetime import timedelta, date

nhl_teams = ["ANA", "ARI", "ATL", "BOS", "BUF","CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM", "FLA", "LA", "MIN","MTL",
             "NSH","NJ", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "WSH", "WPG"]
             

all_urls = []

# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

all_data  <- bind_rows(train, test) #use dpylr's bind_rows when columns aren't the same across datasets

str(all_data) #check the structure of the data 

# Grab title from passenger names
all_data$Title <- gsub('(.*, )|(\\..*)', '', all_data$Name)

table(all_data$Sex, all_data$Title)

#create a variable that indicates family size 

all_data$Fam_Size = all_data$SibSp + all_data$Parch + 1

ggplot(all_data[1:891,], aes(x = Fam_Size, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

#What about survival by gender? 
ggplot(all_data[1:891,], aes(x = Sex, fill = factor(Survived))) +
  geom_bar(stat='count') +
  labs(x = 'Gender') 

##The Cabin variable appears to have missing values: 
#is.na(all_data$Cabin)
#We'll deal with that later...


    
    

all_data[c(62, 830), 'Pclass']

# Create a Deck variable. Get passenger deck A - F:
all_data$Deck<-factor(sapply(all_data$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
    
all_data$Deck[2]

embark_fare <- all_data %>%
  filter(PassengerId != 62 & PassengerId != 830)

embark_fare <- all_data %>%
  filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80),
    colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()# %>% takes the value on the left and pass it to the right as an argument 
embark_fare <- all_data %>%
    filter(PassengerId != 62 & PassengerId != 830)

all_data$Embarked[c(62, 830)] <- 'C'

#Passenger 1044 has NA for fare, survived, and deck 
all_data[1044,]

#1044 was in 3rd class and embarked from "S". 


sum(is.na(all_data$Age))

#Visualizing the variables with missing observations 
library(VIM)
aggr_plot <- aggr(all_data, col=c('cornflower blue','red'), numbers=TRUE, sortVars=TRUE, labels=names(all_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

#Visualizing the variables with missing observations 
library(VIM)
aggr_plot <- aggr(all_data, col=c('cornflower blue','red'), numbers=TRUE, sortVars=TRUE, labels=names(all_data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
