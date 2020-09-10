# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input/train.csv")
list.files("../input/test.csv")
list.files("../input/gender_submission.csv")

# Any results you write to the current directory are saved as output.
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv") 

all <- bind_rows(train, test) 

str(all)

#stripped all title from the name column and place them on a new column called Title

all$Title <- gsub('(.*, )|(\\..*)', '', all$Name) 

table(all$Sex, all$Title) 

#Combined all rare title and stored them on a variable called rare_title 

rare_title <- c('Capt', 'Col', 'Don', 'Dona', 'Dr', 'Jonkheer', 'Lady', 'Major', 'Rev', 'Sir', 'the Countess')

all$Title[all$Title == 'Mlle'] <- 'Miss'
all$Title[all$Title == 'Ms'] <- 'Miss' 
all$Title[all$Title == 'Mme'] <- 'Mrs'
all$Title[all$Title %in% rare_title] <- 'Rare Title' 

table(all$Sex, all$Title)


















