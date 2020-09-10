# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 
# Load packages

library('ggplot2')
library('ggthemes')
library('scales')
library('dplyr')
library('mice')
library('randomForest')

train <- read.csv('../input/train.csv', stringsAsFactors = F)
test <- read.csv('../input/test.csv', stringsAsFactors = F)

full <- bind_rows(train, test)
str(full)

#Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
#show title counts by sex
table(full$Sex, full$Title)

# Combine titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess', 'Capt', 'Col', 'Don', 
'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme
full$Title[full$Title == 'Mlle'] <-'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <-'Mrs'
full$Title[full$Title %in% rare_title] <- 'Rare Title'

#Show title counts by sex again
table(full$Sex, full$Title)

#Finally, grab surname
full$Surname <- sapply(full$Name,
                    function(x) strsplit(x, split = '[,.]')[[1]][1])

cat(paste('We have <b>', nlevels(factor(full$Surname)), '</b> unique surnames.
I would be interested to infer ethnicity based on surname...'))

#Create family Size variable
full$Fsize <- full$SibSp + full$Parch +1

#Create Family variable
full$Family <- paste(full$Surname, full$Fsize, sep='_')

#Use ggpot2 to Visualize the relationship
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) + 
    geom_bar(stat='count', position='dodge')+
    scale_x_continuous(breaks=c(1:11)) + 
    labs(x = 'Family Size') +
    theme_few()