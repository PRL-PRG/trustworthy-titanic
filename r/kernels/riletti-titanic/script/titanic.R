library("ggplot2")
library("ggthemes")
train = read.csv('../input/train.csv')
# calculate the party size based upon number of siblings + number of parents / children + self
train$partySize = train$SibSp + train$Parch + 1

train$title <- gsub('(.*, )|(\\..*)', '', train$Name)
length(train$Sex)
length(train$title)

table(train$Sex, train$title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

train$title[train$title == 'Mlle']        <- 'Miss' 
train$title[train$title == 'Ms']          <- 'Miss'
train$title[train$title == 'Mme']         <- 'Mrs' 
train$title[train$title %in% rare_title]  <- 'Rare Title'
table(train$Sex, train$title)
