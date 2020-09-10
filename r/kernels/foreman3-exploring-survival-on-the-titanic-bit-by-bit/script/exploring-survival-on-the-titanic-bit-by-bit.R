## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # bind training & test data

# check data
str(full)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)

royal_title <- c('Lady', 'the Countess', 'Sir')
foreign_title <- c('Dona', 'Don', 'Jonkheer')
military_title <- c('Capt', 'Col', 'Major')

# Titles with very low cell counts to be combined to "rare" level
# BOB's TWEEK - Not including Rev in this list, based on another's recommendation... Good Revs didn't make it


rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Sir', 'Jonkheer')


# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% royal_title]  <- 'Royal'
full$Title[full$Title %in% military_title]  <- 'Military'
full$Title[full$Title %in% foreign_title]  <- 'Exotic'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                      function(x) strsplit(x, split = '[,.]')[[1]][1])


## ----results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cat(paste('We have <b>', nlevels(factor(full$Surname)), '</b> unique surnames. I would be interested to infer ethnicity 
based on surname (Said Megan - Id have no idea how to do that)'))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use ggplot2 to visualize the relationship between Title & survival
ggplot(full[1:891,], aes(x = Title, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Title') +
  theme_few()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize > 1] <- 'pairs'
full$FsizeD[full$Fsize > 2] <- 'micro'
full$FsizeD[full$Fsize > 4] <- 'mid'
full$FsizeD[full$Fsize > 7] <- 'large'


# Show family size by survival using the same plot types

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = FsizeD, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Family Size D') +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# This variable appears to have a lot of missing values
full$Cabin[1:28]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Deck, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Deck') +
  theme_few()
  
# Hum, who has a known Deck?
table(full$Pclass, full$Deck)


#Did we look at survival by Class?
# Use ggplot2 to visualize the relationship between Class & survival
ggplot(full[1:891,], aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Class') +
  theme_few()
  
  
#How may did the classes do, that had a known cabin?
table(full$Survived[!is.na(full$Deck)], full$Pclass[!is.na(full$Deck)])
  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Passengers 62 and 830 are missing Embarkment
full[c(62, 830, 1, 2, 3), 'Embarked']
full[c(62, 830, 1, 2, 3), 'Fare']
full[c(62, 830, 1, 2, 3), 'Age']
full[c(62, 830, 1, 2, 3), 'Pclass']
full[c(62, 830, 1, 2, 3), 'Deck']


## ----results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cat(paste('We will infer their values for **embarkment** based on present data that we can imagine
may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1],
'</b>and<b> $', full[c(62, 830), 'Fare'][[2]][1], '</b>respectively and their classes are<b>', full[c(62, 830),
'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[2]][1], '</b>. So from where did they embark?'))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
    colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

