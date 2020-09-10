## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----package loading-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages

library(ggplot2) # data visualization
library(dplyr) # data manipulation


## ----load data-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load the datasets
df.train <- read.csv('../input/train.csv', stringsAsFactors = FALSE)
df.test <- read.csv('../input/test.csv', stringsAsFactors = FALSE)

# Combine both datasets into one data frame
df.full <- bind_rows(df.train, df.test)

# See what the dataset contains!
str(df.full)


## ----survival--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# How many in the train dataset survived?
table(df.full$Survived)

# Factorize Survived
df.full$Survived <- factor(df.full$Survived)


## ----pclass----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# What values does Pclass contain?
table(df.full$Pclass)


## ----pclass exploration----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Factorize Pclass
df.full$Pclass <- factor(df.full$Pclass)

# Use ggplot to visualize relationship between Pclass & Survived
ggplot(df.full[1:891,], aes(x = Pclass, fill = Survived)) +
    geom_bar(stat='count', position='dodge', color = 'black') +
    scale_x_discrete(breaks=c(1:3)) +
    labs(x = 'Passenger Class') + theme_minimal()


## ----upper vs lower--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df.full$TicketClass[df.full$Pclass %in% c(1, 2)] <- 'upper'
df.full$TicketClass[df.full$Pclass == 3] <- 'lower'
df.full$TicketClass <- factor(df.full$TicketClass)

table(df.full$TicketClass)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Factorize Sex
df.full$Sex <- factor(df.full$Sex)

ggplot(df.full[1:891,], aes(x = Sex, fill = Survived)) +
    geom_bar(stat='count', position='dodge', color = 'black') +
    scale_x_discrete() + labs(x = 'Sex') + theme_minimal()


## ----age hist--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(df.full[1:891,], aes(x = Age, fill = Survived)) + 
    geom_histogram(binwidth = 5, color = 'black') +
    theme_minimal()


## ----age hist sex----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(df.full[1:891,], aes(x = Age, fill = Survived)) + 
    geom_histogram(binwidth = 5, color = 'black') +
    theme_minimal() + facet_wrap( ~ Sex, ncol = 2)


## ----missing age-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Number of observations missing age
sum(is.na(df.full$Age))


## ----missing age children--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# How many passengers have no age recorded but are likely children?
length(df.full$Age[is.na(df.full$Age) & df.full$SibSp > 1])


## ----children vs. adults---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a column distinguishing child or adult
df.full$ChildAdult[is.na(df.full$Age) & df.full$SibSp > 1] <- 'child'


## ----siblings and spouses--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(df.full[1:891,], aes(x = SibSp, fill = Survived)) + 
    geom_bar(stat='count', position='dodge', color = 'black') +
    scale_x_continuous() + labs(x = '# of Siblings/Spouses') + theme_minimal()


## ----family size-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create the family size feature
df.full$FamilySize <- df.full$SibSp + df.full$Parch + 1

# Plot family size/survival histogram
ggplot(df.full[1:891,], aes(x = FamilySize, fill = Survived)) + 
    geom_bar(stat='count', position='dodge', color = 'black') +
    scale_x_continuous(breaks = 1:11) + labs(x = 'Family Size') + theme_minimal()


## ----FamilySizeDiscrete----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create the feature FamilySizeDiscrete, based on the previous plot
df.full$FamilySizeDiscrete[df.full$FamilySize == 1] <- 'single'
df.full$FamilySizeDiscrete[df.full$FamilySize > 1 & df.full$FamilySize < 5] <- 'small'
df.full$FamilySizeDiscrete[df.full$FamilySize > 4] <- 'large'

# Now make this feature a factor
df.full$FamilySizeDiscrete <- factor(df.full$FamilySizeDiscrete)

# Show this how this new variable influences surviving
ggplot(df.full[1:891,], aes(x = FamilySizeDiscrete, fill = Survived)) + 
    geom_bar(stat='count', position='dodge', color = 'black') +
    scale_x_discrete() + labs(x = 'Family Size') + theme_minimal()


## ----ticket----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Preview of the Ticket feature
head(df.full$Ticket)

