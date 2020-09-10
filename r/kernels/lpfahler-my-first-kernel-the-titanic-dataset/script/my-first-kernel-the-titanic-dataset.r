
# load packages

library(ggplot2)
library(readr) 
library('ggthemes') 
library('scales') # v
library('dplyr') # data manipulation
library('randomForest') # classification algorithm

# Input data files 
getwd()
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)
full  <- bind_rows(train, test) # bind training & test data


# check data
# head(full)
# Use Old School base graphics
# hist(full$Age, xlab="Age (years)", main="Histogram of Age - Titanic Passengers")
# hist(full$Fare, xlab="Passenger fare", main="Histogram of Face - Titanic Data")
# trying out ggplot2
plot1 <- ggplot(full, aes(x = Age)) +
  geom_histogram(col="black", fill="light green")
# trying various approaches to removing grid background and lines
# plot1 + theme_bw()
# plot1 + theme_bw() +
#   theme(
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank() 
#   )
plot1 + theme_bw() +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
  )
plot2 <- ggplot(full, aes(x = Fare)) +
  geom_histogram(col="black", fill="light green")
plot2 + theme_bw()
# plot2 + theme_classic()

plot3 <- ggplot(full, aes(x = Fare, color=Sex)) +
  geom_histogram() + theme_few()
plot3

plot4 <- ggplot(full, aes(x = Age)) +
  geom_histogram(col="black", fill="light green") + facet_wrap(~Sex) + theme_few()
plot4

plot6 <- ggplot(train, aes(x=as.factor(Survived), y=Age, 
          fill=as.factor(Survived))) + geom_boxplot() + guides(fill=FALSE) + 
          coord_flip() + theme_few() + labs(y="Age (years)", x="Survived (1=Yes, 0=No)")
plot6

plot7 <- ggplot(train, aes(x=as.factor(Survived), y=Fare, 
          fill=as.factor(Survived))) + geom_boxplot() + guides(fill=FALSE) + 
          coord_flip() + theme_few() + labs(y="Fare", x="Survived (1=Yes, 0=No)")
plot7

# missing data count
# need to address age!
sapply(full, function(x) sum(is.na(x)))

# Feature engineering borrowed from Megan Risdal
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1
# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# names
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
# Show title counts by sex
table(full$Sex, full$Title)
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
# Show title counts by sex again
table(full$Sex, full$Title)
# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
     function(x) strsplit(x, split = '[,.]')[[1]][1])

# embarked - see https://www.kaggle.com/mrisdal/titanic/exploring-survival-on-the-titanic
# for explanation
full$Embarked[c(62, 830)] <- 'C'
# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# impute missing age values
library('mice') 
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
# Set a random seed
set.seed(129)
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId',
                 'Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
