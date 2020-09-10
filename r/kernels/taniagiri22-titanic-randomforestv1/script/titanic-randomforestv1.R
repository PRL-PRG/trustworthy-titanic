# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr)
library(stringr)
library(ggthemes)
library(scales)
library(mice)
library(randomForest)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")
test<-read.csv("../input/test.csv")
train<-read.csv("../input/train.csv")

# Any results you write to the current directory are saved as output.
full<-bind_rows(train,test)
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex,full$Title)
table(full$Survived,full$Title)

###grouping less frequent titles are rare title####
rare_title=c("Capt","Col","Don","Dona","Dr","Jonkheer","Lady","Major","Rev",
             "Sir","the Countess")

full$Title[full$Title=='Mme']<-"Mrs"
full$Title[full$Title=='Mlle']<-"Miss"
full$Title[full$Title=='Ms']<-"Miss"
full$Title[full$Title %in% rare_title]<-"Rare Title"

full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
                       
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

full$FsizeD[full$Fsize==1]<-'singleton'
full$FsizeD[full$Fsize>1 & full$Fsize<5]<-'small'

full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

strsplit(full$Cabin[2],NULL)[[1]]
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

sapply(full, function(x) sum(is.na(x)))
sapply(full, function(x) sum(as.character(x)==""))

full[c(62, 830), 'Embarked']

embark_fare <- full %>%filter(PassengerId != 62 & PassengerId != 830)

ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) + theme_few()



full$Embarked[c(62, 830)] <- 'C'
full[1044, ]

full$Fare[1044] <- median(full[full$Pclass == '3' & 
                  full$Embarked == 'S', ]$Fare, na.rm = TRUE)
                  

sum(is.na(full$Age))
mean(full$Age,na.rm=TRUE)

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% 
c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 



mice_output <- complete(mice_mod)

full$Age <- mice_output$Age


#Creating a derived variable: if child + single mother do they survive?
full$Child[full$Age<18]<-'Child'
full$Child[full$Age>=18]<-'Adult'

# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & 
              full$Title != 'Miss'] <- 'Mother'
              
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

train <- full[1:891,]
test <- full[892:1309,]

rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked + Title + 
                           FsizeD + Child + Mother,
                         data = train)
