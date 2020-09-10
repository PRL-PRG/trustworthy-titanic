# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages

# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats

# For example, here's several helpful packages to load in 

#library(ggplot2) # Data visualization
#library(readr) # CSV file I/O, e.g. the read_csv function
#library('ggthemes') # visualization
#library('ggridges') # visualization
#library('ggforce') # visualization
#library('ggExtra') # visualization
#library('GGally') # visualisation
#library('scales') # visualization
#library('grid') # visualisation
#library('gridExtra') # visualisation
#library('corrplot') # visualisation
#library('VIM') # missing values

# wrangle
#library('dplyr') # data manipulation
#library('tidyr') # data manipulation
library('readr') # data input
#library('stringr') # string manipulation
#library('forcats') # factor manipulation
#library('modelr') # factor manipulation

# model
#library('randomForest') # classification
#library('xgboost') # classification
#library('ROCR') # model validation
#library(data.table)
#library(knitr)


# Input data files are available in the "../input/" directory.

df.train <- read_csv('../input/train.csv')
df.test  <- read_csv('../input/test.csv')

# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory
system("ls ../input")


#For train

head(df.train)
str(df.train)
dim(df.train)
summary(df.train)
names(df.train)

#For test
head(df.test)
str(df.test)
dim(df.test)
summary(df.test)
names(df.test)

#Check missing value
#install.packages("Amelia")
library(Amelia)
?missmap

missmap(df.train, main = 'Missing Map', col = c('yellow','black'), legend = FALSE)

#Exploration
library(ggplot2)
ggplot(df.train, aes(Survived)) + geom_bar()
ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)))
ggplot(df.train,aes(Sex)) + geom_bar(aes(fill = factor(Sex)))
ggplot(df.train,aes(Age)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
ggplot(df.train,aes(SibSp)) + geom_bar()
ggplot(df.train,aes(Fare)) + geom_histogram(fill = 'green', color = 'black', alpha = 0.5)

#Impute the Missing value
pl <- ggplot(df.train,aes(Pclass,Age))
pl <- pl + geom_boxplot(aes(group = Pclass, fill = factor(Pclass), alpha = 0.4))
pl + scale_y_continuous(breaks = seq(min(0), max(80), by = 2)) + theme_bw()

impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)) {
    
    if (is.na(age[i])){
      
      if (class[i] == 1) {
        out[i]  <- 37
        
      }else if (class[i] == 2) {
        out[i] <- 29
        
      }else {
        out[i] <- 24
      }
      
    }else {
        out[i] <- age[i]
      }
      }
        return(out)
        
}

fixed.ages <- impute_age(df.train$Age,df.train$Pclass)
df.train$Age <- fixed.ages

missmap(df.train, main = 'Imputation Checks', col = c('yellow', 'black'), legend = FALSE)

####
#Remove unwanted column
library(dplyr)
df.train <- select(df.train, - PassengerId, -Name, -Ticket, -Cabin)
head(df.train)
str(df.train)

#Changing the column to factor
df.train$Survived <- as.factor(df.train$Survived)
df.train$Pclass <- as.factor(df.train$Pclass)
df.train$SibSp <- as.factor(df.train$SibSp)
df.train$Parch <- as.factor(df.train$Parch)

str(df.train)

###Run logistic regression
log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = df.train)
summary(log.model)

#####Split the data
library(caTools)
set.seed(101)
split <- sample.split(df.train$Survived, SplitRatio = 0.7)
final.train <- subset(df.train, split == TRUE)
final.test <- subset(df.train, split == FALSE)

final.log.model <- glm(Survived ~ ., family = binomial(link = 'logit'), data = final.train)
summary(final.log.model)

###Predict
fitted.probability <- predict(final.log.model, final.test, type = 'response')
fitted.results <- ifelse(fitted.probability>0.5, 1,0)

missclassError <- mean(fitted.results != final.test$Survived)
print(1- missclassError)

###Confusion Matrix
table(final.test$Survived,fitted.probability>0.5)

















