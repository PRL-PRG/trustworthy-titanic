
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(caret)
library(pROC)


# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

train <- read_csv('../input/train.csv')
test  <- read_csv('../input/test.csv')

library('dplyr') # data manipulation
library('ggplot2') # Data Visualization
library('ggthemes') # Data Visualization

options(warn = -1)
# load train.csv
train <- read.csv('../input/train.csv', stringsAsFactors = F)
# load test.csv
test  <- read.csv('../input/test.csv', stringsAsFactors = F)
# combine them as a whole
test$Survived <- NA
full <- rbind(train,test)

head(train)

#overall survival rate
sum(train$Survived)/length(train$Survived)

# Sex vs Survived
#male fare the worst
ggplot(train[1:891,], aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = 'dodge')+
  theme_few() +
  xlab("Sex") +
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Sex vs Survived")

#Pclass vs Survived 
ggplot(train[1:891,], aes(Pclass,fill = factor(Survived))) +
  geom_bar(stat = "count")+
  xlab('Pclass') +
  ylab("Count") +
  scale_fill_discrete(name = " Survived") + 
  ggtitle("Pclass vs Survived")+
  theme_few()

set.seed(2415784)
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

total  <- bind_rows(train, test) 

# Extract title based on proximiy to . in string
train$Title <- gsub('(.*, )|(\\..*)', '', train$Name) 
table(train$Title)

#no Rev survived
sum(train$Survived[train$Title=='Rev'])

#survival rate for people whose `Age` is empty
sum(subset(train,is.na(Age))$Survived)/length(subset(train,is.na(Age))$Survived)

#Embarked vs Survived
ggplot(train[1:891,], aes(Embarked,fill = factor(Survived))) +
  geom_bar(stat = "count")+
  xlab('Embarked') +
  ylab("Count") +
  scale_fill_discrete(name = " Survived") + 
  ggtitle("Embarked vs Survived")+
  theme_few()

full$Child[full$Age < 12] <- 'Child'
full$Child[full$Age >= 12] <- 'Adult'

ggplot(full[1:891,][full[1:891,]$Child == 'Child', ], aes(Pclass,fill = factor(Survived))) +
  geom_bar(stat = "count")+
  xlab('Pclass') +
  ylab("Count") +
  scale_fill_discrete(name = " Survived") + 
  ggtitle("Pclass vs Survived")+
  theme_few()

#Sadly, Capt Edward Smith went down with the ship. 
train$Survived[train$Title == 'Capt']

max(train$Fare[train$Survived == 0])
train$Name[train$Fare == max(train$Fare[train$Survived == 0])]

# Family Size
# It help when Family size == 2, 3, 4 and hurt when Family size exceeds 4.
full$Fsize <- full$SibSp + full$Parch + 1

ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  xlab('Family Size') +
  ylab("Count") +
  theme_few()+
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Family Size vs Survived")

# Step 1: Load + Clean Data
Train = read.csv("../input/train.csv")
Test = read.csv("../input/test.csv")

str(Train)
summary(Train)

# fill in missing values for Age
Train$Age[is.na(Train$Age)] = mean(Train$Age, na.rm = TRUE)
Test$Age[is.na(Test$Age)] = mean(Test$Age, na.rm = TRUE)


# Step 2: Create DF of independent/dependent variables
nonvars = c("PassengerId","Name","Ticket","Embarked","Cabin")
Train = Train[,!(names(Train) %in% nonvars)]
str(Train)


# Step 3: Build a Logistic Regression Model
TitanicLog1 = glm(Survived~., data = Train, family = binomial)
summary(TitanicLog1)

# Step 4: Revise Model
TitanicLog2 = glm(Survived ~ . - Parch, data = Train, family = binomial)
summary(TitanicLog2)

TitanicLog3 = glm(Survived ~ . - Parch - Fare, data = Train, family = binomial)
summary(TitanicLog3)


# Step 5: Test Accuracy of Model on Training Data
# predicted result of regression
ans_logit = rep(NA,891)
for(i in 1:891){
  ans_logit[i] =as.integer(round(TitanicLog2$fitted.values[[i]],0))
}

# check result
mean(ans_logit == train$Survived)
table(ans_logit)

table(train$Survived)

conf_mat <- caret::confusionMatrix(as.factor(ans_logit), as.factor(train$Survived))  #convert int to factor
conf_mat

library(ROCR) 
auc(train$Survived, ans_logit)
pred <- prediction(train$Survived, ans_logit)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
