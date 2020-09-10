
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gbm)

library(caret)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

train_data = fread('../input/train.csv', na.strings = "")
test_data = fread('../input/test.csv', na.strings = "")

data = rbind(train_data, test_data, fill = T)
summary(data)

colSums(is.na(data))

numerical_data = data[,c("Survived", "Age", "SibSp", "Parch", "Fare") ]
corr_mat = round(cor(numerical_data, use = "na.or.complete"), 2)
head(corr_mat)

melted_cormat <- melt(corr_mat)
head(melted_cormat)
ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() + 
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
        limit = c(-1,1), space = "Lab", name="Pearson\nCorrelation") + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)


#### Siblings Spouse ####

ggplot(data = train_data, aes(x = SibSp, fill = as.factor(Survived))) + 
geom_bar(position = 'fill') + 
labs(y = "Survival Probability", fill = "Survival Probability")

#### Parent Child ####
ggplot(data = train_data, aes(x = Parch, fill = as.factor(Survived))) + 
geom_bar(position = "fill") + 
labs(y = "Survival Probability", fill = "Survival Probability")

#### Age ####
ggplot(data = train_data, aes(x = Age, fill = as.factor(Survived))) + 
geom_density(alpha = 0.6) + 
labs(fill = "Survival Probability")

ggplot(data = train_data, aes(x = Age)) + 
geom_density(alpha = 0.6) + facet_wrap(~ as.factor(Survived)) +
labs(fill = "Survival Probability")

#### Fare ####
sum(is.na(data$Fare))
data$Fare[is.na(data$Fare)] = median(data$Fare, na.rm = TRUE)

ggplot(data, aes(x = Fare)) + 
geom_histogram(aes(y = ..density..), bins = 50, boundary = 0) +
geom_density()

data$LogFare = ifelse(data$Fare > 0, log(data$Fare), 0)

ggplot(data, aes(x = LogFare)) + 
geom_histogram(aes(y = ..density..), bins = 50, boundary = 0) +
geom_density()


#### Pclass vs Survived With Sex ####
ggplot(train_data, aes(x = as.factor(Sex), fill = as.factor(Survived))) + 
    geom_bar(position = 'fill') + 
    facet_wrap(~Pclass) +
    labs(y = 'Survival Probability', x = 'Gender', fill = 'Survived')

#### Pclass vs Survived ####
ggplot(train_data, aes(x = Pclass, fill = as.factor(Survived))) + 
    geom_bar(position = 'fill') +
    labs(y = 'Survival Probability', fill = 'Survived')


#### Impute Embarked Data ####
data$Embarked[is.na(data$Embarked)] = "S"
train_data = data %>% filter(!is.na(Survived))
#table(train_data$Embarked)

#### Embarked Data With Pclass Split ###
ggplot(train_data, aes(x = Pclass, fill = as.factor(Pclass))) + 
    geom_bar(position = 'dodge') + 
    facet_wrap(~Embarked) + 
    labs(fill = 'Pclass')

ggplot(train_data, aes(x = Pclass, fill = as.factor(Survived))) + 
    geom_bar(position = 'fill') + 
    facet_wrap(~Embarked) + 
    labs(y = 'Survival Probability', fill = 'Survived')

#### Embarked Data ####
ggplot(train_data, aes(x = Embarked, fill = as.factor(Survived))) + 
    geom_bar(position = 'fill') + 
    labs(y = 'Survival Probability', fill = 'Survived')

colSums(is.na(data))

#### SibSp Age Distribution ####
ggplot(data, aes(x = as.factor(SibSp), y = Age, fill = SibSp)) + 
    geom_boxplot() +
    labs(x = 'SibSp')

#### Parch Age Distribution ####
ggplot(data, aes(x = as.factor(Parch), y = Age, fill = Parch)) + 
    geom_boxplot() +
    labs(x = 'Parch')

#### Gender Class Age Ditribution ####
ggplot(data, aes(x = as.factor(Pclass), y = Age, fill = Pclass)) + 
    geom_boxplot() +
    facet_wrap(~Sex) + 
    labs(x = 'Pclass')

#### Gender Age Ditribution ####
ggplot(data, aes(x = Sex, y = Age, fill = Sex)) + 
    geom_boxplot()

## Impute Age ##
median_age = median(data$Age, na.rm = T)
age_estimate_feature_bucket = data %>% group_by(Parch, SibSp, Pclass) %>% summarise(age_est = median(Age, na.rm = T))
age_estimate_feature_bucket$age_est[is.na(age_estimate_feature_bucket$age_est)] = median_age

imputed_data = merge(data, age_estimate_feature_bucket, by = c('Parch', 'SibSp', 'Pclass'))
imputed_data$Age = imputed_data$age_est

dim(imputed_data)
sum(is.na(imputed_data$age_est))
colnames(imputed_data)

title_extract = function(name) strsplit(strsplit(name, ',')[[1]][2], '\\.')[[1]][1]
imputed_data$Title =  sapply(imputed_data$Name, title_extract)

RareTitleList = c(' Don', ' Major', ' Dr',  ' Col', ' the Countess', ' Jonkheer', ' Dona', ' Rev', ' Lady', ' Sir', ' Capt')
MarriedFemaleTitleList = c( ' Mrs', ' Mme', ' Mlle', ' Ms')
imputed_data = imputed_data %>% 
            mutate(title = ifelse(Title %in% RareTitleList, "Rare", ifelse(Title %in% MarriedFemaleTitleList, "Mrs/Mme", Title)))
imputed_train_data = imputed_data %>% filter(!is.na(Survived))
table(imputed_train_data$Survived)
unique(imputed_data$title)

ggplot(data = imputed_data, aes(x = as.factor(title))) + 
    geom_bar() + 
    labs(x = 'Title') + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = imputed_train_data, aes(x = as.factor(title), fill = as.factor(Survived))) + 
    geom_bar(position = 'fill') + 
    labs(x = 'Title', y = 'Survival Probability', fill = 'Survived')


imputed_data$FamilySize = imputed_data$Parch + imputed_data$SibSp + 1

imputed_train_data = imputed_data %>% filter(!is.na(Survived))
ggplot(data = imputed_train_data, aes(x = as.factor(FamilySize), fill = as.factor(Survived))) + 
    geom_bar(position = 'fill') + 
    labs(fill = 'Survived', x = 'FamilySize')


sum(is.na(imputed_data$Cabin))
imputed_data = imputed_data %>% mutate(Deck = substr(Cabin, 1, 1))
imputed_data$Deck[is.na(imputed_data$Deck)] = 'X'
imputed_train_data = imputed_data %>% filter(!is.na(Survived))

ggplot(data = imputed_train_data, aes(x = as.factor(Deck), fill = as.factor(Survived))) + 
    geom_bar(position = 'fill') + 
    labs(fill = 'Survived', x = 'Deck')

imputed_data = imputed_data %>% 
                mutate(TicketPrefix = ifelse(grepl('^[[:digit:]]+\\.*[[:digit:]]*$',Ticket), 'X', gsub('([[:digit:]]+)','',Ticket))) %>%
                mutate(TicketPrefix = gsub('\\.| |/','',TicketPrefix))
table(imputed_data$TicketPrefix)
colnames(imputed_data)

train_data = imputed_data %>% filter(!is.na(Survived))
test_data = imputed_data %>% filter(is.na(Survived))

dim(train_data)
dim(test_data)

table(train_data$Survived)

## Remove Redundant Columns ##
drop_cols = c('Title', 'age_est', 'Ticket', 'Name', 'Cabin', 'PassengerId', 'Name')
imputed_data = imputed_data %>% select(-one_of(drop_cols))
str(imputed_data)

factor_colnames = c("Sex", "Embarked", "title", "Deck", "TicketPrefix")
class(imputed_data[factor_colnames[1]])
for(factor_colname in factor_colnames) {
    imputed_data[, factor_colname] = as.factor(imputed_data[,factor_colname])
}
str(imputed_data)

## Create Dummy Variables ##
dmy <- dummyVars(" ~ .", data = imputed_data,fullRank = T)
dataset = data.frame(predict(dmy, imputed_data))

dim(dataset)
train = dataset[!is.na(dataset[, 'Survived']), ]
test = dataset[is.na(dataset[, 'Survived']), ]

dim(train)
dim(test)

summary(test)

## Create train and Validation Split
index <- createDataPartition(train$Survived, p=0.75, list=FALSE)
trainSet <- train[index,]
testSet <- train[-index,]

summary(testSet)


## Feature selection using rfe 
control <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 3, verbose = FALSE)
outcomeName <- 'Survived'
predictors <- names(trainSet)[!names(trainSet) %in% outcomeName]

#head(trainSet[, predictors])
#head(trainSet[, outcomeName])
survivalPredProfile <- rfe(trainSet[, predictors], as.factor(trainSet[,outcomeName]), rfeControl = control)

attributes(survivalPredProfile)
survivalPredProfile$optVariables

numVars = 5
topVariables = survivalPredProfile$optVariables[1:numVars]
topVariables

objControl = trainControl(method = 'repeatedcv',number = 5, repeats = 5)

model_gbm <- train(trainSet[,topVariables], as.factor(trainSet[,outcomeName]), method='gbm', trControl = objControl, tuneLength = 10, verbose = F)
#model_rf<-train(trainSet[,top5Variables], as.factor(trainSet[,outcomeName]),method='rf')
#model_nnet<-train(trainSet[,top5Variables], as.factor(trainSet[,outcomeName]),method='nnet')
#model_glm<-train(trainSet[,top5Variables], as.factor(trainSet[,outcomeName]),method='glm')
model_gbm

#attributes(survivalPredProfile)
#x = survivalPredProfile$results

#attributes(x)
#x
#survivalPredProfile

plot(model_gbm)
varImp(model_gbm)

testPred = predict(model_gbm, type = "raw", newdata = testSet)
length(testPred)
sum(as.numeric(testPred) - 1)


