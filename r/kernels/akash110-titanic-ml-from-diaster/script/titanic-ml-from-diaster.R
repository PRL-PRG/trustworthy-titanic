## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
suppressWarnings(suppressMessages(library(DMwR)))
library(DMwR)
library(randomForest)
# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

#train <- read.csv("train.csv")
#test <- read.csv("test.csv")

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Training data set
head(train[,1:3],2)
head(train[,4:7],2)
head(train[,8:12],2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Test data set - No "Survived" column
head(test[,1:3],2)
head(test[,4:7],2)
head(test[,8:11],2)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train[c(6,18,20),4:7]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test$Survived <- NA
merge_data <-  rbind(train,test)
#Number of rows in each data-set
nrow(train)       # 891
nrow(test)        # 418
nrow(merge_data)  #1309
#Applying KNN algorithm
knnOutput <- knnImputation(merge_data[, !names(merge_data) %in% "Survived"])
merge_data <- cbind.data.frame(knnOutput,Survived = merge_data$Survived) 
merge_data$Age <- as.integer(merge_data$Age)
merge_data[c(6,18,20),4:7]      #Filled NA values


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Extracting meaning from name of the person in the data
name <- as.character(merge_data$Name)

title <-sapply(name, FUN =  function(x) {(strsplit(x,split = '[,.]'))[[1]][2]})
title <- sub(' ','',title)

surname <- sapply(name, FUN =  function(x) {(strsplit(x,split = '[,.]'))[[1]][1]})

table(title)  #Original titles
title <- as.vector(title)

title[title %in% c('Capt','Don','Major', 'Sir')] <- 'Sir' 
title[title %in% c('Dona','Lady','the Countess', 'Jonkheer','Mlle','Mme')] <- 'Lady' 
title[title %in% 'Ms'] <- 'Miss'
title <- as.factor(title);

table(title)  #Grouped Titles

merge_data$Title <- title

#Including family size

merge_data$familySize <- NA
merge_data$familySize <- merge_data$SibSp + merge_data$Parch + 1

merge_data$family_set <- paste(as.character(merge_data$familySize), surname, sep = "")
merge_data$family_set[merge_data$familySize <3] <- 'small'
char <- merge_data$family_set


famIssue <- data.frame(table(merge_data$family_set))
famIssue <- famIssue[famIssue$Freq<3,]

char1 <- famIssue$Var1
char[char %in% char1] <- 'small'

merge_data$family_set <- char
head(merge_data$family_set)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merge_data$family_set<- as.factor(merge_data$family_set)
train <-  merge_data[1:nrow(train),]
test <- merge_data[(nrow(train) + 1):nrow(merge_data),]
nrow(train)
nrow(test)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Conditional Random Forest
suppressWarnings(suppressMessages(library(party)))
library(party)
set.seed(755)

fit <- cforest(as.factor(Survived) ~ Sex + Age + Pclass + SibSp + Parch + Fare +
                      Title + Embarked + familySize + family_set,
                    data = train, controls = cforest_unbiased(ntree = 2000, mtry=3))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pre <- predict(fit, test, OOB=TRUE, type = "response")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predictions <- data.frame(PassengerId = test$PassengerId, Survived = pre)
write.csv(predictions, file = "myoutput.csv", row.names = FALSE)


