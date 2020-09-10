setwd("../input")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)


PassengerId_Test <- test[,1]

test$Survived <- NA
master <- rbind(train, test)
master$Name <- as.character(master$Name)

master$Title <- sapply(master$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
master$Title <- sub(' ', '', master$Title)

master$Title[master$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
master$Title[master$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
master$Title <- factor(master$Title)
master$FamilySize <- master$SibSp + master$Parch + 1

master$Surname <- sapply(master$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
master$FamilyID <- paste(as.character(master$FamilySize), master$Surname, sep="")
master$FamilyID[master$FamilySize <= 2] <- 'Small'

famIDs <- data.frame(table(master$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2,]
master$FamilyID[master$FamilyID %in% famIDs$Var1] <- 'Small'
master$FamilyID <- factor(master$FamilyID)


library(rpart)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=master[!is.na(master$Age),], method="anova")
master$Age[is.na(master$Age)] <- predict(Agefit, master[is.na(master$Age),])

master$Embarked[c(62,830)] = "S"
master$Embarked <- factor(master$Embarked)
master$Sex <- factor(master$Sex)

master$Fare[1044] <- median(master$Fare, na.rm=TRUE)

# The first character is the deck. For example:
# strsplit(master$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
master$Deck<-factor(sapply(master$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


library(stringr) 

master$Ticket <- as.character(master$Ticket)
master$TktPre <- str_extract(master$Ticket, "[^\\d]+")

master$TktPre <- factor(master$TktPre)
master$Ticket <- factor(master$Ticket)

master$TktPre <- as.character(master$TktPre)
master$TktPre[is.na(master$TktPre)] <- "F"
master$TktPre <- factor(master$TktPre)

master$Cabin <- as.character(master$Cabin)
master$cabinClass <- substring(master$Cabin, 1, 1)
master$cabinClass <- factor(master$cabinClass)
master$Cabin <- factor(master$Cabin)


#train <- master[1:891,]
#test <- master[892:1309,]

#predictor_variables <- c("Survived", "Pclass", "Cabin", "Sex", "cabinClass",  "Age",  "SibSp",  "Deck", "Parch",  "Fare",  "Embarked",  "Title",  "FamilySize",  "FamilyID", "TktPre")

#predictor_variables <- c("Survived", "Pclass",  "Sex",   "Age",  "SibSp",  "Deck", "Parch",  "Fare",  "Embarked",  "Title",  "FamilySize",  "FamilyID", "TktPre")

predictor_variables <- c("Survived", "Pclass",  "Sex", "cabinClass",  "Age",  "SibSp",  "Deck", "Parch",  "Fare",  "Embarked",  "Title",  "FamilySize",  "FamilyID", "TktPre")

master <- master[, predictor_variables]

library(class)
library(caret)
library(knncat)
library(dummies)

#system("ls ../input")



