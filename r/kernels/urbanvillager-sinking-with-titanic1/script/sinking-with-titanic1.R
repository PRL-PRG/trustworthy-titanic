
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(party)
library(grid)
library(mvtnorm)
library(modeltools)
library(stats4)
library(strucchange)
library(zoo)
library(sandwich)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][2]})
combi$Title <- sub(' ','',combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][1]})
combi$FamilyId <- paste(as.character(combi$FamilySize), combi$Surname, sep = '')
combi$FamilyId[combi$FamilySize <= 2] <- 'Small'
famId <- data.frame(table(combi$FamilyId))
famId <- famId[famId$Freq <= 2, ]
combi$FamilyId[combi$FamilyId %in% famId$Var1] <- 'Small'
combi$FamilyId <- factor(combi$FamilyId)
train <- combi[1:891,]
test <- combi[892:1309,]
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Title + FamilySize + FamilyId + Ticket,data = train,controls=cforest_unbiased(ntree=2000, mtry=3))
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)