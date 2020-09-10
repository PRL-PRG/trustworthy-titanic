# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
library(rpart)
library(RGtk2)
library(RGtk2)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major')] <- 'Sir'
combi$Title[combi$Title %in% c('Miss', 'Mlle')] <- 'Ms'
combi$Title[combi$Title %in% c('Jonkheer', 'Mr')] <- 'Mr'
combi$Title[combi$Title == 'Mme'] <- 'Mrs'
combi$Title <- factor(combi$Title)

#create family tags
combi$Name <- as.character(combi$Name)
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilySize <- combi$Parch + combi$SibSp +1
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, '')
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
FamId <- data.frame(table(combi$FamilyID))
FamId <- FamId[FamId$Freq <= 2,]
combi$FamilyID[combi$FamilyID %in% FamId$Var1] <- 'Small'
combi$FamilyID <- gsub(' ', '', combi$FamilyID)
combi$FamilyID <- factor(combi$FamilyID)

#create family survived factor (might not help, but don't think it will hurt)
combi$FamSurvived <- 'None'
FamId <- data.frame(table(combi$FamilyID[combi$Survived == 1]))
FamId <- FamId[FamId$Freq > 0,]
FamId <- FamId[FamId$Freq < 200,]
combi$FamSurvived[combi$FamilyID %in% FamId$Var1] <- 'yes'

combi$FamSurvived <- factor(combi$FamSurvived)

#cabin Deck tag (still not sure how to extrapolate...)
combi$Deck <- sapply(as.character(combi$Cabin), FUN=function(x) {strsplit(x, "")[[1]][1]})
#deckid <- data.frame('FamilyID' = combi$FamilyID[!is.na(combi$Deck) & combi$FamilyID != 'Small'], 'Deck' = as.character(combi$Deck[!is.na(combi$Deck) & combi$FamilyID != 'Small']))
#deckid <- deckid[!duplicated(deckid),]
#combi$Deck[combi$FamilyID %in% deckid$FamilyID] <- sapply(combi$FamilyID[combi$FamilyID %in% deckid$FamilyID & is.na(combi$Deck)], FUN=function(x) {deckid$Deck[deckid$FamilyID == x]})
combi$Deck <- factor(combi$Deck)
#combi$Deck[!is.na(combi$Deck) & combi$FamilyID == x]
#summary(combi$Deck)

Dfit <- rpart(as.factor(Deck) ~ Pclass + Fare + Title + Embarked + FamilySize + Survived,
               data=combi[!is.na(combi$Deck),], 
               method="class",
               control=rpart.control(minsplit = 5, cp = .015))
combi$Deck <- factor(combi$Deck)
combi$Deck[is.na(combi$Deck)] <- predict(Dfit, combi[is.na(combi$Deck),])


#cabin number tag
combi$rmnmbr <- sapply(as.character(combi$Cabin), FUN=function(x){substring(x,2,5)})
combi$rmnmbr <- gsub('[^0-9]', '', combi$rmnmbr)
combi$rmnmbr <- as.integer(as.character(combi$rmnmbr))

rmfit <- rpart(rmnmbr ~ Deck + Pclass + Fare + Title + Embarked + FamilySize + Survived, data=combi[!is.na(combi$rmnmbr),], method="anova", control=rpart.control(minsplit = 5, cp = .008))
combi$rmnmbr[is.na(combi$rmnmbr)] <- predict(rmfit, combi[is.na(combi$rmnmbr),])


train <- combi[1:891,]
test <- combi[892:1309,]

#set.seed(27)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + Fare + Embarked + Title + FamilySize + FamilyID + FamSurvived + rmnmbr + Deck, data=train, controls=cforest_unbiased(trace = TRUE, ntree=100, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "rmnmbr2.csv", row.names = FALSE)