# For exploration check my other kernel 
# this one I will focus on pass the 0.81 score mark

# Necessary Libraries 
suppressMessages(library(randomForest))
suppressMessages(library(dplyr))
suppressMessages(library(rpart))
suppressMessages(library(party))

# for validate missing data 
suppressMessages(library(mice)) 
suppressMessages(library(VIM))


# load train.csv
DstTrain <- read.csv('../input/train.csv', stringsAsFactors = FALSE, na.strings = c("NA", ""))
# load test.csv
DstTest  <- read.csv('../input/test.csv', stringsAsFactors = FALSE, na.strings = c("NA", ""))


# set survived to NA on the test dataset
DstTest$Survived <- NA

# set the Set column 
DstTest$Set  <- "Test";
DstTrain$Set <- "Train";

# combine all the data
DstAll <- rbind(DstTrain, DstTest);

# set the TrainSet var
TrainSet <- DstAll$Set == 'Train'

# set the TestSet var
TestSet <- DstAll$Set == 'Test'

# The following fixes SibSp/Parch values for two passengers (Id=280 and Id=1284) 
# according to other kernel because a 16 year old can’t have a 13 year old son!
DstAll$SibSp[DstAll$PassengerId==280] <- 0
DstAll$Parch[DstAll$PassengerId==280] <- 2
DstAll$SibSp[DstAll$PassengerId==1284] <- 1
DstAll$Parch[DstAll$PassengerId==1284] <- 1


# let check the missing
missEmb <- is.na(DstAll$Embarked)

# replace the NA 
DstAll[missEmb,]$Embarked <- 'C'


# Imputing the missing data
# The mice() function takes care of the imputing process
newData <- mice(DstAll,m=5,maxit=50,meth='pmm',seed=500, printFlag=FALSE)

# the complete clean dataset
DstAllClean <- complete(newData,1)

# Clear the survived variable
DstAllClean[DstAllClean$Set == 'Test',]$Survived <- NA

#check for missing
sapply(DstAllClean[,-c(2)], function(x) round((sum(is.na(x))/length(x)*100),2))

# get the title from name
DstAllClean$Title <- gsub('(.*, )|(\\..*)', '', DstAllClean$Name)

other <- c('Capt', 'Don', 'Dona', 'Major', 'Jonkheer')
royalty <- c('Lady', 'the Countess','Sir', 'Col', 'Dr', 'Rev', 'Master')

DstAllClean$Title[DstAllClean$Title == 'Mlle']        <- 'Miss' 
DstAllClean$Title[DstAllClean$Title == 'Ms']          <- 'Miss'
DstAllClean$Title[DstAllClean$Title == 'Mme']         <- 'Mrs' 
DstAllClean$Title[DstAllClean$Title %in% royalty]  <- 'Royalty'
DstAllClean$Title[DstAllClean$Title %in% other]  <- 'Other'


# Family size
DstAllClean$FamilySize <- DstAllClean$SibSp + DstAllClean$Parch + 1

# lets find the fare by person
DstAllClean$FareByP <- DstAllClean$Fare / DstAllClean$FamilySize


# Lets try to get the family together
DstAllClean$Surname <- sapply(DstAllClean$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
DstAllClean$FamilyID <- paste(as.character(DstAllClean$Family_size), DstAllClean$Surname, sep="")

# less than 2 so is alone for sure
DstAllClean$FamilyID[DstAllClean$FamilySize < 2] <- 'Alone'

# by frequency
famIDs <- data.frame(table(DstAllClean$FamilyID))
famIDs <- famIDs[famIDs$Freq < 2,]

# some are also alone
DstAllClean$FamilyID[DstAllClean$FamilyID %in% famIDs$Var1] <- 'Alone'

# the kid factor
DstAllClean$Kid[DstAllClean$Age < 18] <- "Yes"
DstAllClean$Kid[DstAllClean$Age >= 18] <- "No"

# is Mother, default is No 
DstAllClean$IsMother <- 'No'
DstAllClean$IsMother[DstAllClean$Sex == 'female' & DstAllClean$Parch > 0 & DstAllClean$Age > 18 & DstAllClean$Title != 'Miss'] <- 'Yes'

# check the deck
DstAllClean$Deck <- substring(DstAllClean$Cabin, 1, 1)


# convert to factor
DstAllClean$FamilyID <- factor(DstAllClean$FamilyID)
DstAllClean$Embarked <- factor(DstAllClean$Embarked)
DstAllClean$Title <- factor(DstAllClean$Title)
DstAllClean$Kid <- factor(DstAllClean$Kid)
DstAllClean$IsMother <- factor(DstAllClean$IsMother)
DstAllClean$Deck <- factor(DstAllClean$Deck)
DstAllClean$Sex <- factor(DstAllClean$Sex)


# train to fit the deck
fit.deck <- cforest(Deck ~ Pclass + Sex + Age  + SibSp + Parch + Fare + Embarked + Title + Kid + IsMother + FamilySize + FareByP + FamilyID,
                          data = DstAllClean[!is.na(DstAllClean$Deck),],
                          controls=cforest_unbiased(ntree=200, mtry=3))


# fill the deck
DstAllClean$Deck[is.na(DstAllClean$Deck)] <- predict(fit.deck, DstAllClean[is.na(DstAllClean$Deck),], OOB=TRUE, type = "response")


# train the model
fit.rf <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Deck + Kid + IsMother + FamilySize + FareByP + FamilyID, 
                      data = DstAllClean[TrainSet, ],
                      controls=cforest_unbiased(ntree=500, mtry=3))


# predict
pred <- predict(fit.rf, DstAllClean[TestSet, ], OOB=TRUE, type = "response")

# create a csv file for submittion
TitanicResult <- data.frame(PassengerId = DstAllClean[TestSet,]$PassengerId, Survived = pred)
head(TitanicResult,n=5L)

# write the submition file
write.csv(TitanicResult,file = "Result.csv",row.names = FALSE)
table(TitanicResult$Survived)
