library(rpart)
library(randomForest)
library(party)
library(ggplot2)

#######################################################################################
# Loading data and creating structure
# Join together the test and train sets for easier feature engineering
#######################################################################################
train         <- read.csv("../input/train.csv", na.strings=c("","NA"))
test          <- read.csv("../input/test.csv", na.strings=c("","NA"))
test$Survived <- NA
combi         <- rbind(train, test)

# Convert to a string
combi$Name    <- as.character(combi$Name)

#######################################################################################
# Imuting NA Values: Embarked
#######################################################################################
i <- which(is.na(combi$Embarked))
naEmbarks_data <- combi[i,]
naEmbarks_similar <- combi[combi$Pclass %in% naEmbarks_data$Pclass, ]
table(naEmbarks_similar$Embarked)
# both of the passengers are in the same cabin, it shows Embark value is probably S
combi$Embarked[i] = "S"


#######################################################################################
# Imuting NA Values: Fare
#######################################################################################
naFares       <- combi[which(is.na(combi$Fare)),]
fare_range    <- combi[combi$Pclass == naFares$Pclass & combi$Embarked == naFares$Embarked 
                       & combi$Parch == naFares$Parch & combi$SibSp == naFares$SibSp 
                       & combi$Sex == naFares$Sex & !(is.na(combi$Fare)), ]$Fare
combi[which(is.na(combi$Fare)), ]$Fare  <- median(fare_range) 


#######################################################################################
# Engineered variable: Title
#######################################################################################
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c("Capt", "Col", "Jonkheer", "Don", "Major", "Sir")] <- "Sir"
combi$Title[combi$Title %in% c("Dona", "theCountess", "Lady" )] <- "Lady"
combi$Title[combi$Title %in% c("Ms", "Mlle", "Miss", "Mme")] <- "Miss"
combi$Title[combi$Title == "Dr" & combi$Sex == "male"] <- "Sir"
combi$Title[combi$Title == "Dr" & combi$Sex == "female"] <- "Lady"

#######################################################################################
# Engineered variable: Family
#######################################################################################
combi$Family <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$Family <- sub(' ', '', combi$Family)

#######################################################################################
# Engineered variable: Family size
#######################################################################################
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#######################################################################################
# Engineered variable: Family Id (factor)
#######################################################################################
combi$FamilyID <- combi$Family
combi[combi$FamilySize == 2,]$FamilyID <- "Small"
combi[combi$FamilySize == 1,]$FamilyID <- "Single"

#######################################################################################
# Engineered variable: Ticket
# Extracting just ticket numbers
#######################################################################################
combi$Ticket <- as.character(combi$Ticket)
combi$TicketRev <- sapply(combi$Ticket, FUN=function(x) {
  temp <- strsplit(x, split='[ ]')
  if (!(is.na(temp[[1]][4]))){
    return(temp[[1]][4])
  }
  if (!(is.na(temp[[1]][3]))){
    return(temp[[1]][3])
  }
  if (!(is.na(temp[[1]][2]))){
    return(temp[[1]][2])
  }
  if (!(is.na(temp[[1]][1]))){
    return(temp[[1]][1])
  }
  })
combi[combi$TicketRev == "LINE",]$TicketRev <- 0
combi$TicketRev <- as.numeric(combi$TicketRev)

#######################################################################################
# Engineered variable: Number of passengers per each ticket
#######################################################################################
ticketGroup         <- data.frame(table(combi$TicketRev))
combi$PassPerTicket <- sapply(combi$TicketRev, 
                              FUN = function(x) {ticketGroup[as.character(ticketGroup$Var1) == x, ]$Freq})


#######################################################################################
# Engineered variable: FareRev
#######################################################################################
combi$FareRev <- combi$Fare / combi$PassPerTicket

#######################################################################################
# Engineered variable: Cabin
# Extracting CabinLetter and CabinNumber 
#######################################################################################
combi$Cabin <- as.character(combi$Cabin)
combi[is.na(combi$Cabin) | combi$Cabin == "", ]$Cabin <- "U0"
combi$CabinLetter <- substr(combi$Cabin, 1, 1) 
combi$CabinLetter <- factor(combi$CabinLetter)
combi$CabinNo <- sapply(combi$Cabin, FUN = function(x) {strsplit(x, split='[ ]')[[1]][1]})
combi$CabinNo <- as.numeric(substr(combi$CabinNo, 2, length(combi$CabinNo)))

invalidCabinIds <- c(76, 129, 293, 328, 340, 474, 700, 716, 949, 1001, 1180, 1193, 1213)
combi[invalidCabinIds,]$CabinLetter  <- c("G", "E", "D", "D", "T", "D", "G", "G", "G", "F", "E", "D", "E")
combi[invalidCabinIds,]$CabinNo <- c("73", "69", "0", "0", "0", "0", "63", "73", "63", "0", "46", "0", "57")

#######################################################################################
# Engineered variable: Age
#######################################################################################
combi$AgeRev <- combi$Age
Agefit <- rpart(AgeRev ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, 
                data = combi[!is.na(combi$Age),], method="anova")
combi$AgeRev[is.na(combi$AgeRev)] <- predict(Agefit, combi[is.na(combi$AgeRev),])



#######################################################################################
# Engineered variable: Age Level
#######################################################################################
combi$AgeLevel <- 0
combi[!(is.na(combi$Age)) & combi$Age < 10, ]$AgeLevel <- 1
combi[!(is.na(combi$Age)) & combi$Age >= 10 & combi$Age < 15, ]$AgeLevel <- 2
combi[!(is.na(combi$Age)) & combi$Age >= 15 & combi$Age < 20, ]$AgeLevel <- 3
combi[!(is.na(combi$Age)) & combi$Age >= 20 & combi$Age < 30, ]$AgeLevel <- 4
combi[!(is.na(combi$Age)) & combi$Age >= 30 & combi$Age < 40, ]$AgeLevel <- 5
combi[!(is.na(combi$Age)) & combi$Age >= 40 & combi$Age < 50, ]$AgeLevel <- 6
combi[!(is.na(combi$Age)) & combi$Age >= 50 & combi$Age < 60, ]$AgeLevel <- 7
combi[!(is.na(combi$Age)) & combi$Age >= 60 & combi$Age < 70, ]$AgeLevel <- 8
combi[!(is.na(combi$Age)) & combi$Age >= 70 , ]$AgeLevel <- 9


#######################################################################################
# Convert to factors
#######################################################################################
combi$FamilyID  <- factor(combi$FamilyID)
combi$Title     <- factor(combi$Title)
combi$Embarked  <- factor(combi$Embarked)
combi$TicketRev <- factor(combi$TicketRev)
combi$Survived  <- factor(combi$Survived)


#######################################################################################
# Charts
#######################################################################################


ggplot(subset(train,Age != is.na(Age)), aes(x = round(Age, 0), fill = factor(Survived), colour = factor(Survived))) +
  geom_density(alpha = 0.3, adjust = 1) +
  labs(title='Passenger Age Distribution')+ facet_wrap(~ Sex)

ggplot(subset(combi,Age != is.na(Age)), aes(x = Age, fill = factor(Survived))) +
  geom_density(alpha = 0.6) +
  labs(title='Passenger Age Distribution')

ggplot(subset(train,Age != is.na(Age)), aes(x = Age, y = as.numeric(Survived))) +
  geom_point() + geom_density_2d()


#######################################################################################
# Using mice to impute some values
#######################################################################################
combi0        <- rbind(train, test)
combi1        <- rbind(train, test)
combi1        <- combi1[, ! names(combi1) %in% c("PassengerId", "Survived", "Name", "Ticket", "Cabin", "Family", "CabinLetter", "CabinNo", "AgeLevel")] 
combi1$Pclass <- factor(combi1$Pclass)

library(mice)
init = mice(combi1, maxit=0)
meth = init$method
predM = init$predictorMatrix
meth[c("Age")] = "norm"
meth[c("Embarked")] = "rf"
meth[c("Fare")] = "rf"
set.seed(103)
imputed = mice(combi1, method=meth, predictorMatrix=predM, m=7)
imputed <- complete(imputed)

combi$AgeRevMice <- imputed$Age
combi$FareRevMice <- imputed$Fare
combi$EmbarkedRevMice <- imputed$Embarked

#write.csv(combi, file = "combi.csv", row.names = FALSE)


# Split back into test and train sets
train <- combi[!(is.na(combi$Survived)),]
test  <- combi[is.na(combi$Survived),]

#Build Random Forest Ensemble
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + AgeRevMice + EmbarkedRevMice + SibSp + Parch,
                    data=train, importance=TRUE, ntree=2000)


# fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + AgeRevMice + EmbarkedRevMice + Title +
#                       SibSp + Parch + FareRevMice + FamilySize + PassPerTicket,
#                     data=train, importance=TRUE, ntree=2000)
# Look at variable importance
varImpPlot(fit)
# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "../output/firstforest.csv", row.names = FALSE)

#Build condition inference tree Random Forest
set.seed(415)

fit <- cforest(as.factor(Survived) ~ Pclass + Sex + AgeRevMice + EmbarkedRevMice + SibSp + Parch ,
               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

# fit <- cforest(as.factor(Survived) ~ Pclass + Sex + AgeRevMice + EmbarkedRevMice + Title +
#                  SibSp + Parch + FareRevMice + FamilySize + PassPerTicket + FamilyID ,
#                data = train, controls=cforest_unbiased(ntree=2000, mtry=3))

# Now let's make a prediction and write a submission file
Prediction <- predict(fit, test, OOB=TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
# write.csv(submit, file = "../output/ciforest.csv", row.names = FALSE)


