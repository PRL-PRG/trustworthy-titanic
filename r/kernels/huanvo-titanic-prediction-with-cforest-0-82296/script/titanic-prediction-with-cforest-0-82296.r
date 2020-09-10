
# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('rpart') #decision tree
library(party) #cforest 

train <- read.csv('../input/train.csv')
test  <- read.csv('../input/test.csv')

#checking the structure of the train set 
str(train)

#checking the structure of the test set
str(test)

#load the first few rows of the data
head(train)

#create a Survived column for the test set filled with NAs
test$Survived <- NA

#combine the train and test sets to a data set named combi
combi <- rbind(train, test)

# summarize the combi data set
summary(combi)

# convert Name to character
combi$Name <- as.character(combi$Name)

# extracting title from name
combi$Title <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][2]})
# removing white space from the title
combi$Title <- sub(' ','', combi$Title)

table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir', 'Jonkheer')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'

combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1 

combi$Surname <- sapply(combi$Name, FUN = function(x){strsplit(x, split = '[,.]')[[1]][1]})

combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep= "")

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'

table(combi$FamilyID)

famIDs <- data.frame(table(combi$FamilyID))

head(famIDs)

famIDs <- famIDs[famIDs$Freq <= 2,]

combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'

combi$FamilyID <- factor(combi$FamilyID)

table(combi$FamilyID)

summary(combi$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
               data = combi[!is.na(combi$Age),],
               method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

table(combi$Embarked)

# find the position of the missing value
which(combi$Embarked == '')

# fill in those positions with 'S'
combi$Embarked[c(62, 830)] = "S"

#convert Embarked to factor
combi$Embarked <- factor(combi$Embarked)

# find the position of the missing Fare value
which(is.na(combi$Fare))

# fill in the missing value with the media
combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)

# create a dataframe TicketGroup that group by the ticket
TicketGroup <- combi %>%
            select(Ticket) %>%
            group_by(Ticket) %>%
            summarise(Tsize=n())

#join the TicketGroup dataframe to combi on the Ticket feature
combi <- left_join(combi, TicketGroup, by = "Ticket")

#creating the fare per person variable 
combi$FarePP <- combi$Fare/combi$Tsize

#create a dataframe tab3 which computes the mean fare based on Pclass
tab3 <- combi[(!is.na(combi$FarePP)),] %>%
        group_by(Pclass) %>%
        summarise(MeanFarePP = mean(FarePP))
# join the dataframe tab3 to combi on the Pclass feature
combi <- left_join(combi, tab3, by ="Pclass")
# replace FarePP = 0 by the mean fare based on Pclass
combi$FarePP[which(combi$FarePP==0)] <- combi$MeanFarePP[which(combi$FarePP==0)]

train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + FarePP +
                                       Embarked + Title + FamilySize + FamilyID,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction <- predict(fit, test, OOB=TRUE, type = "response")

submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "TitanicPredictionRFare", row.names = FALSE)


