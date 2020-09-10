# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('plyr') # for the revalue() function
library('stringr')  # for the str_sub() function
library('caret') # For machine learning

train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test) # bind training & test data

# check data
str(full)

full$Survived <- revalue(factor(full$Survived), c("1" = "Survived", "0" = "Perished"))
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
    
full$Embarked[c(62, 830)] <- 'C'
# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 
full$Side <- NA
full$Cabin.last.digit <- str_sub(full$Cabin, -1)
full$Side[which(isEven(full$Cabin.last.digit))] <- "port"
full$Side[which(isOdd(full$Cabin.last.digit))] <- "starboard"

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title','FsizeD','Side')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf') 
    
mice_output <- complete(mice_mod)
# Replace Age variable from the mice model.
full$Age <- mice_output$Age
full$Deck <- mice_output$Deck
full$Side <- mice_output$Side

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 16] <- 'Child'
full$Child[full$Age >= 16] <- 'Adult'
    
# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0] <- 'Mother'

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- train(Survived ~ Sex:I(Pclass=="3") + Age + I(exp(Fare)) + Title + Sex*Child + Pclass:Child +
            Mother*I(FsizeD =="large") + I(FsizeD =="large")*Sex + Sex:I(Embarked=="Q") + 
            Side:I(Pclass=="1") , method="glm",  data = train)

print(rf_model)

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
prediction <- revalue(prediction, c("Survived" = "1", "Perished" = "0"))

solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file

write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)










write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
