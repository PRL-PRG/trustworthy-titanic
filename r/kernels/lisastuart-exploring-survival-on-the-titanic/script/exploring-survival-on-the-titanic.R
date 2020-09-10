## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation

library('randomForest') # classification algorithm


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

str(train)
str(test)
full  <- bind_rows(train, test) # bind training & test data

# check data
str(full)
unique(full$Survived) # values are 1, 0, and NA (for the missing Survived row in test)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(full$PassengerId)) 
sum(is.na(full$Survived)) # corresponds with the number in test set
sum(is.na(full$Pclass)) 
sum(is.na(full$Name)) 
sum(is.na(full$Sex)) 
sum(is.na(full$Age)) #263 - need some fancy imputing here!
sum(is.na(full$SibSp)) 
sum(is.na(full$Parch)) 
sum(is.na(full$Ticket)) 
sum(is.na(full$Fare)) # this shouldn't be too difficult to fill
sum(full$Cabin == "") # yikes, most missing here!
sum(full$Embarked == "") 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)

# Found these different titles I've never heard of intriguing so did a little Googling.  Don/Dona are used in Spain, Portugal and Italy as a title of honor (such as Sir when knighted in British culture) while Jonkeer is Dutch translating to Young Lord.  Master is for a boy or young man not yet married and Miss is the same for a girl or young woman.
# Hmm, who knew?

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle (mademoiselle), ms, and mme (madame) accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                      function(x) strsplit(x, split = '[,.]')[[1]][1])



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a family size variable including the passenger themselves
full$FamSize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$FamSize, sep='_')


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = FamSize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
numSurvived = sum(train$Survived) # 342
numDied = nrow(train) - numSurvived #549
PropSurvived = numSurvived/nrow(train) # 0.3838384
PropDied = numDied/nrow(train) # 0.6161616
byClass <- group_by(train, Survived, Pclass)
summarise(byClass, train = n())

    


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use ggplot2 to visualize the relationship between class & survival
ggplot(full[1:891,], aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Class') +
  theme_few()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Table for survival by sex
table(train$Survived, train$Sex)
table(train$Embarked, train$Survived)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Missing_Age = ifelse(is.na(full$Age), 1, 0)
full$Missing_Cabin = ifelse(full$Cabin == "", 1, 0)
full$NumCharName = nchar(full$Name)
full$NumCharTicket <- nchar(full$Ticket)
full$FirstCharTicket<-factor(sapply(full$Ticket, function(x) strsplit(x, NULL)[[1]][1]))
full$FirstCharCabin<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# missing from Embarked
which(full$Embarked == "") # passengers 62 and 830


## ----results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: 
**passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 
'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 
'Pclass'][[1]][2], '</b>. So from where did they embark?'))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
    colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# missing from Fare
which(full$Fare == "") # passenger 1044

# showing the row reveals that this is a 3rd class passenger departing from 'Southampton.'
full[1044, ]


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
    colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make categorical variables into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family', 'FirstCharCabin', 'FirstCharTicket')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived', 'FirstCharCabin', 'FirstCharTicket')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
  col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
  col='lightgreen', ylim=c(0,0.04))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set a random seed
set.seed(1234)

# Build the model starting with all feature variables that, if categorical, have less than 53 categories as Random Forest doesn't like any more than that.
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                            Fare + Embarked + Title + FamSize + FirstCharTicket +
                                            Missing_Age + Missing_Cabin + NumCharName + NumCharTicket,
                                            data = train)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
    y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
    hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set a random seed
set.seed(1234)

# Build the model with top 10 variables
rf_model2 <- randomForest(factor(Survived) ~ Title + Sex + Fare + NumCharName + Age + FirstCharTicket +
                                            Pclass + FamSize + NumCharTicket + Missing_Cabin,
                                            data = train)

# Show model error
plot(rf_model2, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set a random seed
set.seed(1234)

# Build the model with top 10 variables
rf_model3 <- randomForest(factor(Survived) ~ Title + Sex + Fare + NumCharName + Age,
                                            data = train)

# Show model error
plot(rf_model3, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)

