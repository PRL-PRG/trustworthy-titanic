
suppressMessages(library('ggplot2')) # visualization
suppressMessages(library('ggthemes')) # visualization
suppressMessages(library('scales')) # visualization
suppressMessages(library('dplyr')) # data manipulation
suppressMessages(library('mice')) # imputation
suppressMessages(library('randomForest')) # classification algorithm

train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

full  <- bind_rows(train, test)

# check data
str(full)

full[full==''] <- NA
sapply(full, function(x) sum(is.na(x)))

# Grab title from passenger names
full$Title <- gsub('.*, ([^.]*)\\..*', '\\1', full$Name)

# Title vs Survivor
table(full$Title, full$Survived)

# Replace rare titles
full$Title[full$Title == 'Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms'] <- 'Miss'
full$Title[full$Title == 'Mme'] <- 'Mrs'
# Group rare Title to officer and royalty
officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')
full$Title[full$Title %in% officer] <- 'officer'
full$Title[full$Title %in% royalty] <- 'royalty'

# Show title counts by sex
table(full$Sex, full$Title)

# Get Surnames
full$Surname <- gsub('([^,]*),.*', '\\1', full$Name)
str(factor(full$Surname))

full$Deck <- sapply(as.character(full$Cabin), function(x) strsplit(x, NULL)[[1]][1])
# Replace NA with 'U' (Unknown)
full <- within(full,
   Deck <- ifelse(is.na(Deck),'U',Deck)
)
# Group upper, middle and lower decks
full$Deck[full$Deck == 'A' | full$Deck == 'B'] <- 'upper_deck'
full$Deck[full$Deck == 'C' | full$Deck == 'D'] <- 'middle_deck'
full$Deck[full$Deck == 'E' | full$Deck == 'F' | 
          full$Deck == 'G' | full$Deck == 'T'] <- 'lower_deck'
    
full$Deck <- factor(full$Deck)
    
print(summary(full$Deck))

# Familie size: siblings + spouses + parents + children + Passenger themselve
full$Fsize <- full$SibSp + full$Parch + 1

# Group also people in the same cabin with family size == 1
cabins <- full$Cabin
n_occur <- data.frame(table(Var1=cabins))
n_occur <- subset(n_occur, nchar(as.character(Var1)) > 1)

sharedCabins <- n_occur$Var1[n_occur$Freq > 1]

sharedInd <- full$Fsize == 1 & full$Cabin %in% sharedCabins
full$Fsize[sharedInd] <- 2

# Concatenate surname w/ family size
full$Family <- paste(full$Surname, full$FsizeAdj, sep='_')

# Plot Family Size vs Survivor
ggplot(full[1:891, ], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)

# There are missing values
str(factor(full$Embarked))
full$PassengerId[full$Embarked == '']
full$Pclass[full$Embarked == '']

ggplot(full, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
    colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()

# Missing values are both class 1; safe to say they embark in Cherbourg
full$Embarked[c(62, 830)] <- 'C'

# There are missing values
str(factor(full$Fare))
full$PassengerId[is.na(full$Fare) == TRUE]

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
    colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# First we'll look at the relationship between age, title & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Title) + 
  theme_few()

# Show number of missing Age values
sum(is.na(full$Age))

# We know 'Masters' are young boys
sum(is.na(full$Age[full$Title == 'Master']))
summary(full$Age[full$Title == 'Master'])
full$Age[is.na(full$Age) & full$Title == 'Master'] <- 4

# Make variables factors into factors
factor_vars <- c('PassengerId', 'Pclass','Sex','Embarked', 'Deck',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
#mice_mod <- mice(full[, !names(full) %in% c('Name','Ticket','Family','Surname','Survived')], method='rf') 
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived', 'Deck')], method='rf') 
    
# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
  col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
  col='lightgreen', ylim=c(0,0.04))

# Replace Age variable from the mice model.
full$Age <- mice_output$Age

# Show new number of missing Age values
sum(is.na(full$Age))

# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Title) + 
  theme_few()

# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
table(full$Child, full$Survived)

# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

md.pattern(full)

# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]

# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
# Accuracy: 0.80861
# rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
#                                            Fare + Embarked + Title +
#                                            FsizeD + Child + Mother +
#                                            Deck,
#                                            data = train)

# Reduce variables further to avoid overfitting
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Fare +  
                                            Embarked + Title + FsizeD + Child,
                                            data = train)

Pclass + Sex + Fare + Embarked + Title + 
                           FsizeD + Child

# Get importance from random forest model
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

# Predict using the test set
rf_prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
rf_solution <- data.frame(PassengerID = test$PassengerId, Survived = rf_prediction)

# Write the solution to file
write.csv(rf_solution, file = 'Titanic_rf_Solution.csv', row.names = F)
