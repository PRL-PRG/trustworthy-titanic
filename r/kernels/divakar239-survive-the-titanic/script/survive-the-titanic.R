# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library('readr') # CSV file I/O, e.g. the read_csv function
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library('caTools')
library('e1071')
library('xgboost')

#getting the data
train_set <- read.csv('../input/train.csv', stringsAsFactors=F,header=T)
test_set  <- read.csv('../input/test.csv', stringsAsFactors=F,header=T)

#Combining the two sets by rows as the entire dataset was already split for us
full_set <- bind_rows(train_set,test_set)
#str(full_set)

###########################################################FACTOR ENGINEERING##############################################

#extracting meaningful data from the Passenger's name
full_set$Title <- gsub('(.*, )|(\\..*)', '', full_set$Name) 
#(.*, ) : selects the last name
#(\\..*): selects the first name
#gsub(Pattern, replacement, string/stringVector)

# Grouping all rare titkes to one group
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Reassign mlle, ms, and mme accordingly
full_set$Title[full_set$Title == 'Mlle']        <- 'Miss' 
full_set$Title[full_set$Title == 'Ms']          <- 'Miss'
full_set$Title[full_set$Title == 'Mme']         <- 'Mrs' 
full_set$Title[full_set$Title %in% rare_title]  <- 'Rare Title'  #replacing all rare titles with a single entity 'Rare Title'

#Grabbing last name
full_set$Surname <- sapply(full_set$Name, function(x) strsplit(x, split = '[.,]'))
#splits every word at . and ,

                      
#Creating Families column for every field
full_set$Family_Size <- (full_set$Parch + full_set$SibSp + 1)
#converts the objects passed to strings and concatenates them. sep defines what is to be put in between them
full_set$Family <- paste(full_set$Surname, full_set$Family_Size,sep=' ')


#plotting family size vs survival of the training set
# ggplot(full_set[1:891,], aes(x = Family_Size, fill = factor(Survived))) +
#   geom_bar(stat='count', position='dodge') +
#   scale_x_continuous(breaks=c(1:11)) +
#   labs(x = 'Family Size') +
#   theme_few()

#Making a discretized plot
full_set$Discrete_Family_Size[full_set$Family_Size == 1] <- "singleton"
full_set$Discrete_Family_Size[full_set$Family_Size < 5 & full_set$Family_Size >1] <- "small"
full_set$Discrete_Family_Size[full_set$Family_Size > 4] <- "large"

# mosaicplot(table(full_set$Discrete_Family_Size, full_set$Survived), main='Family Size by Survival', shade=TRUE)

#Retrieving useful data from the cabin column
#getting the deck level
full_set$Deck<-factor(sapply(full_set$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

#Imputation
#1. Embarked column has missing values from ID 62 to 830
embark_fare <- full_set %>% filter(PassengerId != 62 & PassengerId != 830)
# ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
#   geom_boxplot() +
#   geom_hline(aes(yintercept=80), 
#     colour='red', linetype='dashed', lwd=2) +
#   scale_y_continuous(labels=dollar_format()) +
#   theme_few()

#Substituting missing values with calss 1 
full_set$Embarked[c(62, 830)] <- 'C'
# Replace missing fare value with median fare for class/embarkment
full_set$Fare[1044] <- median(full_set[full_set$Pclass == '3' & full_set$Embarked == 'S', ]$Fare, na.rm = TRUE)

# 2. filling missing ages

full_set$Surname <- sapply(full_set$Surname, function(x) as.factor(x))
full_set$PassengerId <- sapply(full_set$PassengerId, function(x) as.factor(x))
full_set$Pclass <- sapply(full_set$Pclass, function(x) as.factor(x))
full_set$Sex <- sapply(full_set$Sex, function(x) as.factor(x))
full_set$Embarked <- sapply(full_set$Embarked, function(x) as.factor(x))
full_set$Title <- sapply(full_set$Title, function(x) as.factor(x))
full_set$Family <- sapply(full_set$Family, function(x) as.factor(x))
full_set$Discrete_Family_Size <- sapply(full_set$Discrete_Family_Size, function(x) as.factor(x))


# # Set a random seed
set.seed(129)
# # Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full_set[, !names(full_set) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod)
# Replace Age variable from the mice model.
full_set$Age <- mice_output$Age
#sum(is.na(full_set$Age))
# More feature engineering
#since we have ages now, we can use them to distinguish children from mothers( children and women are rescued first)

#plot between survival vs age
# ggplot(full_set[1:891,], aes(Age, fill = factor(Survived))) + 
#   geom_histogram() + 
#   # we include Sex since we know (a priori) it's a significant predictor
#   facet_grid(.~Sex) + 
#   theme_few()

#Create the column child, and indicate whether child or adult
full_set$Child[full_set$Age < 18] <- 'Child'
full_set$Child[full_set$Age >= 18] <- 'Adult'

# Show counts
#table(full_set$Child, full_set$Survived)

# Adding Mother variable
full_set$Mother <- 'Not Mother'
full_set$Mother[full_set$Sex == 'female' & full_set$Parch > 0 & full_set$Age > 18 & full_set$Title != 'Miss'] <- 'Mother'

# Show counts
#table(full_set$Mother, full_set$Survived)

#factorizing the two new factor variables
full_set$Child  <- factor(full_set$Child)
full_set$Mother <- factor(full_set$Mother)

#full_set$Sex <- as.numeric(factor
  
# Split the data 
train_set <- full_set[1:891,]
test_set <- full_set[892:1309,]

sum(is.na(train_set$Embarked))

# #Prediction
# # Set a random seed
# set.seed(754)
# # Build the model (note: not all possible variables are used)
# #rf_model <- xgboost(data=as.matrix(train_set[-2]), label=train_set$Survived, nrounds=10) 
# rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
#                                             Fare + Embarked + Title + 
#                                             Discrete_Family_Size + Child + Mother,
#                                             data = train_set,
#                                             ntree = 3500,
#                                             mtry=19,
#                                             nodesize=30)
# # Show model error
# # plot(rf_model, ylim=c(0,0.36))
# # legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
# #black line = overall error rate which falls below 20%
# #it shows we are more successful predicting death than we are survival

# #Checking for most important varibles in the prediction model
# # Get importance
# importance    <- importance(rf_model)
# varImportance <- data.frame(Variables = row.names(importance),Importance = round(importance[ ,'MeanDecreaseGini'],2))

# # Create a rank variable based on importance
# rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# # Use ggplot2 to visualize the relative importance of variables
# # ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
# #     y = Importance, fill = Importance)) +
# #   geom_bar(stat='identity') + 
# #   geom_text(aes(x = Variables, y = 0.5, label = Rank),
# #     hjust=0, vjust=0.55, size = 4, colour = 'red') +
# #   labs(x = 'Variables') +
# #   coord_flip() + 
# #   theme_few()

# #Predictions
# prediction <- predict(rf_model, test_set)
# # Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
# solution <- data.frame(PassengerID = test_set$PassengerId, Survived = prediction)
# # Write the solution to file
# write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
