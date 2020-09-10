## ----echo = TRUE, message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm
library("rpart") # Decision tree algorith
library("rpart.plot") # Build plot for decision tree
library("RColorBrewer") # To use color palette
library("rattle") # Build fancy plot for decision tree. You may face issues if you try to run in your machine. You should install and library in console.


## ----echo = TRUE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

str(train)

str(test)

full  <- bind_rows(train, test) # bind training & test data

# check data
str(full)


slice(full, 886:896)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Grab title from passenger names

#'(.*, ) - Ignore all till you find a "," and then space
# (\\..*)' - Ignore all starting from "." and everthing after it

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show title counts by survived
table(full$Survived, full$Title)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Don', 
                'Dr', 'Major',  'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Show title counts by survived again
table(full$Survived, full$Title)

# Finally, grab surname from passenger name
# sapply - run the function(x) on each element of the input vector full$Name
# split = '[,.]' - splits each name by ',' or '.'
# [[1]]  - the whole name divided into multiple parts
# [[1]][1] - pick the first part of the dvided name
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])


## ----results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cat(paste('We have <b>', nlevels(factor(full$Surname)), '</b> unique surnames. I would be interested to infer ethnicity based on surname --- another time.'))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

head(full)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size by Survival', shade=TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(full$Cabin)) 
# This variable appears to have a lot of missing values

full$Cabin[1:28]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use summary to find missing values/NAs and outlier
summary(full)
sample_n(full, 10)
sample_n(full, 10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Analyse for "" or " "
subset(full, full$Cabin == "" | full$Cabin == " ") %>% count()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
subset(full, full$Embarked == "" | full$Embarked == " ") %>% count()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
subset(full, full$Deck == "" | full$Deck == " ") %>% count()
subset(full, is.na(full$Deck)) %>% count()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Find the exact row, where Embarked has missing value
subset(full, full$Embarked == "" | full$Embarked == " ")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']


## ----results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Are there people with same surname. They may be from same place
subset(full, full$Surname == "Stone" | full$Surname == "Icard")

# Was there a fixed fare from each emabrak city
subset(full, full$Fare == 80)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
# Show row 1044
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
# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show number of missing Age values
sum(is.na(full$Age))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
# mice imputes all NA with values using method. Method used below is rf = random forest
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

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

#predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
#                       data=full[!is.na(full$Age),], method="anova")
#full$Age[is.na(full$Age)] <- predict(predicted_age, combi[is.na(full$Age),])


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram(position = "fill") + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Child[full$Age < 6] <- 'Baby'
full$Child[full$Age >= 6 & full$Age < 14] <- 'Kid'
full$Child[full$Age >= 14 & full$Age < 18] <- 'Young'
full$Child[full$Age >= 18 & full$Age < 60] <- 'Adult'
full$Child[full$Age >= 60] <- 'Old'

# Show counts
table(full[1:891,]$Child, full[1:891,]$Survived)

# Show counts in proportion
prop.table(table(full[1:891,]$Child, full[1:891,]$Survived), 1)

# First we'll look at the relationship between age category and survival
ggplot(full[1:891,], aes(factor(Child), fill = factor(Survived))) + 
  geom_histogram(stat = "count", position = "fill") + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
md.pattern(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                            Fare + Embarked + Title + 
                                            FsizeD + Child + Mother,
                                            data = train, ntree=100, importance=TRUE)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
varImpPlot(rf_model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show counts
table(full[1:891,]$Title, full[1:891,]$Survived)

# Show counts in proportion
prop.table(table(full[1:891,]$Title, full[1:891,]$Survived), 1)

ggplot(full[1:891,], aes(factor(Title), fill = factor(Survived))) + 
  geom_histogram(stat = "count", position = "fill") + 
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


posn.jd <- position_jitterdodge(jitter.width = 0.4, dodge.width = 0.5)

ggplot(full[1:891,], aes(y = Fare, x = Sex, col = factor(Survived))) + 
  geom_point(size = 3, alpha = 0.4, position = posn.jd) + 
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram(position = "fill") + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show counts
table(full[1:891,]$Pclass, full[1:891,]$Survived)

# Show counts in proportion
prop.table(table(full[1:891,]$Pclass, full[1:891,]$Survived), 1)

ggplot(full[1:891,], aes(factor(Pclass), fill = factor(Survived))) + 
  geom_histogram(stat = "count", position = "fill") + 
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set a random seed
set.seed(754)

tree_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                                            Fare + Embarked + Title + 
                                            FsizeD + Child + Mother , data = train, method = "class", control=rpart.control(cp=0.0001))

# Plot the tree
plot(tree_model)
text(tree_model)

prp(tree_model, type = 4, extra = 100)

# Time to plot fancy tree
fancyRpartPlot(tree_model)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make predictions on the test set
my_prediction <- predict(tree_model, newdata = test, type = "class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Finish the write.csv() call
write.csv(my_solution, file = "tree_mod_Solution.csv", row.names = FALSE)


## ----eval = FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## library(knitr)
## purl("Titanic_ForkedFromMeganScript.Rmd")

