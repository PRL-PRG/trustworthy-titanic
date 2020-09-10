## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr') 
library('mice')
library('randomForest') 
library('readr') 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

train <-read.csv('../input/train.csv', stringsAsFactors = F)
test  <-read.csv('../input/test.csv', stringsAsFactors = F)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic<-bind_rows(train,test)

## Data check

str(titanic)
summary(titanic)
head(titanic)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
colnames(titanic)

### Retrieve title from passenger names

titanic$title<-gsub('(.*, )|(\\..*)', '', titanic$Name)

# Show title counts by sex

table(titanic$Sex, titanic$title)

## Convert title with low count into new title

unusual_title<-c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

## Rename/reassign  Mlle, Ms, and Mme

titanic$title[titanic$title=='Mlle']<-'Miss'
titanic$title[titanic$title=='Ms']<-'Miss'
titanic$title[titanic$title=='Mme']<-'Mrs'
titanic$title[titanic$title %in% unusual_title]<-'Unusual Title'

## Check the title count again

table(titanic$Sex, titanic$title)

## Create variable which contain surnames of passenger name

titanic$surname<-sapply(titanic$Name, function(x) strsplit(x,split='[,.]')[[1]][1])
nlevels(factor(titanic$surname)) ## 875 unique sirname


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$famsize <- titanic$SibSp + titanic$Parch + 1

# Create a family variable 

titanic$family <- paste(titanic$surname, titanic$famsize, sep='_')


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic[1:891,], aes(x = famsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Discretize family size

titanic$fsizeD[titanic$famsize == 1] <- 'single'
titanic$fsizeD[titanic$famsize < 5 & titanic$famsize> 1] <- 'small'
titanic$fsizeD[titanic$famsize> 4] <- 'large'


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Cabin[1:28]

# The first character is the deck:

strsplit(titanic$Cabin[2], NULL) [[1]] 

# Deck variable

titanic$deck<-factor(sapply(titanic$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(titanic) 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Embarked[titanic$Embarked == ""] <- NA 

titanic[(which(is.na(titanic$Embarked))), 1] 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic[c(62, 830), 'Embarked']


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic[c(62, 830), c(1,3,10)]


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic%>%
  group_by(Embarked, Pclass) %>%
  filter(Pclass == "1") %>%
  summarise(mfare = median(Fare),n = n())


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
embark_fare <- titanic %>%
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

titanic$Embarked[c(62, 830)] <- 'C'


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## ## to know Which passenger has no fare information 

titanic[(which(is.na(titanic$Fare))) , 1] 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic[1044, c(3, 12)]


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic[1044, ]


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## First way:

titanic%>%
filter(Pclass == '3' & Embarked == 'S') %>%
summarise(missing_fare = median(Fare, na.rm = TRUE))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Second way:

ggplot(titanic[titanic$Pclass == '3' & titanic$Embarked == 'S', ], 
aes(x = Fare)) +
geom_density(fill = '#99d6ff', alpha=0.4) + 
geom_vline(aes(xintercept=median(Fare, na.rm=T)),
           colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Fare[1044] <- 8.05


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic$Fare[1044] <- median(titanic[titanic$Pclass == '3' & titanic$Embarked == 'S', ]$Fare, na.rm = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Show number of missing Age values

sum(is.na(titanic$Age))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Set a random seed

set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:

mice_mod <- mice(titanic[, !names(titanic) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 

mice_output <- complete(mice_mod)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot age distributions

par(mfrow=c(1,2))
hist(titanic$Age, freq=F, main='Age: Original Data', 
     col='darkred', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Replace Age variable from the mice model

titanic$Age <- mice_output$Age

# Show new number of missing Age values

sum(is.na(titanic$Age))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Relationship between age & survival: I include Sex since we know it's a significant predictor

ggplot(titanic[1:891,], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + facet_grid(.~Sex) + theme_few()

# Create the column child, and indicate whether child or adult

titanic$Child[titanic$Age < 18] <- 'Child'
titanic$Child[titanic$Age >= 18] <- 'Adult'

# Show counts

table(titanic$Child, titanic$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Adding Mother variable

titanic$Mother <- 'Not Mother'
titanic$Mother[titanic$Sex == 'female' & titanic$Parch >0 & titanic$Age > 18 & titanic$title != 'Miss'] <- 'Mother'

# Show counts

table(titanic$Mother, titanic$Survived)

## Factorizing variables

titanic$Child  <- factor(titanic$Child)
titanic$Mother <- factor(titanic$Mother)
titanic$Pclass<-factor(titanic$Pclass)
titanic$Sex<-factor(titanic$Sex)
titanic$Embarked<-factor(titanic$Embarked)
titanic$Survived<-factor(titanic$Survived)
titanic$title<-factor(titanic$title)
titanic$fsizeD<-factor(titanic$fsizeD)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- titanic[1:891,]
test <- titanic[892:1309,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set a random seed

set.seed(754)

# Build the model (note: not all possible variables are used)

titanic_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + 
                                Fare + Embarked + title + 
                                fsizeD + Child + Mother,
                              data = train)


# Show model error

plot(titanic_model, ylim=c(0,0.36))
legend('topright', colnames(titanic_model$err.rate), col=1:3, fill=1:3)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get importance

importance    <- importance(titanic_model)
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
# Predict using the test set

prediction <- predict(titanic_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)

Output<- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the Output to file

write.csv(Output, file = 'pradeep_titanic_output.csv', row.names = F)

