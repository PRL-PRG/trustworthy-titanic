## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# visualization
library('ggplot2') 
library('ggthemes')
library('corrplot')
library('scales') 
library('polycor')      # Correlaton for non numeric (ex factors)
library('knitr')        # Output formatting
library('dplyr')        # data manipulation
library('wru')          # ethnicity prediction
library('mice')         # imputation
library('randomForest') # classification algorithm


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

# bind training & test data
full  <- bind_rows(train, test) 



## ---- message=FALSE, warning=FALSE, echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(str(full))


## ---- message=FALSE, warning=FALSE, fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train$Survived <- factor(train$Survived)                       # improves correlations
train$Pclass <- factor(train$Pclass,levels = c(3,2,1))
train$Sex <- factor(train$Sex,levels=c("male","female"))       # change order for positive correlation
train$Embarked <- factor(train$Embarked,levels=c("S","Q","C")) # change order for positive correlation
corr <- hetcor(train[,c("Survived","Sex","Pclass","Fare","Embarked","Parch","Age","SibSp")])

par(fin=c(6,5)) # width/height of corr plot. Errors when height > 5 
corrplot.mixed(corr$correlations,lower="ellipse", upper="number",tl.cex=.8,tl.srt=45,tl.col="black")



## ---- fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train, aes(Age, fill = Survived, colour = Survived)) +
   geom_density(alpha = 0.1)  + 
   facet_grid(.~Sex)  


## ---- message=FALSE, warning=FALSE, fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
kable(table(full$Title, full$Sex))

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
kable(table(full$Sex, full$Title))

# Same information as a scatter plot
titles_by_sex = data.frame(table(full$Sex, full$Title))
names(titles_by_sex) <- c("Sex","Title","Freq")
pp <- ggplot(titles_by_sex, aes(factor(Sex), factor(Title), size=Freq)) 
pp + geom_point() + geom_point(aes(colour = factor(Title))) + scale_size_area(max_size = 10)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$Surname <- sapply(full$Name,function(x) strsplit(x, split = '[,.]')[[1]][1])

regn <- full[,c("PassengerId","Surname")]
colnames(regn)[colnames(regn) == 'Surname'] <- 'surname'
regn <- race.pred(regn,surname.only=TRUE)
colnames(regn)[colnames(regn) == 'pred.whi'] <- 'white'
colnames(regn)[colnames(regn) == 'pred.bla'] <- 'black'
colnames(regn)[colnames(regn) == 'pred.his'] <- 'latino'
colnames(regn)[colnames(regn) == 'pred.asi'] <- 'asian'
colnames(regn)[colnames(regn) == 'pred.oth'] <- 'other'
regn$ethnicity <- colnames(regn[c("white","black","latino","asian","other")])[max.col(regn[c("white","black","latino","asian","other")],ties.method="first")]

full <- merge(full,regn[,c("PassengerId","ethnicity")],by=c("PassengerId"))
full$ethnicity <- factor(full$ethnicity)
rm(regn)


## ---- message=FALSE, warning=FALSE, fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use ggplot2 to visualize the relationship between family size & survival
#full$Survived <- factor(full$Survived,levels = c(0,1))

ggplot(full, aes(x = ethnicity, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  labs(x = 'Family Size') +
  theme_few()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sample <- full[full$Surname %in% c("Andersson","Asplund","Elias"),]
kable(sample[order(sample$Family,sample$Ticket,sample$Age),c("Family","Surname","Ticket","Embarked","Cabin","Fare","SibSp","Parch","Age","Name")])



## ---- message=FALSE, warning=FALSE, fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use ggplot2 to visualize the relationship between family size & survival
ggplot(full[!is.na(full$Survived),], aes(x = Fsize, fill = factor(Survived))) +
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
# This variable appears to have a lot of missing values
full$Cabin[1:28]

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']


## ----results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cat(paste('We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid<b> $', full[c(62, 830), 'Fare'][[1]][1], '</b>and<b> $', full[c(62, 830), 'Fare'][[1]][2], '</b>respectively and their classes are<b>', full[c(62, 830), 'Pclass'][[1]][1], '</b>and<b>', full[c(62, 830), 'Pclass'][[1]][2], '</b>. So from where did they embark?'))


## ---- message=FALSE, warning=FALSE, fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ---- message=FALSE, warning=FALSE, fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make variables factors into factors
#full$Survived <- factor(full$Survived)                       
full$Pclass <- factor(full$Pclass,levels = c(3,2,1))
full$Sex <- factor(full$Sex,levels=c("male","female"))       
full$Embarked <- factor(full$Embarked,levels=c("S","Q","C")) 

factor_vars <- c('PassengerId','Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)


## ---- fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
  col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
  col='lightgreen', ylim=c(0,0.04))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Replace Age variable from the mice model.
full$Age <- mice_output$Age


## ---- message=FALSE, warning=FALSE, fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# First we'll look at the relationship between age & survival
ggplot(full[!is.na(full$Survived),], aes(Age, fill = factor(Survived))) + 
  geom_histogram() + 
  # I include Sex since we know (a priori) it's a significant predictor
  facet_grid(.~Sex) + 
  theme_few()

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
table(full$Child, full$Survived)


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
# Classify the age into age groups
full$AgeGroup[full$Age < 18] <- "Child"
full$AgeGroup[full$Age >= 18 & full$Age < 25] <- "Young Adult"
full$AgeGroup[full$Age >= 25 & full$Age < 50] <- "Adult"
full$AgeGroup[full$Age >= 50 ] <- "Old"

full$AgeGroup <- factor(full$AgeGroup)


## ---- fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- full[!(is.na(full$Survived)),]
train$Survived <- factor(train$Survived)
ggplot(train, aes(Age, fill = Survived, colour = Survived)) +
   geom_density(alpha=.3) +
   facet_grid(Sex~AgeGroup)  


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
md.pattern(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Split the data back into a train set and a test set
train <- full[!(is.na(full$Survived)),]
test <- full[is.na(full$Survived),]

dim(train)
dim(test)


## ---- fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~  Sex + Title + AgeGroup + ethnicity + Age + Pclass + Mother + Child + Fsize + FsizeD,
                                            data = train)


# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)



## ---- message=FALSE, warning=FALSE, fig.width=10---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution2.csv', row.names = F)

