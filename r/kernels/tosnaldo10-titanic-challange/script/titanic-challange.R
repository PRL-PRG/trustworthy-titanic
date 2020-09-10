#Titanic Challenge_
#The below script is my first attempt at R and predictive modelling.
#I work with in a data environment and have always had questions of why business does not use more then just the variables.
#What i know now after my attempted model through the help of the Kaggle community is that there is value to be found and that we can learn from past events.
#
#
#My approach was to focus on all variables that would help with identifying the positioning of people the time they realized the ship was sinking.
#we have heard the term when something bad happens that he/she was in the wrong place at the wrong time and there is a lot of thuth in this hence my angle of approach.
#
#I tried keeping the code simple so that hopefully one person with no experience in R or predictive modeling can find some once of value in it.
#
#
#
#load packages
library('ggplot2')  
library('caret') 
library('dplyr')
library('rpart')
library('rpart.plot')
library('car')
library('e1071')
library('randomForest')
library('ggthemes') 
#Importing Data
train.tit <- read.csv('../input/train.csv', stringsAsFactors = F)
test.tit  <- read.csv('../input/test.csv', stringsAsFactors = F)
test.tit$Survived <- rep(0, 418)
str(test.tit)

#combining the two files but analysis will be done on the first 891 observations
#combo  <- bind_rows(train, test) # bind training & test data
combo <- rbind(train.tit, test.tit)

str(combo)

#Checking for missing values in each variables and  doing imputation on Embarked
colSums(is.na(combo))
colSums(combo=='')
combo$Embarked[combo$Embarked==""]="S"
table(combo$Embarked)

#variables(Survived,Age,Cabin and Embarked) has missing and no values


#Analyzing variables we planning to use and what impact they have on survival as well as some Feature engineering on Name and variable
#Analysis will be done on the first 891 observations(from train set)


#Lets explore variable Name first as we plan to use the variable to create a variable Title that should be helpfull as Titles mattered more in that era

head(combo$Name)
names <- combo$Name
title <-  gsub("^.*, (.*?)\\..*$", "\\1", names)

combo$title <- title

table(title)
 combo$Name <- as.character(combo$Name)
 combo$Name[1]
#group rare title as factor
rare_title <- c('Dona', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
#grouping titles                
combo$title[combo$title == 'Mlle']        <- 'Miss' 
combo$title[combo$title == 'Ms']          <- 'Miss'
combo$title[combo$title == 'Mme']         <- 'Mrs' 
combo$title[combo$title == 'Lady']          <- 'Miss'
combo$title[combo$title == 'Dona']          <- 'Miss'
combo$title[combo$title %in% rare_title]  <- 'Rare Title'

 ggplot(combo[1:891,],aes(x = title,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Title V/S Survival rate")+
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 

str(combo)


#Relationships matters single people would have been more scattered around the ship the smaller families, bigger families could also be scattered in part
#creating familysize factor
combo$FamilySize <-combo$SibSp + combo$Parch + 1

combo$FamilySized[combo$FamilySize == 1]   <- 'Single'
combo$FamilySized[combo$FamilySize < 5 & combo$FamilySize >= 2]   <- 'Small'
combo$FamilySized[combo$FamilySize >= 5]   <- 'Big'

combo$FamilySized=as.factor(combo$FamilySized)

ggplot(combo[1:891,],aes(x = FamilySized,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Family Size V/S Survival Rate") +
  xlab("FamilySize") +
  ylab("Total Count") +
  labs(fill = "Survived")

str(combo)


#PCLASS class is our GPS and gives us some idea where and how far people were from the life boats and safer areas on the ship while it was still a float
ggplot(combo[1:891,],aes(x = Pclass,fill=factor(Survived))) +
geom_bar() +
ggtitle("Pclass v/s Survival Rate")+
xlab("Pclass") +
ylab("Total Count") +
labs(fill = "Survived")  


#Sex it was the ladies and kids first era where being a gentlemen ment something
ggplot(combo[1:891,],aes(x = Sex,fill=factor(Survived))) +
geom_bar() +
ggtitle("Sex v/s Survival Rate")+
xlab("Sex") +
ylab("Total Count") +
labs(fill = "Survived")  

#have a look at the status of the combined set Combo
str(combo)
colSums(is.na(combo))
colSums(combo=='')


#Prediction

#Create factors for variables we going to use
combo$Embarked=as.factor(combo$Embarked)
combo$Pclass=as.factor(combo$Pclass)
combo$title=as.factor(combo$title)
combo$Sex=as.factor(combo$Sex)
combo$Embarked=as.factor(combo$Embarked)

# Split the data back into a train set and a test set
train <- combo[1:891,c("Pclass", "title","Sex","Embarked","FamilySized","Survived")]
test <- combo[892:1309,c("Pclass", "title","Sex","Embarked","FamilySized","Survived")] 


##Random Forest
#i went for this algorithm as  I wanted to see how other variables impacted the Survived variable and the chance of survival on the ship.
#As per the one on one comparison between identified variables versus the Survived variable.
#
#
# Set a random seed
set.seed(754)

# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + title + Sex,  data = train)

# Show model error
plot(rf_model, ylim=c(0,0.50))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test.tit$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
#
#
#
## Get importance
#looking at te rank of variables on the outcome of survival
#
#
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


#Propensity as per mosaicplot below
#More men then woman bought tickets
#Most people embarked from area S 
#Wealthier people embarked from area C
#Most males as they say are third class citizen which seems to be one of the reason mostly males died.
#
mosaicplot(table(train$Pclass, train$Sex,train$Embarked), main='Population by Sex', shade=TRUE)


