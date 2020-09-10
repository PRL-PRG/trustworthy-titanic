
#Hello to everyone, I am an Aeronautical Engineer working as Research Assistant.
#This is my very first code in kaggle and also in my life time and i would like to thank the kagglers for your submissions.They gave me some good idea about the codings.

##Loading of all Libraries##

library('dplyr') # Data Manipulation
library('mice') # Data Imputaion
library('ggplot2') # Data Visualization
library('ggthemes') # Data Visualization
library('Hmisc') # Exploratory Analysis
library('caret') # Classification and Regression
library('randomForest') # Classification and Regression
library('e1071') # Statistical Methods

##Loading and Consolidation of Data##

train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

##Joining of Training and Testing Dataset##

Titanic <- bind_rows(train, test)

##Summary of Whole Titanic Dataset##

summary(Titanic)

##To Understand the Structure of the Dataset##

str(Titanic) 

##To know the Missing Values in given Dataset##

colSums(Titanic=='')

##To know the number of NA in dataset#

colSums(is.na(Titanic))

##Data Visualization (start with the first variable "Pclass", which gives the basic understanding of this survival Analysis)##

ggplot(Titanic[1:891,],aes(x = Pclass,fill=factor(Survived))) +
                        geom_bar() + ggtitle("Pclass versus Survival Rate")+
                          xlab("Pclass") + ylab("Count") + labs(fill = "Survived")
 # The Visualization Clearly explains the 1st class peoples had a better surviaval rate than other class#
##Feature Analysis on Name##

Titanic$Title <- gsub('(.*, )|(\\..*)', '',Titanic$Name)

table(Titanic$Title)

table(Titanic$Sex, Titanic$Title)

 # combine and Replace the small titles for understanding#
Titanic$Title[Titanic$Title == 'Ms'] <- 'Miss'
Titanic$Title[Titanic$Title == 'Lady'] <- 'Miss'
Titanic$Title[Titanic$Title == 'Mme'] <- 'Mrs'
Titanic$Title[Titanic$Title == 'Mlle'] <- 'Miss'

rare_title <- c('Dona','the Countess','Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

Titanic$Title[Titanic$Title %in% rare_title]  <- 'Officer'

table(Titanic$Sex, Titanic$Title)

 
ggplot(Titanic[1:891,],aes(x = Title,fill=factor(Survived))) +
                        geom_bar() + ggtitle("Title Versus Survival rate")+
                         xlab("Title") + ylab("Count") + labs(fill = "Survived") 
 # The above plot proves the female has a very good survuival rate"

##Exploratory analysis on Age##

sum(is.na(Titanic$Age))

 # Conveting the Useful variables into factors#

variables <- c('PassengerId','Pclass','Sex','Embarked','Title')

Titanic[variables] <- lapply(Titanic[variables], function(x) as.factor(x))

 #Imputation#

mice_mod <- mice(Titanic[, !names(Titanic) %in% c('PassengerId','Name','Ticket','Cabin','Sibsp','Parch','Survived','Fare')], method='rf') 

mice_output <- complete(mice_mod)

 #Comparison of oth original and mice age data#

hist(Titanic$Age, freq=F, main='Original Age ',col='darkgreen', ylim=c(0,0.05))

hist(mice_output$Age, freq=F, main='MICE Output Age',col='Red', ylim=c(0,0.05))
 # the comparison of data almost same so we can use the impute the mice age into original dataset#

Titanic$Age <- mice_output$Age

sum(is.na(Titanic$Age))

ggplot(Titanic[1:891,], aes(Age, fill = factor(Survived))) + 
                          geom_histogram() + facet_grid(.~Sex) + theme_few()

##Feature Analysis on Family Size using the Passenger, SibSp and Parch Data##

Titanic$FamilySize <-Titanic$SibSp + Titanic$Parch + 1

Titanic$FamilySized[Titanic$FamilySize == 1]   <- 'Single'
Titanic$FamilySized[Titanic$FamilySize < 5 & Titanic$FamilySize >= 2]   <- 'Small'
Titanic$FamilySized[Titanic$FamilySize >= 5]   <- 'Big'

Titanic$FamilySized=as.factor(Titanic$FamilySized)

ggplot(Titanic[1:891,],aes(x = FamilySized,fill=factor(Survived))) +
                        geom_bar() + ggtitle("Family Size V/S Survival Rate") +
                          xlab("FamilySize") + ylab("Total Count") + labs(fill = "Survived")
 # Single and Small size family has a good surviaval rate than big family#

##Exploratory analysis on Embarked##

table(Titanic$Embarked)

Titanic$Embarked[Titanic$Embarked ==''] = 'S'

table(Titanic$Embarked)
 
ggplot(Titanic[1:891,],aes(x = Embarked,fill=factor(Survived))) +
                        geom_bar() + ggtitle("Embarked vs Survival") +
                          xlab("Embarked") + ylab("Total Count") + labs(fill = "Survived") 

##Spliting the data into a training set and a testing set##

Train <- Titanic[1:600,]

Train_1 <- Titanic[601:891,]
 #30 percent of data has been splited to cross verify the result#

Test <- Titanic[892:1309,]

# Set a random seed

set.seed(754)

## Model Building##

Model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                          Embarked + Title +FamilySized,data = Train)

##Get importance##

importance    <- importance(Model)

varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

##**Predict using the train_1 set (For Cross Verification)**##
 #prediction <- predict(Model, Train_1)#
 #solution <- data.frame(PassengerID = Train_1$PassengerId, Survived = prediction)#
 #write.csv(solution, file = 'Solution_Train_1.csv', row.names = F)#
 # The model accuracy is 82 percent#
 
##Predict using the test set##

prediction <- predict(Model, Test)

##Save the solution to a dataframe##

solution <- data.frame(PassengerID = Test$PassengerId, Survived = prediction)

##Write the solution to file##

write.csv(solution, file = 'Solution_Test.csv', row.names = F)
