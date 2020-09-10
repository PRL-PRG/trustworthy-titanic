## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Get all the pakcages you need
library(ggplot2) # visualisation
library(Amelia) # missing data visualisation
library(caret)
library(dplyr) # data manipulation
library(randomForest) # Classification algorithm 
library(epiR) # For measuring OR  


# Load data
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

# Combine train and test data. Before combining makes sure the columns match, create a Survived variable in the test data
test$Survived <- NA   
# combine data
full <- rbind(train, test)

# Check the data
head(full, 10)
tail(full, 10)
# check the structure of the data
str(full)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Any missing data?
colSums(is.na(full))

# Age has 263 missing values, fare has 1 missing value. 

# Any empty data?

colSums(full == "")
# Cabin has 1014 empty values whereas embarked has 2.

# Visualise missing data
missmap(full, main = "Missing values vs observed")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Drop the cabin variable
full$Cabin <- NULL
# check to see if variable has been removed
str(full) # it has been removed


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Let us find the most common port so that we can impute it into the missing values
table(full$Embarked)
# it appears that S is the most common port

# Impute s into the missing values 
full[full$Embarked == "", "Embarked"] <- "S"

# Check to see what's changed 
table(full$Embarked)
# We don't have any missing values for Embarked anymore


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Make new Age column
age <- full$Age
n = length(age)

# Replace missing value with a random sample from raw data
set.seed(1)
for(i in 1:n){
  if(is.na(age[i])){
    age[i] = sample(na.omit(full$Age),1)
  }
}

# Plot graph to see the effect of the change
ggplot(full, aes(age))+
  geom_histogram(fill="Dark Green", color = "Black") +
  labs(title="Before replacement") 
ggplot(full, aes(Age) ) +
  geom_histogram(fill="Dark Blue", color = "Black") +
  labs(title="After replacement")

full$Age <- age
colSums(is.na(full)) ###check
# The are 0 missing values for age now.


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Find out other information on the individual with now fare data
full[is.na(full$Fare),]

# Passenger 1044 is from pclass 3 and has embarked from port 3
# Before we use the mean or median, we need to first have a look at the distribution 
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = 'light green', alpha=0.5)
 # The data seems to be skewed to the right. 
 # In this case the median is the best approach to help us impute the Fare value

# So we use the Pclass and Embarked values to help us impute the Fare Value for passenger 1044.
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Check to see if we have made any changes to the data.
full[1044,] # Yes we have!


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Make sure the variables are in the right structure
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Name', 'Survived')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Split Data apart
# Split the train model
train.cl <- full[1:891,]

## plit the test model 
test.cl <- full[892:1309,]
# Check to see if it split right!
str(train.cl)
str(test.cl)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create an age group column with Child & Adult

train.cl$Agegrp[train.cl$Age < 18] <- 'Child'
train.cl$Agegrp[train.cl$Age >= 18] <- 'Adult'

# Visualise the relationship between age group and Survival
ggplot(train.cl, aes(Agegrp,fill = Survived)) +
    geom_bar() +
   labs(title = "Age-groups Vs Suvival rate") +
   labs(xlab("Age-Group")) +
   labs(ylab("Count"))+
  facet_grid(.~Sex)
 

## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Make a 2x2 table having Adults as the reference group
tab.age <- table(train.cl$Agegrp, train$Survived)
tab.age
# Check out the OR to see the strength of the association between Age group and suvival rate.
epi.2by2(tab.age, method="cohort.count")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(train.cl, aes(Sex,fill = Survived)) +
    geom_bar() +
   labs(title = "Gender Vs Suvival rate") +
   labs(xlab("Gender")) +
   labs(ylab("Count"))
# Female survival rate appears to be greater then male survival rate.

# Odds Ratio and Chi2 test to see the likelihood of surival rate based on sex
# First, make a 2x2 table having Females as the referene group.
tab.sex <- table(train$Sex, train$Survived)
tab2.sex <- cbind(tab.sex[,2], tab.sex[,1]) 
colnames(tab2.sex) <- c("Survived", "Not Survived")
tab2.sex

#Now do the OR and chi2 test
epi.2by2(tab2.sex, method="cohort.count")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(train.cl, aes(Pclass,fill = Survived)) +
    geom_bar() +
   labs(title = "Ticket Class Vs Suvival rate") +
   labs(xlab("Ticket Class")) +
   labs(ylab("Count"))

# Those in the lower Pclasses have a higher survival rate than those in the lower Pclass.

# Family Size vs Survival
ggplot(train.cl, aes(x=Parch, fill=Survived)) +
  geom_histogram(stat = "count")+
  labs(title = "Family Size (Parents and Children) Vs Suvival rate") +
   labs(xlab("Family Size")) +
   labs(ylab("Count"))



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(train.cl, aes(x=SibSp, fill=Survived)) +
  geom_histogram(stat = "count") +
  labs(title = "Family Size (Siblings and spouses) Vs Suvival rate") +
   labs(xlab("Family Size")) +
   labs(ylab("Count"))

# Since Pclass SibSp are similiar and have the same distribution, we can cobine them together.

family <- train.cl$SibSp + train.cl$Parch

ggplot(train.cl, aes(x=family, fill=Survived)) +
  geom_histogram(stat = "count") +
  labs(title = "Family Size Vs Suvival rate") +
   labs(xlab("Family Size")) +
   labs(ylab("Count"))        

# Those with little to none family members appear have a higher survival rate than those with bigger families.


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# random seed 
set.seed(992)

# deploy your model
model_rf <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                            Fare + Embarked, data = train.cl)

# Graph model error
plot(model_rf, ylim=c(0,0.36))



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Use the test data to make a prediction
prediction <- predict(model_rf, test.cl)

# Save the solution to a dataframe and write the solutuion into an excel file
solution <- data.frame(PassengerID = test.cl$PassengerId, Survived = prediction)
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)


