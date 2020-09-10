titanic.train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
titanic.test <- read.csv("../input/test.csv",  stringsAsFactors=FALSE)

# Check the type of variables
str(titanic.train)    
str(titanic.train)

# Check Summary of data
summary(titanic.train)
summary(titanic.train)

# Check missing values in Train data
sum(is.na(titanic.train))

# Age
sum(is.na(titanic.train$Age))

# Embarked
table(titanic.train$Embarked)

#Fare
sum(is.na(titanic.train$Fare))

# Check missing values in test data
sum(is.na(titanic.test))

# Age
sum(is.na(titanic.test$Age))

# Embarked
table(titanic.test$Embarked)

#Fare
sum(is.na(titanic.test$Fare))

# Merge the training and test data for fill the missing values
# Number of Columns
ncol(titanic.train)
ncol(titanic.test)

# Identify training and testing data set
titanic.train$isTrainSet <- TRUE
titanic.test$isTrainSet <- FALSE

# Columns name
colnames(titanic.train)
colnames(titanic.test)

# Add one new column in test data set and the column name is "Survived"
titanic.test$Survived <- NA

# Check dimension of both data
dim(titanic.train)
dim(titanic.test)

# Now our data is ready to merging

Titanic.full <- rbind(titanic.train, titanic.test)

# You can also this line
#Titanic.full <-  dplyr::bind_rows(titanic.train, titanic.test)

418 + 891

# Check missing values
sum(is.na(Titanic.full))

# Now fill the missing values of the variables "Age", "Fare", "Embarked"
# Embarked
table(Titanic.full$Embarked)
Titanic.full[Titanic.full$Embarked == '', "Embarked"] <- 'S'

# Age
table(is.na(Titanic.full$Age))
sum(is.na(Titanic.full$Age))

titanic.age_median <- median(Titanic.full$Age, na.rm = TRUE)
Titanic.full[is.na(Titanic.full$Age), "Age"] <- titanic.age_median

# Fare
table(is.na(Titanic.full$Fare))
sum(is.na(Titanic.full$Fare))

titanic.fare_median <- median(Titanic.full$Fare, na.rm = TRUE)
Titanic.full[is.na(Titanic.full$Fare), "Fare"] <- titanic.fare_median

# Convert the Chr to Factor some variables like "Pclass", "Sex", "Embarked"
Titanic.full$Pclass <- as.factor(Titanic.full$Pclass)
Titanic.full$Sex <- as.factor(Titanic.full$Sex)
Titanic.full$Embarked <- as.factor(Titanic.full$Embarked)

# Check the structure of all variables
str(Titanic.full)

# Now split the data in training and testing
titanic.train <- Titanic.full[Titanic.full$isTrainSet == TRUE,]
titanic.test <- Titanic.full[Titanic.full$isTrainSet == FALSE,]

# Again check the missing values for training and testing data
# Train
sum(is.na(titanic.train$Age))
sum(is.na(titanic.train$Fare))
sum(is.na(titanic.train$Embarked))

# Test
sum(is.na(titanic.test$Age))
sum(is.na(titanic.test$Fare))
sum(is.na(titanic.test$Embarked))

sum(is.na(titanic.train))
sum(is.na(titanic.test))

# Remove "isTrainSet" column from the tranig and test data and also remove "Survived" from the test dataset
titanic.train$isTrainSet <- NULL
titanic.test$isTrainSet <- NULL
titanic.test$Survived <- NULL

# The variable name "Survived" chr to Factor in training dataset
titanic.train$Survived <- as.factor(titanic.train$Survived)

# Apply randomForest in traing data set
# All variables or columns names
colnames(titanic.train)

# "PassengerId" "Survived"    "Pclass"      "Name"        "Sex"
# "Age"         "SibSp"       "Parch"       "Ticket"      "Fare"       
# "Cabin"       "Embarked"  

# Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked

library(randomForest)
rf <- randomForest(formula = Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = titanic.train, ntree = 300)
print(rf)
attributes(rf)
# We get out of bag error 16.61% and the accuracy is 83.39%

# Prediction with training data set and confusion matrix
library(caret)
library(lubridate)
library(lattice)
library(ggplot2)

prd.train <- predict(rf, titanic.train)
print(head(prd.train))
# confusionMatrix(prd.train, titanic.train$Survived)

# Prediction with test data set
prd.test <- predict(rf, titanic.test)
print(head(prd.test))
print(tail(prd.test))

# Error rate of RandomForest
plot(rf)
# In plot we see when the numbers of trees are increses the the OOB error is decreses so we need to chose number of tree is 100-150

# We need "PassengerId" and "Survived" for submitting the result in kaggle.
PassengerId <- titanic.test$PassengerId
str(PassengerId)

# Convert in to dataframe
PassengerId.df <- as.data.frame(PassengerId)
str(PassengerId.df)

# Now fill the "Survived" column in the PassengerId.df
PassengerId.df$Survived <- prd.test

tail(PassengerId.df)

# Export the data in CSV format.
write.csv(PassengerId.df, file = "Titanic_Kaggle_Submission.csv", row.names = FALSE) 






# No. of nodes for the trees
hist(treesize(rf), main = "no of nodes per tress", col = "green")

# Check which variables play the importance in the model
varImpPlot(rf)

# Top 5
varImpPlot(rf, sort = TRUE, n.var = 5)


