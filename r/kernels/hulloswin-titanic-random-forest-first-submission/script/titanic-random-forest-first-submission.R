#Set Working Directory

setwd("../input")

#Import Data

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

#Various Stats About Tables

median(titanic.train$Age, na.rm = TRUE)
median(titanic.test$Age, na.rm = TRUE)
ncol(titanic.test)
ncol(titanic.train)
names(titanic.test)
names(titanic.train)
#table(titanic.full$IsTrainSet)
#tail(titanic.full)

#Adding Indicator for Training Set before Union

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

#Adding Survived Column missing from test to UNION

titanic.test$Survived <- NA

#UNION

titanic.full <- rbind(titanic.train, titanic.test)

#Clean
titanic.full$Embarked==''

titanic.full[titanic.full$Embarked=='', "Embarked"] 

#table(titanic.full$Embarked)

#Replace nulls with Mode

titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

#Replace nulls with Mean (Not Optimal)
age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

table(is.na(titanic.full$Age))

fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median
#  table(is.na(titanic.full$Fare))
  
#This is a better way to clean the data (Regression Model to Predict Fare)

#find outiliers of fare and build linear model to impute missing values
#boxplot(titanic.full$Fare)
#boxplot.stats(titanic.full$Fare)

#upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
#outlier.filter <- titanic.full$Fare < upper.whisker
#titanic.full[outlier.filter, ]
#fare.equation = "Fare ~ Pclass + Sex + Age + SibSp +Parch + Embarked"

#fare.model <- lm(
 # formula = fare.equation,
  #data = titanic.full[outlier.filter, ]
#)


#fare.row <- titanic.full[is.na(titanic.full$Fare), 
 #            c("Pclass" , "Sex" , "Age" , "SibSp" ,"Parch" , "Embarked")]

#fare.predictions <- predict(fare.model, newdata = fare.row )
#titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions
  
  

#Categorical Casting
  
#str(titanic.full)

titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
  
  
#Split Data back to Train and Test
  
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE, ]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE, ]
#tail(titanic.train)
#tail(titanic.test)

# Changed Survived back to Factor
titanic.train$Survived <- as.factor(titanic.train$Survived)

#Build Model

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

library(randomForest)

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId

output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived


#Write submission file
#write.csv(output.df, file="kaggle-submission.csv", row.names = FALSE)

#tail(output.df)























