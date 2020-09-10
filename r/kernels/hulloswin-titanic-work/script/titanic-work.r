
#Set Working Directory

setwd("../input")

#Import Data

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)






#Explore Various Stats About Tables

#median(titanic.train$Age, na.rm = TRUE)
#median(titanic.test$Age, na.rm = TRUE)
#ncol(titanic.test)
#ncol(titanic.train)
#names(titanic.test)
#names(titanic.train)

str(titanic.test)
head(titanic.test)

#Adding Indicator for Training Set before Unioning both datasets

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

#Adding Survived Column which is missing from test set so that I can Union

titanic.test$Survived <- NA

#UNION

titanic.full <- rbind(titanic.train, titanic.test)

#Show top of full data set

head(titanic.full)

#Cleaning the Data

#Replacing null Embarks with "S" since that is the mode (Not Optimal)

titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

#Replace nulls Ages with Mean (Not Optimal)
age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

#Replace null fares with median fare (Not Optimal)

fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median
 # table(is.na(titanic.full$Fare))

#Casting features as factors for computations

titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

#Split Data back to Train and Test
  
titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE, ]
titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE, ]
tail(titanic.train)
tail(titanic.test)

# Change Survived back to Factor
titanic.train$Survived <- as.factor(titanic.train$Survived)

#Build the Actual Model

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

library(randomForest)

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId

output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived

#Look at results

head(output.df)

#Write the File to working directory

#write.csv(output.df, file="kaggle-submission.csv", row.names = FALSE)
