#setwd("~/kaggle/My kaggle/Titanic using ML")

TrainData <- read.csv(file="../input/train.csv", stringsAsFactors = FALSE, header = TRUE)
TrainData
TestData <- read.csv(file="../input/test.csv", stringsAsFactors = FALSE, header = TRUE)
TestData
TrainData$IsTrainSet <- TRUE

TestData$IsTrainSet <- FALSE

TestData$Survived <- NA

FullData <- rbind(TrainData,TestData)

FullData[FullData$Embarked=='', "Embarked"] <- 'S'

#Clean missing value of age
is.na(FullData$Age)
#table(is.na(FullData$Age))
age.mean <- mean(FullData$Age, na.rm = TRUE)
FullData[is.na(FullData$Age), "Age"] <- age.mean

# Clean missing value of fare
is.na(FullData$Fare)
table(is.na(FullData$Fare))
fare.mean <- mean(FullData$Fare, na.rm = TRUE)
FullData[is.na(FullData$Fare), "Fare"] <- fare.mean

# Cetegorical casting
FullData$Pclass <- as.factor(FullData$Pclass)
FullData$Sex <- as.factor(FullData$Sex)
FullData$Embarked <- as.factor(FullData$Embarked)


# split dataset back out into train and test
TrainData <- FullData[FullData$IsTrainSet == TRUE,]
TestData <- FullData[FullData$IsTrainSet == FALSE,]

TrainData$Survived <- as.factor(TrainData$Survived)

survived.equation <- "Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

install.packages("devtools")

install.packages("randomForest")
library(randomForest)

TitanicModel <- randomForest(formula = survived.formula, data = TrainData, ntree = 500, mtry = 3, nodesize = 0.01*nrow(TestData))

features.equation <- "Pclass + Sex + SibSp + Parch + Fare + Embarked"

Survived <- predict(TitanicModel, newdata = TestData)

PassengerId <- TestData$PassengerId
Output.df <- as.data.frame(PassengerId)
Output.df$Survived <- Survived

#write.csv(Output.df, file="kaggle_titanic_prediction.csv")

TitanicModel <- randomForest(formula = survived.formula, data = TrainData, ntree = 500, mtry = 3, nodesize = 0.01*nrow(TestData))

features.equation <- "Pclass + Sex + SibSp + Parch + Fare + Embarked"

Survived <- predict(TitanicModel, newdata = TestData)

PassengerId <- TestData$PassengerId
Output.df <- as.data.frame(PassengerId)
Output.df$Survived <- Survived

#write.csv(Output.df, file="kaggle_titanic_prediction.csv")
