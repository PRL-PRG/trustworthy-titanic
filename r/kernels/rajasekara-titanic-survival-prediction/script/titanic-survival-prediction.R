# read data
train <- read.csv('../input/train.csv')
test  <- read.csv('../input/test.csv')

library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation


#create Survived column in test
test$Survived <- "NA" 

#create Train or Test column in both
train$TrainOrTest <- "Train"
test$TrainOrTest <- "Test"

# Union both train and test
full <- rbind(train, test)

str(full)

#create Family Size column
full$FamilySize <- 1+full$SibSp+full$Parch

table(full$Sex) #0
sum(is.na(full$Age)) #263
table(full$FamilySize) #0
sum(is.na(full$Fare)) #1
table(full$Embarked) #2
table(full$Pclass) #0

        
        # 1. Predictive Model to predict the missing Age values 
        UpperQuartileAge <- boxplot.stats(full$Age)$stats[5] #Set Upper Whisker as limit to exclude outliers
        NonAgeOutliers <- full$Age < UpperQuartileAge #Get the set with no outliers
        full_NoAgeOutliers <- full[NonAgeOutliers,] # filter dataset with no Age outliers
        summary(full_NoAgeOutliers$Age, na.rm = TRUE)
        
           # Build model
        set.seed(123)
        Age.equation = "Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked" # formula for lm
        Age_Model <- lm (
                formula = Age.equation, 
                data = full_NoAgeOutliers # data without outliers
        )
        Blank_Age_Rows <- full[
                is.na(full$Age), #blank Age
                c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked") # include only these columns
                ]
        Predicted_Age_Rows <- predict(Age_Model, newdata = Blank_Age_Rows)
        
        # Add these predicted missing Age back to the full dataset
        full[is.na(full$Age),"Age" ]     <- Predicted_Age_Rows # replaces only the cells in Age column with NAs with the newly predicted values
        
        full[is.na(full$Age),"Age" ] # now this should not return any values, as the missing rows are already replaced with predicted Age values
        summary(full$Age)
        full[full$Age<0,"Age"] <- median(full$Age)
        
        # 2. Predictive Model to predict the missing Fare values 
        UpperQuartileFare <- boxplot.stats(full$Fare)$stats[5] #Set Upper Whisker as limit to exclude outliers
        NonFareOutliers <- full$Fare < UpperQuartileFare #Get the set with no outliers
        full_NoFareOutliers <- full[NonFareOutliers,] # filter dataset with no Fare outliers
        summary(full_NoFareOutliers$Fare, na.rm = TRUE)
        
        # Build model
        set.seed(1234)
        Fare.equation = "Fare ~ Pclass + Sex + SibSp + Parch + Age + Embarked" # formula for lm
        Fare_Model <- lm (
                formula = Fare.equation, 
                data = full_NoFareOutliers # data without outliers
        )
        
        # predict missing Age values
        Blank_Fare_Rows <- full[
                is.na(full$Fare), #blank Fare
                c("Pclass", "Sex", "SibSp", "Parch", "Age", "Embarked") # include only these columns
                ]
        Predicted_Fare_Rows <- predict(Fare_Model, newdata = Blank_Fare_Rows)
        
        # Add these predicted missing Fare back to the full dataset
        full[is.na(full$Fare),"Fare" ] <- Predicted_Fare_Rows # replaces only the cells in Age column with NAs with the newly predicted values
        
        full[is.na(full$Fare),"Fare" ] # now this should not return any values, as the missing rows are already replaced with predicted Age values
        full[full$Fare<0,"Fare"] <- median(full$Fare)
          
        # 3. Impute missing Embarked values 
        full[is.na(full$Embarked),"Embarked"] <- "S"
        
str(full)

full$AgeGroup <-  cut(full$Age , breaks = c(-1,5,10,20,30,40,50,60,70,80))
full$FareGroup <- cut(full$Fare, breaks = c(-1,5,10,25,50,75,100,150,200,300,500,1000))

        
#Identify Factors
full$Pclass <- as.factor(full$Pclass)
full$Sex <- as.factor(full$Sex)
full$AgeGroup <- as.factor(full$AgeGroup)
full$FamilySize <- as.factor(full$FamilySize)
full$SibSp <- as.factor(as.character(full$SibSp))
full$Parch <- as.factor(as.character(full$Parch))
full$FareGroup <- as.factor(full$FareGroup)
full$Embarked <- as.factor(full$Embarked)
full$Survived <- as.factor(full$Survived)
str(full)


# build model
install.packages("randomForest")
library(randomForest)

titanic_model<-randomForest(factor(Survived) ~ Pclass + Sex + AgeGroup + FamilySize + FareGroup + Embarked, data = full[full$TrainOrTest == "Train",])
str(test)
set.seed(12345)
Prediction <- predict(titanic_model, full[full$TrainOrTest == "Test",])

Submission <- data.frame(PassengerID = test$PassengerId, Survived = Prediction)

write.csv(Submission, file = 'Submission.csv', row.names = FALSE)
