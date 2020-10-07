train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
library(ggplot2)
library(ggthemes)
library(dplyr)
test$Survived <- "NA"
train$TrainOrTest <- "Train"
test$TrainOrTest <- "Test"
full <- rbind(train, test)
str(full)
full$FamilySize <- 1 + full$SibSp + full$Parch
table(full$Sex)
sum(is.na(full$Age))
table(full$FamilySize)
sum(is.na(full$Fare))
table(full$Embarked)
table(full$Pclass)
UpperQuartileAge <- boxplot.stats(full$Age)$stats[5]
NonAgeOutliers <- full$Age < UpperQuartileAge
full_NoAgeOutliers <- full[NonAgeOutliers, ]
summary(full_NoAgeOutliers$Age, na.rm = TRUE)
set.seed(123)
Age.equation = "Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked"
Age_Model <- lm(formula = Age.equation, data = full_NoAgeOutliers)
Blank_Age_Rows <- full[is.na(full$Age), c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked")]
Predicted_Age_Rows <- predict(Age_Model, newdata = Blank_Age_Rows)
full[is.na(full$Age), "Age"] <- Predicted_Age_Rows
full[is.na(full$Age), "Age"]
summary(full$Age)
full[full$Age < 0, "Age"] <- median(full$Age)
UpperQuartileFare <- boxplot.stats(full$Fare)$stats[5]
NonFareOutliers <- full$Fare < UpperQuartileFare
full_NoFareOutliers <- full[NonFareOutliers, ]
summary(full_NoFareOutliers$Fare, na.rm = TRUE)
set.seed(1234)
Fare.equation = "Fare ~ Pclass + Sex + SibSp + Parch + Age + Embarked"
Fare_Model <- lm(formula = Fare.equation, data = full_NoFareOutliers)
Blank_Fare_Rows <- full[is.na(full$Fare), c("Pclass", "Sex", "SibSp", "Parch", "Age", "Embarked")]
Predicted_Fare_Rows <- predict(Fare_Model, newdata = Blank_Fare_Rows)
full[is.na(full$Fare), "Fare"] <- Predicted_Fare_Rows
full[is.na(full$Fare), "Fare"]
full[full$Fare < 0, "Fare"] <- median(full$Fare)
full[is.na(full$Embarked), "Embarked"] <- "S"
str(full)
full$AgeGroup <- cut(full$Age, breaks = c(-1, 5, 10, 20, 30, 40, 50, 60, 70, 80))
full$FareGroup <- cut(full$Fare, breaks = c(-1, 5, 10, 25, 50, 75, 100, 150, 200, 300, 500, 1000))
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
install.packages("randomForest")
library(randomForest)
titanic_model <- randomForest(factor(Survived) ~ Pclass + Sex + AgeGroup + FamilySize + FareGroup + Embarked, data = full[full$TrainOrTest == "Train", ])
str(test)
set.seed(12345)
Prediction <- predict(titanic_model, full[full$TrainOrTest == "Test", ])
Submission <- data.frame(PassengerID = test$PassengerId, Survived = Prediction)
write.csv(Submission, file = "Submission.csv", row.names = FALSE)
