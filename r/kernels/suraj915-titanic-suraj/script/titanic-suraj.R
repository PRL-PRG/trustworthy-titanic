titanic.test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
titanic.train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
titanic.train$isTrainSet <- TRUE
titanic.test$isTrainSet <- FALSE
titanic.test$Survived <- NA
titanic.full <- rbind(titanic.train, titanic.test)
titanic.full$Embarked == ""
titanic.full[titanic.full$Embarked == "", "Embarked"] <- "S"
median.age <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- median.age
median.fare <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- median.fare
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
titanic.test <- titanic.full[titanic.full$isTrainSet == FALSE, ]
titanic.train <- titanic.full[titanic.full$isTrainSet == TRUE, ]
titanic.train$Survived <- as.factor(titanic.train$Survived)
survived.equation <- "Survived ~ Pclass + Age + Sex + SibSp + Parch + Fare + Embarked "
survived.formula <- as.formula(survived.equation)
library(randomForest)
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))
features.equation <- "Pclass + Age + Sex + SibSp + Parch + Fare + Embarked "
survived <- predict(titanic.model, newdata = titanic.test)
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- survived
tail(output.df)
write.csv(output.df, file = "kaggle_submission_suraj.csv", row.names = FALSE)
