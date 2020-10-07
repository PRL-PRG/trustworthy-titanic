library(tidyverse)
list.files(path = "../input")
library(tidyverse)
list.files(path = "../input")
titanic.train <- read.csv(file = "../input/train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "../input/test.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.train$IstrainSet <- TRUE
titanic.test$IstrainSet <- FALSE
titanic.test$Survived <- NA
titanic.full <- rbind(titanic.train, titanic.test)
median(titanic.train$Age)
table(titanic.full$Embarked)
titanic.full[titanic.full$Embarked == "", "Embarked"]
titanic.full[titanic.full$Embarked == "", "Embarked"] <- "S"
library(stringr)
Title <- str_extract(titanic.full$Name, " ([A-Za-z]+)[.]")
titanic.full$Title <- Title
library(dplyr)
upper.whisker <- boxplot.stats(titanic.full$Fare)$stats[5]
nonoutlier <- titanic.full$Fare < upper.whisker
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
titanic.full$Name <- as.factor(titanic.full$Name)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Title <- as.factor(titanic.full$Title)
titanic.full$Ticket <- as.factor(titanic.full$Ticket)
titanic.full$FamilySize <- titanic.full$SibSp + titanic.full$Parch + 1
table(titanic.full$FamilySize)
titanic.full$FamilySizelabel <- sapply(1:nrow(titanic.full), function(x) ifelse(titanic.full$FamilySize[x] == 1, "Single", ifelse(titanic.full$FamilySize[x] > 4, "Large", "Small")))
titanic.full$FamilySizelabel <- as.factor(titanic.full$FamilySizelabel)
library(rpart)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize + Ticket, data = titanic.full[!is.na(titanic.full$Age), ], method = "anova")
titanic.full$Age[is.na(titanic.full$Age)] <- predict(Agefit, titanic.full[is.na(titanic.full$Age), ])
fare.eq = "Fare~Pclass+Sex+Parch+Age+SibSp+Embarked+Title+FamilySizelabel"
fare.model <- lm(formula = fare.eq, data = titanic.full[nonoutlier, ])
fare.row <- titanic.full[is.na(titanic.full$Fare), c("Pclass", "Sex", "Age", "Embarked", "SibSp", "Parch", "Title", "FamilySizelabel")]
fare.predictions <- predict(fare.model, newdata = fare.row)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
titanic.full$Name <- as.factor(titanic.full$Name)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$FamilySizelabel <- as.factor(titanic.full$FamilySizelabel)
titanic.full$Title <- as.factor(titanic.full$Title)
titanic.full$Ticket <- as.factor(titanic.full$Ticket)
titanic.train <- titanic.full[titanic.full$IstrainSet == TRUE, ]
titanic.test <- titanic.full[titanic.full$IstrainSet == FALSE, ]
titanic.train$Survived <- as.factor(titanic.train$Survived)
Survived.eq <- "Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+FamilySize+FamilySizelabel+Title"
survive.formula <- as.formula(Survived.eq)
titanic.model <- randomForest(formula = survive.formula, data = titanic.train, ntree = 2000, mtry = 3, nodesize = 0.01 * nrow(titanic.test))
Survived <- predict(titanic.model, titanic.test, OOB = TRUE, type = "response")
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
write.csv(output.df, file = "titanic_kaggle_13.csv", row.names = FALSE)
