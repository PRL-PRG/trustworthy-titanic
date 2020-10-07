titanic.train <- read.csv("../input/train.csv")
titanic.test <- read.csv("../input/test.csv")
head(titanic.train)
tail(titanic.train)
summary(titanic.train)
str(titanic.train)
head(titanic.test)
tail(titanic.test)
summary(titanic.test)
str(titanic.test)
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE
names(titanic.train)
names(titanic.test)
titanic.test$Survived <- NA
titanic.full <- rbind(titanic.train, titanic.test)
table(titanic.full$IsTrainSet)
summary(titanic.full)
table(titanic.full$Embarked)
titanic.full[titanic.full$Embarked == "", ]
titanic.full[titanic.full$Embarked == "", "Embarked"] <- "S"
titanic.full[titanic.full$Embarked == "", ]
titanic.full[titanic.full$PassengerId == 62 | titanic.full$PassengerId == 830, ]
table(is.na(titanic.full$Age))
median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- median(titanic.full$Age, na.rm = TRUE)
table(titanic.full$Age)
titanic.full[is.na(titanic.full$Fare), ]
median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full$Cabin <- NULL
str(titanic.full)
titanic.full$PassengerId <- as.factor(titanic.full$PassengerId)
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE, ]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE, ]
titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)
library(ggplot2)
g <- ggplot(data = titanic.train, aes(x = Sex, fill = Survived)) + geom_bar()
g
g <- ggplot(data = titanic.train, aes(x = Pclass, fill = Survived)) + geom_bar()
g
g <- ggplot(data = titanic.train, aes(x = Embarked, fill = Survived)) + geom_bar(position = "dodge")
g
table(titanic.train$Embarked, titanic.train$Survived)
g <- ggplot(data = titanic.train, aes(x = SibSp, fill = Survived)) + geom_bar()
g
g <- ggplot(data = titanic.train, aes(x = SibSp, fill = Survived)) + geom_bar() + facet_grid(~Sex)
g
g <- ggplot(data = titanic.train, aes(x = SibSp, fill = Survived)) + geom_bar() + facet_wrap(~Pclass)
g
g <- ggplot(data = titanic.train, aes(x = Age)) + geom_density()
g
g <- ggplot(data = titanic.train, aes(x = Fare)) + geom_density() + facet_grid(~Pclass)
g
g <- ggplot(titanic.train, aes(x = Pclass, y = Fare)) + geom_boxplot()
g
g <- ggplot(titanic.train, aes(x = Pclass, y = Fare, color = Pclass)) + geom_boxplot() + geom_jitter()
g
g <- ggplot(titanic.train, aes(x = Pclass, y = Fare, color = Survived)) + geom_boxplot() + geom_jitter() + ggtitle("Titanic Survival by Class")
g
library(randomForest)
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.train))
Survived <- predict(titanic.model, newdata = titanic.test)
PassengerID <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerID)
output.df$Survived <- Survived
write.csv(output.df, "titanic_kaggle_submission.csv", row.names = FALSE)
