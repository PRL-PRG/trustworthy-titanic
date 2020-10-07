library(readr)
library(randomForest)
library(caret)
titanic_train <- read.csv("../input/train.csv", header = TRUE, dec = ".", sep = ",")
titanic_test <- read.csv("../input/test.csv", header = TRUE, dec = ".", sep = ",")
titanic_test$Survived <- NA
titanic_total <- rbind(titanic_train, titanic_test)
str(titanic_total)
titanic_total <- titanic_total[, -c(1, 9, 11)]
impute <- preProcess(titanic_total, method = "bagImpute")
titanic_total <- predict(impute, titanic_total)
titanic_total$Survived[892:1309] <- NA
table(titanic_total$Embarked)
titanic_total$Embarked <- as.character(titanic_total$Embarked)
titanic_total$Embarked[titanic_total$Embarked == ""] <- "S"
table(titanic_total$Embarked)
titanic_total$Family <- 1 + titanic_total$SibSp + titanic_total$Parch
titanic_total$Stage[titanic_total$Age < 18] <- "Child"
titanic_total$Stage[titanic_total$Age >= 18] <- "Adult"
titanic_total$Name <- as.character(titanic_total$Name)
titanic_total$Title <- sapply(titanic_total$Name, FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][2]
})
titanic_total$Title <- sub(" ", "", titanic_total$Title)
titanic_total$Title[titanic_total$Title %in% c("Mme", "Mlle")] <- "Mlle"
titanic_total$Title[titanic_total$Title %in% c("Capt", "Don", "Major", "Sir")] <- "Sir"
titanic_total$Title[titanic_total$Title %in% c("Dona", "Lady", "the Countess", "Jonkheer")] <- "Lady"
titanic_total <- titanic_total[, -3]
titanic_total$Survived <- as.factor(titanic_total$Survived)
titanic_total$Pclass <- factor(titanic_total$Pclass, ordered = TRUE, levels = c("1", "2", "3"))
titanic_total$Embarked <- as.factor(titanic_total$Embarked)
titanic_total$Stage <- as.factor(titanic_total$Stage)
titanic_total$Title <- as.factor(titanic_total$Title)
standard <- function(x) (x - min(x))/(max(x) - min(x))
titanic_total$Age <- standard(titanic_total$Age)
titanic_total$SibSp <- standard(titanic_total$SibSp)
titanic_total$Parch <- standard(titanic_total$Parch)
titanic_total$Family <- standard(titanic_total$Family)
titanic_total$Fare <- standard(titanic_total$Fare)
dummy.vars <- dummyVars(~., data = titanic_total[, -1])
dummy_titanic <- predict(dummy.vars, titanic_total[, -1])
titanic_total <- data.frame(titanic_total[, 1], dummy_titanic)
colnames(titanic_total)[1] <- "Survived"
str(titanic_total)
titanic_train <- titanic_total[1:891, ]
titanic_test <- titanic_total[892:1309, ]
set.seed(415)
fit_rf <- randomForest(Survived ~ ., data = titanic_train, importance = TRUE, ntree = 2000)
titanic_pred <- predict(fit_rf, newdata = titanic_test)
summary(titanic_pred)
submit <- data.frame(PassengerId = 892:1309, Survived = 0)
submit$Survived <- titanic_pred
write.csv(submit, "gender_submission.csv", row.names = FALSE)
