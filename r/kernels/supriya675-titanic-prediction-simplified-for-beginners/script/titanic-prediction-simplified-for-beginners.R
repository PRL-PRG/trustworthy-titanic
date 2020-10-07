library("randomForest")
titanic.train <- read.csv(file = "../input/train.csv", header = T, stringsAsFactors = F)
titanic.test <- read.csv(file = "../input/test.csv", header = T, stringsAsFactors = F)
str(titanic.train)
head(titanic.train)
tail(titanic.train)
str(titanic.test)
head(titanic.test)
tail(titanic.test)
titanic.test$Survived <- NA
titanic.combined <- rbind(titanic.train, titanic.test)
head(titanic.combined)
tail(titanic.combined)
titanic.combined$Family.size <- titanic.combined$SibSp + titanic.combined$Parch + 1
sapply(titanic.combined, function(x) {
    sum(is.na(x))
})
titanic.combined[titanic.combined$Embarked == "", "Embarked"] <- "S"
age.formula <- "Age ~ Pclass + Sex"
age.model <- lm(formula = age.formula, data = titanic.combined)
age.row <- titanic.combined[is.na(titanic.combined$Age), c("Pclass", "Sex")]
age.predict <- predict(age.model, newdata = age.row)
titanic.combined[is.na(titanic.combined$Age), "Age"] <- age.predict
fare.median <- median(titanic.combined$Fare, na.rm = T)
titanic.combined[is.na(titanic.combined$Fare), "Fare"] <- fare.median
titanic.combined$Pclass <- as.factor(titanic.combined$Pclass)
titanic.combined$Sex <- as.factor(titanic.combined$Sex)
titanic.combined$Embarked <- as.factor(titanic.combined$Embarked)
titanic.train <- titanic.combined[1:891, ]
titanic.test <- titanic.combined[892:1309, ]
titanic.train$Survived <- as.factor(titanic.train$Survived)
set.seed(675)
survived.formula <- as.formula("Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Family.size")
survive.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test), keep.forest = TRUE)
Survived <- predict(survive.model, newdata = titanic.test)
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
table(output.df$Survived)
write.csv(output.df, file = "titanic_submission.csv", row.names = FALSE)
plot(survive.model, ylim = c(0, 0.4))
legend("topright", colnames(survive.model$err.rate), col = 1:3, fill = 1:3)
