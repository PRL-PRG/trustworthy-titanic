library(MASS)
test <- read.csv("../input/test.csv")
train <- read.csv("../input/train.csv")
head(train)
str(train)
summary(train)
train$Pclass <- factor(train$Pclass)
train$Survived <- factor(train$Survived)
test$Pclass <- factor(test$Pclass)
NAvalue_train <- sapply(train, function(x) sum(is.na(x)))
MissingNA_train <- data.frame(index = names(train), Missing_Values = NAvalue_train)
MissingNA_train[MissingNA_train$Missing_Values > 0, ]
NAvalue_test <- sapply(test, function(x) sum(is.na(x)))
MissingNA_test <- data.frame(index = names(test), Missing_Values = NAvalue_test)
MissingNA_test[MissingNA_test$Missing_Values > 0, ]
train$Age[is.na(train$Age)] <- median(train$Age, na.rm = TRUE)
test$Fare[is.na(test$Fare)] <- median(test$Fare, na.rm = TRUE)
test$Age[is.na(test$Age)] <- median(train$Age, na.rm = TRUE)
rl <- glm(Survived ~ Sex + Pclass + Age + SibSp, data = train, family = "binomial")
summary(rl)
previsto <- ifelse(predict(rl, newdata = train, type = "response") > 0.5, 1, 0)
train$Previsao <- previsto
tb <- table(train$Previsao >= 0.5, train$Survived)
tb
cat("Accuracy: ", (tb[1, 1] + tb[2, 2])/nrow(train) * 100, "/ Taxa de acerto:", (tb[1, 1] + tb[2, 2])/nrow(train) * 100)
predictTest <- predict(rl, newdata = test, type = "response")
test$Survived <- as.numeric(predictTest >= 0.5)
table(test$Survived)
subm <- data.frame(test$PassengerId)
names(subm)[1] <- "PassengerId"
subm$Survived <- test$Survived
write.csv(subm, file = "titanic_logr_submission.csv", row.names = FALSE)
