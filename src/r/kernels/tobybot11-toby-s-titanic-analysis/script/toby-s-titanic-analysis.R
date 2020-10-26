options(width = 120)
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
str(train)
head(train, 1)
table(train$Sex)
summary(train$Sex)
prop.table(table(train$Sex))
prop.table(table(train$Survived))
train$SurvivedBoolean <- as.logical(train$Survived)
train$SurvivedLabel[train$SurvivedBoolean == TRUE] <- "Survived"
train$SurvivedLabel[train$SurvivedBoolean == FALSE] <- "Died"
prop.table(table(train$Sex, train$SurvivedLabel))
prop.table(table(train$Sex, train$SurvivedLabel), 1)
summary(train$Age)
train$Child <- FALSE
train$Child[train$Age < 18] <- TRUE
table(train$Child)
table(train$Child, train$Survived)
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = length)
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {
    sum(x)/length(x)
})
train$Fare2 <- "30+"
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- "20-30"
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- "10-20"
train$Fare2[train$Fare < 10] <- "<10"
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) {
    sum(x)/length(x)
})
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
plot(fit)
text(fit)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
test$Survived <- rep(0, 418)
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Sex == "female" & test$Pclass == 3 & test$Fare >= 20] <- 0
head(test, 10)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
