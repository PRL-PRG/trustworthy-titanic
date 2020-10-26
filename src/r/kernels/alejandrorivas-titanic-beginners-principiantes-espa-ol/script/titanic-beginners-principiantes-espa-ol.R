knitr::opts_chunk$set(echo = TRUE, message = F, warning = F, fig.width = 9.475, fig.height = 5)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
str(train)
table(train$Survived)
prop.table(table(train$Survived))
test$Survived <- rep(0, 418)
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
summary(train$Sex)
prop.table(table(train$Sex, train$Survived))
prop.table(table(train$Sex, train$Survived), 1)
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
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
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Sex == "female" & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
plot(fit)
text(fit)
library(rpart.plot)
library(RColorBrewer)
Prediction <- predict(fit, test, type = "class")
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train$Name[1]
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Name[1]
strsplit(combi$Name[1], split = "[,.]")
strsplit(combi$Name[1], split = "[,.]")[[1]]
strsplit(combi$Name[1], split = "[,.]")[[1]][2]
combi$Title <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][2]
})
combi$Title <- sub(" ", "", combi$Title)
table(combi$Title)
combi$Title[combi$Title %in% c("Mme", "Mlle")] <- "Mlle"
combi$Title[combi$Title %in% c("Capt", "Don", "Major", "Sir")] <- "Sir"
combi$Title[combi$Title %in% c("Dona", "Lady", "the Countess", "Jonkheer")] <- "Lady"
combi$Title <- factor(combi$Title)
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][1]
})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")
combi$FamilyID[combi$FamilySize <= 2] <- "Small"
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2, ]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- "Small"
combi$FamilyID <- factor(combi$FamilyID)
train <- combi[1:891, ]
test <- combi[892:1309, ]
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, method = "class")
sample(1:10, replace = TRUE)
summary(combi$Age)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = combi[!is.na(combi$Age), ], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age), ])
summary(combi)
summary(combi$Embarked)
which(combi$Embarked == "")
combi$Embarked[c(62, 830)] = "S"
combi$Embarked <- factor(combi$Embarked)
summary(combi$Fare)
which(is.na(combi$Fare))
combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)
combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- "Small"
combi$FamilyID2 <- factor(combi$FamilyID2)
train <- combi[1:891, ]
test <- combi[892:1309, ]
library(randomForest)
set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2, data = train, importance = TRUE, ntree = 2000)
varImpPlot(fit)
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest.csv", row.names = FALSE)
library(party)
set.seed(415)
fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, controls = cforest_unbiased(ntree = 2000, mtry = 3))
Prediction <- predict(fit, test, OOB = TRUE, type = "response")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "firstforest2.csv", row.names = FALSE)
