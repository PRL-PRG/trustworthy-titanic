library(ggplot2)
library(readr)
library(rpart)
library(party)
library(randomForest)
system("ls ../input")
train <- read.csv("../input/train.csv", header = TRUE, stringsAsFactors = F)
test <- read.csv("../input/test.csv", header = TRUE, stringsAsFactors = F)
test$Survived <- NA
combi <- rbind(train, test)
combi$Child <- 0
combi$Child[test$Age < 18] <- 1
combi$Fare2 <- "30+"
combi$Fare2[combi$Fare < 30 & combi$Fare >= 20] <- "20-30"
combi$Fare2[combi$Fare < 20 & combi$Fare >= 10] <- "10-20"
combi$Fare2[combi$Fare < 10] <- "<10"
combi$Title <- gsub("(.*, )|(\\..*)", "", combi$Name)
rare.title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
combi$Title[combi$Title == "Mlle"] <- "Miss"
combi$Title[combi$Title == "Ms"] <- "Miss"
combi$Title[combi$Title == "Mme"] <- "Miss"
combi$Title[combi$Title %in% rare.title] <- "Rare Title"
combi$Surname <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][1]
})
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$FamilyUnit <- paste(combi$Surname, as.character(combi$FamilySize), sep = "_")
combi$FamilySize2[combi$FamilySize == 1] <- "singleton"
combi$FamilySize2[combi$FamilySize < 5 & combi$FamilySize > 1] <- "small"
combi$FamilySize2[combi$FamilySize > 4] <- "large"
combi$Fare3[combi$Fare <= 5] <- "0-5"
combi$Fare3[combi$Fare > 5 & combi$Fare <= 11] <- "6-10"
combi$Fare3[combi$Fare > 11 & combi$Fare <= 15] <- "11-15"
combi$Fare3[combi$Fare > 15 & combi$Fare <= 20] <- "16-20"
combi$Fare3[combi$Fare > 20 & combi$Fare <= 25] <- "21-25"
combi$Fare3[combi$Fare > 25 & combi$Fare <= 30] <- "26-30"
combi$Fare3[combi$Fare > 30 & combi$Fare <= 40] <- "31-40"
combi$Fare3[combi$Fare > 40 & combi$Fare <= 60] <- "41-60"
combi$Fare3[combi$Fare > 60 & combi$Fare <= 100] <- "60-100"
combi$Fare3[combi$Fare > 100 & combi$Fare <= 200] <- "101-200"
combi$Fare3[combi$Fare > 200] <- "200+"
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Embarked + Title + FamilySize, data = combi[!is.na(combi$Age), ], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age), ])
combi$Embarked[c(62, 830)] <- "C"
combi$Fare[1044] <- median(combi[combi$Pclass == "3" & combi$Embarked == "S", ]$Fare, na.rm = TRUE)
combi$Fare2[1044] <- "<10"
combi$Name <- as.factor(combi$Name)
combi$Sex <- as.factor(combi$Sex)
combi$Embarked <- as.factor(combi$Embarked)
combi$Fare3 <- as.factor(combi$Fare2)
combi$Title <- as.factor(combi$Title)
combi$Surname <- as.factor(combi$Surname)
combi$FamilySize <- as.factor(combi$FamilySize)
combi$FamilySize2 <- as.factor(combi$FamilySize2)
train <- combi[1:891, ]
test <- combi[892:1309, ]
set.seed(187)
fit3 <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare3 + Embarked + Title + FamilySize2 + Child, data = train, importance = TRUE, ntree = 2000, mtry = 5)
Prediction <- predict(fit3, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "randoforest6.csv", row.names = FALSE)
