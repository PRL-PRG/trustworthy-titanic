library("dplyr")
library("mice")
library("ggplot2")
library("ggthemes")
library("Hmisc")
library("caret")
library("randomForest")
library("e1071")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
Titanic <- bind_rows(train, test)
summary(Titanic)
str(Titanic)
colSums(Titanic == "")
colSums(is.na(Titanic))
ggplot(Titanic[1:891, ], aes(x = Pclass, fill = factor(Survived))) + geom_bar() + ggtitle("Pclass versus Survival Rate") + xlab("Pclass") + ylab("Count") + labs(fill = "Survived")
Titanic$Title <- gsub("(.*, )|(\\..*)", "", Titanic$Name)
table(Titanic$Title)
table(Titanic$Sex, Titanic$Title)
Titanic$Title[Titanic$Title == "Ms"] <- "Miss"
Titanic$Title[Titanic$Title == "Lady"] <- "Miss"
Titanic$Title[Titanic$Title == "Mme"] <- "Mrs"
Titanic$Title[Titanic$Title == "Mlle"] <- "Miss"
rare_title <- c("Dona", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
Titanic$Title[Titanic$Title %in% rare_title] <- "Officer"
table(Titanic$Sex, Titanic$Title)
ggplot(Titanic[1:891, ], aes(x = Title, fill = factor(Survived))) + geom_bar() + ggtitle("Title Versus Survival rate") + xlab("Title") + ylab("Count") + labs(fill = "Survived")
sum(is.na(Titanic$Age))
variables <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title")
Titanic[variables] <- lapply(Titanic[variables], function(x) as.factor(x))
mice_mod <- mice(Titanic[, !names(Titanic) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Sibsp", "Parch", "Survived", "Fare")], method = "rf")
mice_output <- complete(mice_mod)
hist(Titanic$Age, freq = F, main = "Original Age ", col = "darkgreen", ylim = c(0, 0.05))
hist(mice_output$Age, freq = F, main = "MICE Output Age", col = "Red", ylim = c(0, 0.05))
Titanic$Age <- mice_output$Age
sum(is.na(Titanic$Age))
ggplot(Titanic[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Sex) + theme_few()
Titanic$FamilySize <- Titanic$SibSp + Titanic$Parch + 1
Titanic$FamilySized[Titanic$FamilySize == 1] <- "Single"
Titanic$FamilySized[Titanic$FamilySize < 5 & Titanic$FamilySize >= 2] <- "Small"
Titanic$FamilySized[Titanic$FamilySize >= 5] <- "Big"
Titanic$FamilySized = as.factor(Titanic$FamilySized)
ggplot(Titanic[1:891, ], aes(x = FamilySized, fill = factor(Survived))) + geom_bar() + ggtitle("Family Size V/S Survival Rate") + xlab("FamilySize") + ylab("Total Count") + labs(fill = "Survived")
table(Titanic$Embarked)
Titanic$Embarked[Titanic$Embarked == ""] = "S"
table(Titanic$Embarked)
ggplot(Titanic[1:891, ], aes(x = Embarked, fill = factor(Survived))) + geom_bar() + ggtitle("Embarked vs Survival") + xlab("Embarked") + ylab("Total Count") + labs(fill = "Survived")
Train <- Titanic[1:600, ]
Train_1 <- Titanic[601:891, ]
Test <- Titanic[892:1309, ]
set.seed(754)
Model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Title + FamilySized, data = Train)
importance <- importance(Model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
prediction <- predict(Model, Test)
solution <- data.frame(PassengerID = Test$PassengerId, Survived = prediction)
write.csv(solution, file = "Solution_Test.csv", row.names = F)
