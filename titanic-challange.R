library("ggplot2")
library("caret")
library("dplyr")
library("rpart")
library("rpart.plot")
library("car")
library("e1071")
library("randomForest")
library("ggthemes")
train.tit <- read.csv("../input/train.csv", stringsAsFactors = F)
test.tit <- read.csv("../input/test.csv", stringsAsFactors = F)
test.tit$Survived <- rep(0, 418)
str(test.tit)
combo <- rbind(train.tit, test.tit)
str(combo)
colSums(is.na(combo))
colSums(combo == "")
combo$Embarked[combo$Embarked == ""] = "S"
table(combo$Embarked)
head(combo$Name)
names <- combo$Name
title <- gsub("^.*, (.*?)\\..*$", "\\1", names)
combo$title <- title
table(title)
combo$Name <- as.character(combo$Name)
combo$Name[1]
rare_title <- c("Dona", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
combo$title[combo$title == "Mlle"] <- "Miss"
combo$title[combo$title == "Ms"] <- "Miss"
combo$title[combo$title == "Mme"] <- "Mrs"
combo$title[combo$title == "Lady"] <- "Miss"
combo$title[combo$title == "Dona"] <- "Miss"
combo$title[combo$title %in% rare_title] <- "Rare Title"
ggplot(combo[1:891, ], aes(x = title, fill = factor(Survived))) + geom_bar() + ggtitle("Title V/S Survival rate") + xlab("Title") + ylab("Total Count") + labs(fill = "Survived")
str(combo)
combo$FamilySize <- combo$SibSp + combo$Parch + 1
combo$FamilySized[combo$FamilySize == 1] <- "Single"
combo$FamilySized[combo$FamilySize < 5 & combo$FamilySize >= 2] <- "Small"
combo$FamilySized[combo$FamilySize >= 5] <- "Big"
combo$FamilySized = as.factor(combo$FamilySized)
ggplot(combo[1:891, ], aes(x = FamilySized, fill = factor(Survived))) + geom_bar() + ggtitle("Family Size V/S Survival Rate") + xlab("FamilySize") + ylab("Total Count") + labs(fill = "Survived")
str(combo)
ggplot(combo[1:891, ], aes(x = Pclass, fill = factor(Survived))) + geom_bar() + ggtitle("Pclass v/s Survival Rate") + xlab("Pclass") + ylab("Total Count") + labs(fill = "Survived")
ggplot(combo[1:891, ], aes(x = Sex, fill = factor(Survived))) + geom_bar() + ggtitle("Sex v/s Survival Rate") + xlab("Sex") + ylab("Total Count") + labs(fill = "Survived")
str(combo)
colSums(is.na(combo))
colSums(combo == "")
combo$Embarked = as.factor(combo$Embarked)
combo$Pclass = as.factor(combo$Pclass)
combo$title = as.factor(combo$title)
combo$Sex = as.factor(combo$Sex)
combo$Embarked = as.factor(combo$Embarked)
train <- combo[1:891, c("Pclass", "title", "Sex", "Embarked", "FamilySized", "Survived")]
test <- combo[892:1309, c("Pclass", "title", "Sex", "Embarked", "FamilySized", "Survived")]
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Pclass + title + Sex, data = train)
plot(rf_model, ylim = c(0, 0.5))
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3)
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test.tit$PassengerId, Survived = prediction)
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()
mosaicplot(table(train$Pclass, train$Sex, train$Embarked), main = "Population by Sex", shade = TRUE)
