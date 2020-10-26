library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("rpart")
library("randomForest")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
str(train)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
test$Survived <- NA
train_test <- rbind(train, test)
sapply(train_test, function(x) sum(is.na(x)))
sapply(train_test, function(x) sum(x == ""))
faremiss <- which(is.na(train_test$Fare))
train_test[faremiss, ]
Fare1 <- ggplot(train_test[train_test$Pclass == "3" & train_test$Embarked == "S" & train_test$Age >= 50, ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", led = 1) + ggtitle("Fare1:Age considered") + scale_x_continuous(labels = dollar_format()) + theme_few()
Fare2 <- ggplot(train_test[train_test$Pclass == "3" & train_test$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", led = 1) + ggtitle("Fare2:Regardless of age") + scale_x_continuous(labels = dollar_format()) + theme_few()
library(gridExtra)
grid.arrange(Fare1, Fare2, ncol = 2, nrow = 1)
Fare1 <- median(train_test[train_test$Pclass == "3" & train_test$Embarked == "S" & train_test$Age >= 50, ]$Fare, na.rm = TRUE)
Fare2 <- median(train_test[train_test$Pclass == "3" & train_test$Embarked == "S", ]$Fare, na.rm = TRUE)
Fare1
Fare2
train_test$Fare[faremiss] <- 8
ggplot(train_test[1:891, ], aes(x = Fare, color = factor(Survived))) + geom_line(stat = "count", position = "dodge") + theme_few()
embarkedmiss <- which(train_test$Embarked == "")
train_test[embarkedmiss, ]
library("dplyr")
embark_fare <- train_test %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
train_test$Embarked[c(62, 830)] <- "C"
train_test$Embarked <- factor(train_test$Embarked)
train_test$Fsize <- train_test$SibSp + train_test$Parch + 1
ggplot(train_test[1:891, ], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
train_test$FsizeD[train_test$Fsize == 1] <- "singleton"
train_test$FsizeD[train_test$Fsize < 5 & train_test$Fsize > 1] <- "small"
train_test$FsizeD[train_test$Fsize > 4] <- "large"
train_test$FsizeD <- factor(train_test$FsizeD)
mosaicplot(table(train_test[1:891, ]$FsizeD, train_test[1:891, ]$Survived), main = "Family Size by Survival", shade = TRUE)
train_test$Ptitle <- gsub("(.*, )|(\\..*)", "", train_test$Name)
table(train_test$Sex, train_test$Ptitle)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
train_test$Ptitle[train_test$Ptitle == "Mlle"] <- "Miss"
train_test$Ptitle[train_test$Ptitle == "Ms"] <- "Miss"
train_test$Ptitle[train_test$Ptitle == "Mme"] <- "Mrs"
train_test$Ptitle[train_test$Ptitle %in% rare_title] <- "Rare Title"
train_test$Ptitle <- factor(train_test$Ptitle)
table(train_test$Sex, train_test$Ptitle)
age_model <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Ptitle + FsizeD, data = train_test[!is.na(train_test$Age), ], method = "anova")
train_test$Age[is.na(train_test$Age)] <- predict(age_model, train_test[is.na(train_test$Age), ])
ggplot(train_test[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Sex) + theme_few()
train_test$Age_group[train_test$Age <= 12] <- "Child"
train_test$Age_group[train_test$Age > 12 & train_test$Age < 18] <- "youth"
train_test$Age_group[train_test$Age >= 18] <- "Adult"
train_test$Age_group <- factor(train_test$Age_group)
mosaicplot(table(train_test$Age_group, train_test$Survived), main = "Comparison of child and adult", color = c("pink", "lightblue"))
train_test$Mother <- "Not Mother"
train_test$Mother[train_test$Sex == "female" & train_test$Parch > 0 & train_test$Age > 18 & train_test$Ptitle != "Miss"] <- "Mother"
mosaicplot(table(train_test$Mother, train_test$Survived), main = "Comparison of mother and non mother", color = c("pink", "lightblue"))
train_test$Mother <- factor(train_test$Mother)
train_test$Sex <- factor(train_test$Sex)
train <- train_test[1:891, ]
test <- train_test[892:1309, ]
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Sex + Ptitle + Pclass + Embarked + Age_group + Mother + Fare + FsizeD, data = train)
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "gender_submission.csv", row.names = F)
