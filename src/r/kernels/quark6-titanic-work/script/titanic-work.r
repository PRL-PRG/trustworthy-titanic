library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("mice")
library("randomForest")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
combined <- bind_rows(train, test)
str(combined)
combined[combined == ""] <- NA
sum(is.na(combined$Sex))
ggplot(combined[1:891, ], aes(x = Sex, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_discrete(breaks = c("female", "male")) + labs(x = "Sex") + theme_few()
sum(is.na(combined$Embarked))
filter(combined, is.na(Embarked))
embark_fare <- combined %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
combined$Embarked[c(62, 830)] <- "C"
ggplot(combined[1:891, ], aes(x = Embarked, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_discrete(breaks = c("C", "Q", "S")) + labs(x = "Embarked") + theme_few()
sum(is.na(combined$Pclass))
ggplot(combined[1:891, ], aes(x = Pclass, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:3)) + labs(x = "Passenger Class") + theme_few()
combined$FamSize <- combined$SibSp + combined$Parch + 1
ggplot(combined[1:891, ], aes(x = FamSize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
sum(is.na(combined$Fare))
filter(combined, is.na(Fare))
ggplot(combined[combined$Pclass == "3" & combined$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", lwd = 1) + scale_x_continuous(labels = dollar_format()) + theme_few()
median(combined[combined$Pclass == "3" & combined$Embarked == "S", ]$Fare, na.rm = TRUE)
combined$Fare[1044] <- median(combined[combined$Pclass == "3" & combined$Embarked == "S", ]$Fare, na.rm = TRUE)
ggplot(combined[1:891, ], aes(Fare, fill = factor(Survived))) + geom_histogram(binwidth = 6) + facet_grid(. ~ Sex) + theme_few()
sum(is.na(combined$Age))
combined$Title <- gsub("(.*, )|(\\..*)", "", combined$Name)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
combined$Title[combined$Title == "Mlle"] <- "Miss"
combined$Title[combined$Title == "Ms"] <- "Miss"
combined$Title[combined$Title == "Mme"] <- "Mrs"
combined$Title[combined$Title %in% rare_title] <- "Rare_Title"
table(combined$Sex, combined$Title)
combined$Surname <- sapply(combined$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
combined$Fam <- paste(combined$Surname, combined$FamSize, sep = "_")
combined$Fam_SizeD[combined$FamSize == 1] <- "single"
combined$Fam_SizeD[combined$FamSize < 5 & combined$FamSize > 1] <- "small"
combined$Fam_SizeD[combined$FamSize > 4] <- "large"
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname", "Fam", "Fam_SizeD")
combined[factor_vars] <- lapply(combined[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(combined[, !names(combined) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Fam", "Surname", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
par(mfrow = c(1, 2))
hist(combined$Age, freq = F, main = "Age: Original Data", col = "darkgreen", ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = "Age: MICE Output", col = "lightgreen", ylim = c(0, 0.04))
combined$Age <- mice_output$Age
sum(is.na(combined$Age))
ggplot(combined[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram(binwidth = 2) + facet_grid(. ~ Sex) + theme_few()
train <- combined[1:891, ]
test <- combined[892:1309, ]
combined$Status[combined$Age < 18] <- "Child"
combined$Status[combined$Age >= 18] <- "Adult"
table(combined$Status, combined$Survived)
combined$Mother <- "Not Mother"
combined$Mother[combined$Sex == "female" & combined$Parch > 0 & combined$Age > 18 & combined$Title != "Miss"] <- "Mother"
table(combined$Mother, combined$Survived)
combined$Status <- factor(combined$Status)
combined$Mother <- factor(combined$Mother)
md.pattern(combined)
set.seed(1000)
RanFor_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Fam_SizeD, data = train, ntree = 900, mtry = 6, nodesize = 0.01 * nrow(test))
plot(RanFor_model, ylim = c(0, 0.36))
legend("bottomleft", colnames(RanFor_model$err.rate), col = 1:3, fill = 1:3, bty = "n", lty = 1:3, cex = 0.8)
importance <- importance(RanFor_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()
prediction <- predict(RanFor_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "RanFor_Solution.csv", row.names = F)
