library(mice)
library(dplyr)
library(randomForest)
library(arulesViz)
library(ggplot2)
library(ggthemes)
library(scales)
train <- read.csv("../input/train.csv", stringsAsFactors = F)
head(train)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
head(test)
t <- bind_rows(train, test)
tail(t)
t[890, ]
t[980, ]
str(t)
t$Title <- gsub("(.*, )|(\\..*)", "", t$Name)
table(t$Sex, t$Title)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
t$Title[t$Title == "Mlle"] <- "Miss"
t$Title[t$Title == "Ms"] <- "Miss"
t$Title[t$Title == "Mme"] <- "Mrs"
t$Title[t$Title %in% rare_title] <- "Rare Title"
table(t$Sex, t$Title)
t$Surname <- sapply(t$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
cat(paste("We have ", nlevels(factor(t$Surname)), " unique Surnames onboard Titanic."))
t$Fsize <- t$SibSp + t$Parch + 1
t$Family <- paste(t$Surname, t$Fsize, sep = "_")
par(mar = c(2, 2, 2, 1), mfrow = c(1, 1))
ggplot(t, aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
t$FsizeD[t$Fsize == 1] <- "singleton"
t$FsizeD[t$Fsize < 5 & t$Fsize > 1] <- "small"
t$FsizeD[t$Fsize > 4] <- "large"
mosaicplot(table(t$FsizeD, t$Survived), main = "Family Size by Survival", shade = TRUE)
t$Cabin[1:50]
strsplit(t$Cabin[2], NULL)[[1]]
t$Deck <- factor(sapply(t$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
t[t$Embarked == "", ]
t[t$Ticket == 113572, ]
cat(paste("We will infer their values for **embarkment** based on present data that we can imagine may be relevant: **passenger class** and **fare**. We see that they paid $", t[62, "Fare"][[1]][1], "and $", t[830, "Fare"][[1]][1], "respectively and their classes are", t[62, "Pclass"][[1]][1], "and", t[830, "Pclass"][[1]][1], ". So from where did they embark?"))
embark_fare <- t %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
t$Embarked[c(62, 830)] <- "C"
t[t$Ticket == 113572, ]
t[1044, ]
ggplot(t[t$Pclass == "3" & t$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", lwd = 1) + scale_x_continuous() + theme_few()
t$Fare[1044] <- median(t[t$Pclass == "3" & t$Embarked == "S", ]$Fare, na.rm = TRUE)
sum(is.na(t$Age))
fac_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname", "Family", "FsizeD")
t[fac_vars] <- lapply(t[fac_vars], function(x) as.factor(x))
set.seed(11)
mice_mod <- mice(t[, !names(t) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
par(mfrow = c(1, 2))
hist(t$Age, freq = F, main = "Age: Original Data", col = "blue", ylim = c(0, 0.04), border = "white")
hist(mice_output$Age, freq = F, main = "Age: MICE Output", col = "dodgerblue", ylim = c(0, 0.04), border = "white")
t$Age <- mice_output$Age
sum(is.na(t$Age))
ggplot(t[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Sex) + theme_few()
t$AgeAC[t$Age < 18] <- "Child"
t$AgeAC[t$Age >= 18] <- "Adult"
table(t$AgeAC, t$Survived)
t$Mother <- "Not Mother"
t$Mother[t$Sex == "female" & t$Parch > 0 & t$Age > 18 & t$Title != "Miss"] <- "Mother"
table(t$Mother, t$Survived)
t$AgeAC <- factor(t$AgeAC)
t$Mother <- factor(t$Mother)
md.pattern(t)
train <- t[1:891, ]
test <- t[892:1309, ]
set.seed(1942)
rf_m <- randomForest(factor(Survived) ~ Pclass + Sex + Age + AgeAC + SibSp + Parch + Fare + Embarked + Title + FsizeD + Mother, data = train)
plot(rf_m, ylim = c(0, 0.36))
legend("topright", colnames(rf_m$err.rate), col = 1:3, fill = 1:3)
importance <- importance(rf_m)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()
prediction <- predict(rf_m, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "titanic-survival-arkitekt.csv", row.names = F)
