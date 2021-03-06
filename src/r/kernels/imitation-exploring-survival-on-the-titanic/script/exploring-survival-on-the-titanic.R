library(tidyverse)
library(ggthemes)
library(stringr)
library(scales)
library(mice)
library(randomForest)
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
glimpse(train)
glimpse(test)
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[, ])
head(test.survived)
ntest.survived <- test.survived[, c(2, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)]
head(ntest.survived)
names(train)
names(ntest.survived)
data.combined <- rbind(train, ntest.survived)
glimpse(data.combined)
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)
glimpse(data.combined)
table(data.combined$Survived)
table(data.combined$Pclass)
g2.3 <- ggplot(data.combined, aes(x = factor(Pclass), fill = factor(Sex))) + geom_bar(position = "fill")
g2.3
g1 <- ggplot(test, aes(x = (Pclass), fill = (Sex))) + geom_bar(width = 0.5)
g1
g2 <- ggplot(train, aes(x = factor(Pclass), fill = factor(Sex))) + geom_bar(position = "dodge") + facet_grid(. ~ Survived)
g2
posn.j <- position_jitter(0.5, 0)
g3 <- ggplot(train, aes(x = factor(Pclass), y = Age, col = factor(Sex))) + geom_jitter(position = posn.j, alpha = 0.5, size = 3) + facet_grid(. ~ Survived)
g3
length(unique(data.combined$Name))
str(data.combined)
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])
data.combined[which(data.combined$Name %in% dup.names), ]
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")), ]
head(misses)
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")), ]
mrses[1:5, ]
masters <- data.combined[which(str_detect(data.combined$Name, "Master")), ]
masters[1:5, ]
mr <- data.combined[which(str_detect(data.combined$Name, "Mr.")), ]
mr[1:5, ]
extractTitle <- function(Name) {
    Name <- as.character(Name)
    if (length(grep("Miss.", Name)) > 0) {
        return("Miss.")
    }
    else if (length(grep("Master.", Name)) > 0) {
        return("Master.")
    }
    else if (length(grep("Mrs.", Name)) > 0) {
        return("Mrs.")
    }
    else if (length(grep("Mr.", Name)) > 0) {
        return("Mr.")
    }
    else {
        return("Other")
    }
}
titles <- NULL
for (i in 1:nrow(data.combined)) {
    titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$Title <- as.factor(titles)
p2.1 <- ggplot(data.combined[1:891, ], aes(x = Title, fill = Survived)) + geom_bar(width = 0.5) + facet_wrap(~Pclass) + ggtitle("Pclass") + xlab("Title") + ylab("Total Count") + labs(fill = "Survived")
p2.1
table(data.combined$Sex)
p2.2 <- ggplot(data.combined[1:891, ], aes(x = Sex, fill = Survived)) + geom_bar(width = 0.5) + facet_wrap(~Pclass) + ggtitle("Pclass") + xlab("Title") + ylab("Total Count") + labs(fill = "Survived")
p2.2
summary(data.combined$Age)
g2.3 <- ggplot(data.combined[1:891, ], aes(x = Age, fill = Survived)) + geom_bar(width = 0.5) + facet_wrap(~Sex + Pclass) + geom_bar(binwidth = 10) + xlab("Age") + ylab("Total Count")
g2.3
boys <- data.combined[which(data.combined$Title == "Master."), ]
summary(boys$Age)
misses <- data.combined[which(data.combined$Title == "Miss."), ]
summary(misses$Age)
g2.4 <- ggplot(misses[misses$Survived != "None", ], aes(x = Age, fill = Survived)) + facet_wrap(~Pclass) + geom_bar(binwidth = 5) + ggtitle("Age for Miss by Pclass") + xlab("Age") + ylab("Total Count")
g2.4
misses.alone <- misses[which(misses$SibSp == 0, misses$Parch == 0), ]
summary(misses.alone$Age)
summary(data.combined$SibSp)
data.combined$SibSp <- as.factor(data.combined$SibSp)
g2.5 <- ggplot(data.combined[1:891, ], aes(x = SibSp, fill = Survived)) + geom_bar(width = 0.5) + facet_wrap(~Pclass + Title) + ggtitle("Pclass,Title") + xlab("Sibsp") + ylab("Total Count") + ylim(0, 300) + labs(fill = "Survived")
g2.5
data.combined$Parch <- as.factor(data.combined$Parch)
g2.6 <- ggplot(data.combined[1:891, ], aes(x = Parch, fill = Survived)) + geom_bar(width = 1) + facet_wrap(~Pclass + Title) + ggtitle("Pclass,Title") + xlab("Parch") + ylab("Total Count") + ylim(0, 300) + labs(fill = "Survived")
g2.6
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$Family.Size <- as.factor(temp.sibsp + temp.parch + 1)
g2.7 <- ggplot(data.combined[1:891, ], aes(x = Family.Size, fill = Survived)) + geom_bar(width = 1) + facet_wrap(~Pclass + Title) + ggtitle("Pclass,Title") + xlab("Family.Size") + ylab("Total Count") + ylim(0, 300) + labs(fill = "Survived")
g2.7
subsmr <- subset(data.combined[1:891, ], is.na(Age) & Title == "Mr.")
summary(subsmr)
g3.1 <- ggplot(subsmr, aes(x = Pclass, fill = Survived)) + geom_bar(width = 0.5) + facet_wrap(~Family.Size)
g3.1
summary(misses.alone$Age)
data.combined2 <- data.combined
data.combined2$Embarked[c(62, 830)]
data.combined2$Embarked[c(62, 830)] <- "C"
data.combined2[1044, ]
data.combined2$Fare[1044] <- median(data.combined2[data.combined2$Pclass == 3 & data.combined2$Embarked == "S", ]$Fare, na.rm = T)
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Family.Size")
data.combined2[factor_vars] <- lapply(data.combined2[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(data.combined2[, !names(data.combined2) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
par(mfrow = c(1, 2))
hist(data.combined2$Age, freq = F, main = "Age: Original Data", col = "darkgreen", ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = "Age: MICE Output", col = "lightgreen", ylim = c(0, 0.04))
data.combined2$Age <- mice_output$Age
data.combined2$Deck <- mice_output$Deck
summary(data.combined2)
train <- data.combined2[1:891, ]
test <- data.combined2[892:1309, ]
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, ntree = 100, importance = T, data = train)
plot(rf_model, ylim = c(0, 0.36))
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3)
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
