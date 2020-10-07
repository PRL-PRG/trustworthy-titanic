suppressMessages(library("ggplot2"))
suppressMessages(library("ggthemes"))
suppressMessages(library("scales"))
suppressMessages(library("dplyr"))
suppressMessages(library("mice"))
suppressMessages(library("randomForest"))
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)
str(full)
full[full == ""] <- NA
sapply(full, function(x) sum(is.na(x)))
full$Title <- gsub(".*, ([^.]*)\\..*", "\\1", full$Name)
table(full$Title, full$Survived)
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
officer <- c("Capt", "Col", "Don", "Dr", "Major", "Rev")
royalty <- c("Dona", "Lady", "the Countess", "Sir", "Jonkheer")
full$Title[full$Title %in% officer] <- "officer"
full$Title[full$Title %in% royalty] <- "royalty"
table(full$Sex, full$Title)
full$Surname <- gsub("([^,]*),.*", "\\1", full$Name)
str(factor(full$Surname))
full$Deck <- sapply(as.character(full$Cabin), function(x) strsplit(x, NULL)[[1]][1])
full <- within(full, Deck <- ifelse(is.na(Deck), "U", Deck))
full$Deck[full$Deck == "A" | full$Deck == "B"] <- "upper_deck"
full$Deck[full$Deck == "C" | full$Deck == "D"] <- "middle_deck"
full$Deck[full$Deck == "E" | full$Deck == "F" | full$Deck == "G" | full$Deck == "T"] <- "lower_deck"
full$Deck <- factor(full$Deck)
print(summary(full$Deck))
full$Fsize <- full$SibSp + full$Parch + 1
cabins <- full$Cabin
n_occur <- data.frame(table(Var1 = cabins))
n_occur <- subset(n_occur, nchar(as.character(Var1)) > 1)
sharedCabins <- n_occur$Var1[n_occur$Freq > 1]
sharedInd <- full$Fsize == 1 & full$Cabin %in% sharedCabins
full$Fsize[sharedInd] <- 2
full$Family <- paste(full$Surname, full$FsizeAdj, sep = "_")
ggplot(full[1:891, ], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
full$FsizeD[full$Fsize == 1] <- "singleton"
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- "small"
full$FsizeD[full$Fsize > 4] <- "large"
mosaicplot(table(full$FsizeD, full$Survived), main = "Family Size by Survival", shade = TRUE)
str(factor(full$Embarked))
full$PassengerId[full$Embarked == ""]
full$Pclass[full$Embarked == ""]
ggplot(full, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
full$Embarked[c(62, 830)] <- "C"
str(factor(full$Fare))
full$PassengerId[is.na(full$Fare) == TRUE]
ggplot(full[full$Pclass == "3" & full$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", lwd = 1) + scale_x_continuous(labels = dollar_format()) + theme_few()
full$Fare[1044] <- median(full[full$Pclass == "3" & full$Embarked == "S", ]$Fare, na.rm = TRUE)
ggplot(full[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Title) + theme_few()
sum(is.na(full$Age))
sum(is.na(full$Age[full$Title == "Master"]))
summary(full$Age[full$Title == "Master"])
full$Age[is.na(full$Age) & full$Title == "Master"] <- 4
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Deck", "Title", "Surname", "Family", "FsizeD")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived", "Deck")], method = "rf")
mice_output <- complete(mice_mod)
par(mfrow = c(1, 2))
hist(full$Age, freq = F, main = "Age: Original Data", col = "darkgreen", ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = "Age: MICE Output", col = "lightgreen", ylim = c(0, 0.04))
full$Age <- mice_output$Age
sum(is.na(full$Age))
ggplot(full[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Title) + theme_few()
ggplot(full[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Sex) + theme_few()
full$Child[full$Age < 18] <- "Child"
full$Child[full$Age >= 18] <- "Adult"
table(full$Child, full$Survived)
full$Mother <- "Not Mother"
full$Mother[full$Sex == "female" & full$Parch > 0 & full$Age > 18 & full$Title != "Miss"] <- "Mother"
table(full$Mother, full$Survived)
full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)
md.pattern(full)
train <- full[1:891, ]
test <- full[892:1309, ]
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Fare + Embarked + Title + FsizeD + Child, data = train)
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()
rf_prediction <- predict(rf_model, test)
rf_solution <- data.frame(PassengerID = test$PassengerId, Survived = rf_prediction)
write.csv(rf_solution, file = "Titanic_rf_Solution.csv", row.names = F)
