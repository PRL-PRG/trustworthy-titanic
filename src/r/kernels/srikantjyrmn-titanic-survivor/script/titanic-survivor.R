rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(ggplot2)
library(caret)
library(stringr)
library(mice)
library(randomForest)
library(caretEnsemble)
library("ggthemes")
library("scales")
train_data <- read.csv("../input/train.csv", stringsAsFactors = F)
test_data <- read.csv("../input/test.csv", stringsAsFactors = F)
full_data <- bind_rows(train_data, test_data)
str(full_data)
full_data$Title <- (gsub("(.*, )|(\\..*)", "", full_data$Name))
table(full_data$Sex, full_data$Title)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full_data$Title[full_data$Title == "Mlle"] <- "Miss"
full_data$Title[full_data$Title == "Ms"] <- "Miss"
full_data$Title[full_data$Title == "Mme"] <- "Mrs"
full_data$Title[full_data$Title %in% rare_title] <- "Rare Title"
table(full_data$Sex, full_data$Title)
full_data$Title <- as.factor(full_data$Title)
full_data$Surname <- sapply(full_data$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
full_data$Surname <- as.factor(full_data$Surname)
str(full_data)
full_data$family_size <- full_data$SibSp + full_data$Parch + 1
full_data$Family <- as.factor(paste(full_data$Surname, full_data$family_size, sep = "_"))
ggplot(full_data[1:891, ], aes(x = family_size, fill = factor(Survived))) + geom_bar(position = "fill") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
full_data$family_size_group[full_data$family_size == 1] <- "singleton"
full_data$family_size_group[full_data$family_size < 5 & full_data$family_size > 1] <- "small"
full_data$family_size_group[full_data$family_size > 4] <- "large"
table(full_data$family_size_group)
full_data$family_size_group <- as.factor(full_data$family_size_group)
levels(full_data$family_size_group) = c("single", "medium", "large")
full_data %>% filter(!is.na(Survived)) %>% ggplot(aes(x = family_size, fill = factor(Survived))) + geom_bar(position = "fill") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
mosaicplot(table(full_data$family_size_group, full_data$Survived), main = "Family Size by Survival", shade = TRUE)
full_data$Cabin_letter <- as.factor(str_extract(full_data$Cabin, boundary("character")))
full_data$Cabin_letter_length <- str_length(full_data$Cabin)
summary(full_data)
full_data %>% filter(Pclass == 3) %>% ggplot(aes(x = Fare)) + geom_density()
full_data$Fare[is.na(full_data$Fare)] <- median(full_data$Fare[full_data$Pclass == 3 & full_data$Embarked == "S"], na.rm = TRUE)
embark_fare <- full_data %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
full_data$Embarked[c(62, 830)] <- "C"
full_data$Sex <- as.factor(full_data$Sex)
full_data$Embarked <- as.factor(full_data$Embarked)
full_data$PassengerId <- as.factor(full_data$PassengerId)
full_data$Pclass <- as.factor(full_data$Pclass)
set.seed(129)
mice_mod <- mice(full_data[, !names(full_data) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived", "Cabin_letter_length")], method = "rf")
mice_output <- complete(mice_mod)
full_data$Age <- mice_output$Age
par(mfrow = c(1, 1))
hist(mice_output$Age)
hist(full_data$Age)
full_data$Child[full_data$Age < 18] <- "Child"
full_data$Child[full_data$Age >= 18] <- "Adult"
table(full_data$Child, full_data$Survived)
full_data$Mother <- "Not Mother"
full_data$Mother[full_data$Sex == "female" & full_data$Parch > 0 & full_data$Age > 18 & full_data$Title != "Miss"] <- "Mother"
table(full_data$Mother, full_data$Survived)
full_data$Child <- as.factor(full_data$Child)
full_data$Mother <- as.factor(full_data$Mother)
full_data$Fare_bin <- (full_data$Fare - min(full_data$Fare))/(max(full_data$Fare) - min(full_data$Fare))
full_data$num_people_with_ticket <- sapply(full_data$Ticket, function(x) {
    sum(full_data$Ticket == x)
})
full_data$party_size[full_data$num_people_with_ticket == 1] <- "singleton"
full_data$party_size[full_data$num_people_with_ticket %in% 2:4] <- "two_to_four"
full_data$party_size[full_data$num_people_with_ticket %in% 5:8] <- "five_to_eight"
full_data$party_size[full_data$num_people_with_ticket > 8] <- "gt_eight"
full_data$party_size <- as.factor(full_data$party_size)
surviving_tickets <- unique(full_data$Ticket[full_data$Survived == 1 & !is.na(full_data$Survived) & full_data$party_size != "singleton"])
full_data$surviving_ticket <- sapply(full_data$Ticket, function(x) {
    x %in% surviving_tickets
})
full_data %>% filter(!is.na(Survived)) %>% ggplot(aes(x = surviving_ticket, fill = factor(Survived))) + geom_bar(position = "fill")
train <- full_data[1:891, ]
test <- full_data[892:1309, ]
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare_bin + party_size + surviving_ticket + Embarked + Title + family_size_group + Child + Mother, data = train)
sapply(full_data, class)
plot(rf_model, ylim = c(0, 0.36))
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()
p <- predict(rf_model, test)
results <- data.frame(PassengerId = test_data$PassengerId, Survived = p)
write.csv(results, file = "submit_results.csv", row.names = FALSE)
