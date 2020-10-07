library(tidyverse)
list.files(path = "../input")
library(tidyverse)
library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("mice")
library("randomForest")
list.files(path = "../input")
getwd()
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)
str(full)
var_summ <- function(x) {
    if (class(x) == "numeric") {
        var_type = class(x)
        n <- length(x)
        n_miss <- sum(is.na(x))
        mean <- mean(x, na.rm = T)
        std <- sd(x, na.rm = T)
        min <- min(x, na.rm = T)
        max <- max(x, na.rm = T)
        return(c(var_type = var_type, n = n, n_miss = n_miss, mean = mean, std = std, min = min, max = max))
    }
    else {
        var_type = class(x)
        n = length(x)
        n_miss = sum(is.na(x))
        return(c(n = n, n_miss = n_miss))
    }
}
num_var <- sapply(full, is.numeric)
num_data <- t(data.frame(apply(full[num_var], 2, var_summ)))
num_data
cat_data <- data.frame(apply(full[!num_var], 2, var_summ))
cat_data
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
table(full$Sex, full$Title)
rare_title <- c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady", "Major", "Mlle", "Mme", "Ms", "Rev", "Sir", "the Countess")
full$Title[full$Title %in% rare_title] <- "rare_title"
table(full$Sex, full$Title)
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
full$Fsize <- full$Parch + full$SibSp + 1
full$Family <- paste(full$Surname, full$Fsize, sep = "_")
ggplot(full[1:891, ], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
full$FsizeD[full$Fsize == 1] <- "Singleton"
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- "Small"
full$FsizeD[full$Fsize > 4] <- "Large"
table(full$Survive, full$FsizeD)
full$Cabin[1:28]
strsplit(full$Cabin[2], NULL)[[1]][1]
full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
table(full$Deck)
unique(full$Embarked)
table(full$Embarked)
full[c(62, 830), ]
embarked_value <- full %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embarked_value, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
full$Embarked[c(62, 830)] <- "C"
full[is.na(full$Fare), ]
full[1044, ]
ggplot(full[full$Pclass == 3 & full$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", lwd = 1) + scale_x_continuous(labels = dollar_format()) + theme_few()
full$Fare[1044] <- median(full[full$Pclass == 3 & full$Embarked == "S", ]$Fare, na.rm = T)
sum(is.na(full$Age))
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname", "Family", "FsizeD")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
par(mfrow = c(1, 2))
hist(full$Age, freq = F, main = "Age: Original Data", col = "darkgreen", ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = "Age: MICE Output", col = "lightgreen", ylim = c(0, 0.04))
full$Age <- mice_output$Age
sum(is.na(full$Age))
full$Child[full$Age < 18] <- "Child"
full$Child[full$Age >= 18] <- "Adult"
table(full$Child, full$Survived)
full$Mother <- "Not Mother"
full$Mother[full$Sex == "female" & full$Parch > 0 & full$Age > 18 & full$Title != "Miss"] <- "Mother"
table(full$Mother, full$Survived)
full$Child <- factor(full$Child)
full$Mother <- factor(full$Mother)
train <- full[1:891, ]
test <- full[892:1309, ]
set.seed(101)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child + Mother, data = train)
rf_model
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
table(solution$Survived)
write.csv(solution, "rf_model_solution.csv", row.names = F)
