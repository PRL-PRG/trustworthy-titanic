list.files("../input")
library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("mice")
library("randomForest")
library("readr")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
titanic <- bind_rows(train, test)
str(titanic)
summary(titanic)
head(titanic)
colnames(titanic)
titanic$title <- gsub("(.*, )|(\\..*)", "", titanic$Name)
table(titanic$Sex, titanic$title)
unusual_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
titanic$title[titanic$title == "Mlle"] <- "Miss"
titanic$title[titanic$title == "Ms"] <- "Miss"
titanic$title[titanic$title == "Mme"] <- "Mrs"
titanic$title[titanic$title %in% unusual_title] <- "Unusual Title"
table(titanic$Sex, titanic$title)
titanic$surname <- sapply(titanic$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
nlevels(factor(titanic$surname))
titanic$famsize <- titanic$SibSp + titanic$Parch + 1
titanic$family <- paste(titanic$surname, titanic$famsize, sep = "_")
ggplot(titanic[1:891, ], aes(x = famsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
titanic$fsizeD[titanic$famsize == 1] <- "single"
titanic$fsizeD[titanic$famsize < 5 & titanic$famsize > 1] <- "small"
titanic$fsizeD[titanic$famsize > 4] <- "large"
titanic$Cabin[1:28]
strsplit(titanic$Cabin[2], NULL)[[1]]
titanic$deck <- factor(sapply(titanic$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
summary(titanic)
titanic$Embarked[titanic$Embarked == ""] <- NA
titanic[(which(is.na(titanic$Embarked))), 1]
titanic[c(62, 830), "Embarked"]
titanic[c(62, 830), c(1, 3, 10)]
titanic %>% group_by(Embarked, Pclass) %>% filter(Pclass == "1") %>% summarise(mfare = median(Fare), n = n())
embark_fare <- titanic %>% filter(PassengerId != 62 & PassengerId != 830)
embark_fare
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
titanic$Embarked[c(62, 830)] <- "C"
titanic[(which(is.na(titanic$Fare))), 1]
titanic[1044, c(3, 12)]
titanic[1044, ]
titanic %>% filter(Pclass == "3" & Embarked == "S") %>% summarise(missing_fare = median(Fare, na.rm = TRUE))
ggplot(titanic[titanic$Pclass == "3" & titanic$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", lwd = 1) + scale_x_continuous(labels = dollar_format()) + theme_few()
titanic$Fare[1044] <- 8.05
summary(titanic$Fare)
titanic$Fare[1044] <- median(titanic[titanic$Pclass == "3" & titanic$Embarked == "S", ]$Fare, na.rm = TRUE)
sum(is.na(titanic$Age))
set.seed(129)
mice_mod <- mice(titanic[, !names(titanic) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
par(mfrow = c(1, 2))
hist(titanic$Age, freq = F, main = "Age: Original Data", col = "darkred", ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = "Age: MICE Output", col = "lightgreen", ylim = c(0, 0.04))
titanic$Age <- mice_output$Age
sum(is.na(titanic$Age))
ggplot(titanic[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Sex) + theme_few()
titanic$Child[titanic$Age < 18] <- "Child"
titanic$Child[titanic$Age >= 18] <- "Adult"
table(titanic$Child, titanic$Survived)
titanic$Mother <- "Not Mother"
titanic$Mother[titanic$Sex == "female" & titanic$Parch > 0 & titanic$Age > 18 & titanic$title != "Miss"] <- "Mother"
table(titanic$Mother, titanic$Survived)
titanic$Child <- factor(titanic$Child)
titanic$Mother <- factor(titanic$Mother)
titanic$Pclass <- factor(titanic$Pclass)
titanic$Sex <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)
titanic$Survived <- factor(titanic$Survived)
titanic$title <- factor(titanic$title)
titanic$fsizeD <- factor(titanic$fsizeD)
train <- titanic[1:891, ]
test <- titanic[892:1309, ]
set.seed(754)
titanic_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + fsizeD + Child + Mother, data = train)
plot(titanic_model, ylim = c(0, 0.36))
legend("topright", colnames(titanic_model$err.rate), col = 1:3, fill = 1:3)
importance <- importance(titanic_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()
prediction <- predict(titanic_model, test)
prediction
Output <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
Output
write.csv(Output, file = "pradeep_titanic_output.csv", row.names = F)
