library(ggplot2)
library(dplyr)
library(stringr)
library(ggthemes)
library(randomForest)
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
test$Survived <- NA
test <- test[, c(1, 12, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]
head(test)
full <- rbind(train, test)
sapply(full, function(x) {
    sum(is.na(x) | x == "")
})
which(is.na(full$Fare))
full[1044, ]
mean(full$Fare[full$Pclass == 3 & full$Sex == "male"], na.rm = TRUE)
median(full$Fare[full$Pclass == 3 & full$Sex == "male"], na.rm = TRUE)
ggplot(full[full$Pclass == 3 & full$Sex == "male", ], aes(x = Fare, y = Pclass)) + geom_point()
full$Fare[is.na(full$Fare)] <- mean(full$Fare[full$Pclass == 3 & full$Sex == "male"], na.rm = TRUE)
sum(full$Cabin == "")/nrow(full)
which(full$Embarked == "")
full[c(62, 830), ]
full[full$Ticket == 113572, ]
full[full$Cabin == "B28", ]
ggplot(full[full$Pclass == 1, ], aes(x = Embarked)) + geom_bar() + facet_grid(. ~ Sex)
t <- full[full$Fare > 75 & full$Fare < 85, ]
t %>% group_by(Embarked) %>% summarise(n())
full$Embarked[full$Embarked == ""] <- "C"
sum(is.na(full$Age))
full$Title <- str_extract(string = full$Name, pattern = "(Mr|Master|Mrs|Miss)\\.")
table(full$Title)
length(which(is.na(full$Title)))
full$Title[is.na(full$Title)] <- "Rare"
m <- c("Mr.", "Mrs.", "Miss.", "Master.", "Rare")
for (i in 1:length(m)) {
    cat("NA in Age having title", m[i], nrow(full[is.na(full$Age) & full$Title == m[i], ]), "\n")
}
for (i in 1:length(m)) {
    full$Age[(is.na(full$Age) & full$Title == m[i])] <- round(mean(full$Age[full$Title == m[i]], na.rm = T), 2)
}
full$family_size <- full$SibSp + full$Parch + 1
ggplot(full[1:891, ], aes(x = family_size, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size")
table(full$Pclass, full$Survived)
ggplot(full[1:891, ], aes(x = Pclass, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + labs(x = "Pclass") + theme_few()
ggplot(full[1:891, ], aes(x = Sex, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + labs(x = "Gender") + theme_few()
ggplot(full[1:891, ], aes(x = Title, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + labs(x = "Title") + theme_few()
table(cut(full$Age, 10), full$Survived)
ggplot(full[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram(bins = 20) + theme_few()
ggplot(full[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram(bins = 10) + facet_grid(. ~ Sex) + theme_few()
table(full$Embarked, full$Survived)
ggplot(full[1:891, ], aes(x = Embarked, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + labs(x = "Embarked") + theme_few()
l <- c("Survived", "Pclass", "Sex", "Embarked", "Title")
index <- match(l, names(full))
for (i in index) {
    full[, i] <- as.factor(full[, i])
}
train2 <- full[1:891, ]
test2 <- full[892:1309, ]
set.seed(754)
rf_model_test <- randomForest(factor(Survived) ~ Pclass + Age + Sex + Title + Fare + family_size + Embarked, data = train2)
importance(rf_model_test)
rf_model_test <- randomForest(factor(Survived) ~ Pclass + Age + Sex + Title + Fare + family_size, data = train2)
rf_model_test <- randomForest(factor(Survived) ~ Pclass + Age + Sex + Title + Fare, data = train2)
rf_model_test <- randomForest(factor(Survived) ~ Pclass + Age + Title + Fare + family_size, data = train2)
rf_model_test <- randomForest(factor(Survived) ~ Age + Title + Fare + family_size, data = train2)
rf_model <- randomForest(factor(Survived) ~ Pclass + Age + Title + Fare + family_size, data = train2)
plot(rf_model, ylim = c(0, 0.4))
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3)
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()
prediction <- predict(rf_model, test2)
sol <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(sol, file = "Survived.csv", row.names = F)
