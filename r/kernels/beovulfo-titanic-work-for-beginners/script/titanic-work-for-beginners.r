library("ggplot2")
library("ggthemes")
library("scales")
library("mice")
library("randomForest")
library("caret")
library("Amelia")
library("dplyr")
training.data.raw <- read.csv("../input/train.csv", header = T, na.strings = c(""), stringsAsFactors = F)
test.data.raw <- read.csv("../input/test.csv", header = T, na.strings = c(""), stringsAsFactors = F)
full <- bind_rows(training.data.raw, test.data.raw)
sapply(full, function(x) sum(is.na(x)))
missmap(full, main = "Missing values vs observed")
head(full)
summary(full)
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
table(full$Pclass, full$Title)
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
full$Fsize <- full$SibSp + full$Parch + 1
full$FsizeD[full$Fsize == 1] <- "singleton"
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- "small"
full$FsizeD[full$Fsize > 4] <- "large"
mosaicplot(table(full$FsizeD, full$Survived), main = "Family Size by Survival", shade = TRUE)
full[full$PassengerId[is.na(full$Embarked)], ]
embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
full$Embarked[c(62, 830)] <- "C"
full[full$PassengerId[is.na(full$Fare)], ]
mean_mv <- mean(na.omit(full$Fare[full$Embarked == "S" & full$Pclass == 3]))
full$Fare[1044] <- mean_mv
ggplot(full[full$Pclass == "3" & full$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", lwd = 1) + scale_x_continuous(labels = dollar_format()) + theme_few()
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title %in% rare_title] <- "Rare Title"
table(full$Sex, full$Title)
colnames(full)
factor_vars <- c("Pclass", "Embarked", "SibSp", "Parch", "Title")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived")], method = "rf")
mice_output <- complete(mice_mod)
par(mfrow = c(1, 2))
hist(full$Age, freq = F, main = "Age: Original Data", col = "darkgreen", ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = "Age: MICE Output", col = "lightgreen", ylim = c(0, 0.04))
full$Age <- mice_output$Age
sum(is.na(full$Age))
ggplot(full[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Sex) + theme_few()
full$Child[full$Age < 18] <- "Child"
full$Child[full$Age >= 18] <- "Adult"
table(full$Child, full$Survived)
full$Child <- factor(full$Child)
full$Sex <- factor(full$Sex)
train <- full[1:891, ]
test <- full[892:1309, ]
is.factor(train$Sex)
is.factor(train$Embarked)
contrasts(train$Sex)
sapply(train, function(x) sum(is.na(x)))
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Child, data = train)
plot(rf_model, ylim = c(0, 0.36))
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3)
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(Importance))))
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) + geom_bar(stat = "identity") + geom_text(aes(x = Variables, y = 0.5, label = Rank), hjust = 0, vjust = 0.55, size = 4, colour = "red") + labs(x = "Variables") + coord_flip() + theme_few()
summary(rf_model)
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "rf_mod_Solution3.csv", row.names = F)
anova(rf_model, test = "Chisq")
library(pscl)
pR2(model)
fitted.results <- predict(model, newdata = subset(test, select = c(2, 3, 4, 5, 6, 7, 8)), type = "response")
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)
misClasificError <- mean(fitted.results != test$Survived)
print(paste("Accuracy", 1 - misClasificError))
install.packages("ROCR")
library(ROCR)
p <- predict(model, newdata = subset(test, select = c(2, 3, 4, 5, 6, 7, 8)), type = "response")
pr <- prediction(p, test$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
test.raw <- read.csv("titanic/test.csv", header = T, na.strings = c(""))
sapply(test.raw, function(x) sum(is.na(x)))
sapply(test.raw, function(x) length(unique(x)))
missmap(test.raw, main = "Missing values vs observed")
vnames <- colnames(test.raw)
vnames
vnames <- vnames[c(2, 4, 5, 7)]
vnames
test <- subset(test.raw, select = vnames)
test$Age[is.na(test$Age)] <- median(data$Age, na.rm = T)
NAN <- subset(test, is.na(Fare))
NAN$Pclass
temp <- subset(data, Pclass == NAN$Pclass)
mean_fare_NAN <- mean(temp$Fare)
mean_fare_NAN
test$Fare[is.na(test$Fare)] <- mean_fare_NAN
embark_fare <- test
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
train <- data[1:889, ]
model <- glm(Survived ~ ., family = binomial(link = "logit"), data = train)
summary(model)
anova(model, test = "Chisq")
library(pscl)
pR2(model)
p <- predict(model, newdata = subset(test, select = vnames), type = "response")
test[153, ]
solution <- data.frame(PassengerID = test.raw$PassengerId, Survived = round(p))
write.csv(solution, file = "rf_mod_Solution2.csv", row.names = F)
