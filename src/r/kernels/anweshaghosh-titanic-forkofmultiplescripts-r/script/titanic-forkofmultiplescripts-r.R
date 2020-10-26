library(data.table)
readData <- function(file.name, column.types, missing.types) {
    read.csv(file.name, colClasses = column.types, na.strings = missing.types, stringsAsFactors = FALSE)
}
train.data.file <- "../input/train.csv"
test.data.file <- "../input/test.csv"
missing.types <- c("NA", "")
train.column.types <- c("integer", "factor", "factor", "character", "factor", "numeric", "integer", "integer", "character", "numeric", "character", "factor")
test.column.types <- train.column.types[-2]
train <- readData(train.data.file, train.column.types, missing.types)
test <- readData(test.data.file, test.column.types, missing.types)
summary(train)
require(Amelia)
missmap(train, main = "Titanic Training Data - Missing Data", col = c("red", "black"))
missmap(test, main = "Titanic Test Data - Missing Data", col = c("red", "black"))
library(ggplot2)
library(ggthemes)
pi1 <- ggplot(train, aes(x = PassengerId, y = Survived))
pi1 + geom_point()
mosaicplot(train$Pclass ~ train$Survived, main = "Passenger Fate by Traveling Class", shade = FALSE, color = TRUE, xlab = "Pclass", ylab = "Survived")
mosaicplot(train$Sex ~ train$Survived, main = "Passenger Fate by Gender", shade = FALSE, color = TRUE, xlab = "Pclass", ylab = "Survived")
boxplot(train$Age ~ train$Survived, main = "Passenger Fate by Age", xlab = "Survived", ylab = "Age")
mosaicplot(train$SibSp ~ train$Survived, main = "Passenger Fate by Siblings", shade = FALSE, color = TRUE, xlab = "SibSp", ylab = "Survived")
mosaicplot(train$Parch ~ train$Survived, main = "Passenger Fate by Parents/Children", shade = FALSE, color = TRUE, xlab = "Parch", ylab = "Survived")
boxplot(train$Fare ~ train$Survived, main = "Passenger Fate by Fare", xlab = "Survived", ylab = "Fare")
boxplot(train$Fare ~ train$Pclass, main = "Fare vs Passenger Class", xlab = "Pclass", ylab = "Fare")
mosaicplot(train$Embarked ~ train$Survived, main = "Passenger Fate by Port of Embarkation", shade = FALSE, color = TRUE, xlab = "Embarked", ylab = "Survived")
barplot(table(train$Embarked), names.arg = c("Cherbourg", "Queenstown", "Southampton"), main = "Embarked (Port of Emparkation)")
require(corrgram)
require(plyr)
corrgram.data <- train
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, c(C = 1, Q = 2, S = 3))
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[0:891, corrgram.vars], order = FALSE, lower.panel = panel.ellipse, upper.panel = panel.pie, text.panel = panel.txt, main = "Titanic Training Data")
train$Fate <- train$Survived
train$Fate <- revalue(train$Fate, c(`1` = "Survived", `0` = "Perished"))
train$Title <- gsub("(.*, )|(\\..*)", "", train$Name)
table(train$Sex, train$Title)
test$Title <- gsub("(.*, )|(\\..*)", "", test$Name)
table(test$Sex, test$Title)
rare_title <- c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir", "the Countess")
train$Title[train$Title == "Mlle"] <- "Miss"
train$Title[train$Title == "Ms"] <- "Miss"
train$Title[train$Title == "Mme"] <- "Mrs"
train$Title[train$Title %in% rare_title] <- "Rare"
test$Title[test$Title == "Mlle"] <- "Miss"
test$Title[test$Title == "Ms"] <- "Miss"
test$Title[test$Title == "Mme"] <- "Mrs"
test$Title[test$Title %in% rare_title] <- "Rare"
table(train$Sex, train$Title)
table(test$Sex, test$Title)
train$Surname <- sapply(train$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
test$Surname <- sapply(test$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
train$Fsize = train$SibSp + train$Parch + 1
test$Fsize = test$SibSp + test$Parch + 1
train$Family <- paste(train$Surname, train$Fsize, sep = "_")
test$Family <- paste(test$Surname, test$Fsize, sep = "_")
ggplot(train, aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
train$FsizeD[train$Fsize == 1] <- "singleton"
train$FsizeD[train$Fsize < 5 & train$Fsize > 1] <- "small"
train$FsizeD[train$Fsize > 4] <- "large"
test$FsizeD[test$Fsize == 1] <- "singleton"
test$FsizeD[test$Fsize < 5 & test$Fsize > 1] <- "small"
test$FsizeD[test$Fsize > 4] <- "large"
mosaicplot(table(train$FsizeD, train$Survived), main = "Family Size by Survival", shade = TRUE)
sum(is.na(train$Age))
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Title", "Surname", "Family", "FsizeD")
train[factor_vars] <- lapply(train[factor_vars], function(x) as.factor(x))
test[factor_vars] <- lapply(test[factor_vars], function(x) as.factor(x))
set.seed(129)
library(mice)
mice_mod <- mice(train[, !names(train) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived", "Embarked")], method = "rf")
mice_output <- complete(mice_mod)
par(mfrow = c(1, 2))
hist(train$Age, freq = F, main = "Age: Original Data", col = "darkgreen", ylim = c(0, 0.04))
hist(mice_output$Age, freq = F, main = "Age: MICE Output", col = "lightgreen", ylim = c(0, 0.04))
train$Age <- mice_output$Age
sum(is.na(train$Age))
mice_mod2 <- mice(test[, !names(test) %in% c("PassengerId", "Name", "Ticket", "Cabin", "Family", "Surname", "Survived", "Embarked")], method = "rf")
mice_output2 <- complete(mice_mod2)
par(mfrow = c(1, 2))
hist(test$Age, freq = F, main = "Age: Original Data", col = "darkgreen", ylim = c(0, 0.04))
hist(mice_output2$Age, freq = F, main = "Age: MICE Output", col = "lightgreen", ylim = c(0, 0.04))
test$Age <- mice_output2$Age
sum(is.na(test$Age))
plot(train$Age, train$Survived, xlab = "Age", ylab = "Survived")
survivers <- data.frame(train$Age[train$Survived == 1])
nonsurvivers <- data.frame(train$Age[train$Survived == 0])
survivers$title <- "Survivers"
nonsurvivers$title <- "Non-Survivers"
colnames(survivers)[1] <- "Age"
colnames(nonsurvivers)[1] <- "Age"
hist(survivers$Age, breaks = 32, xlim = c(0, 80), ylim = c(0, 40), col = "red")
hist(nonsurvivers$Age, breaks = 32, add = T, col = rgb(0, 1, 0, 0.5))
train$Agegroup[train$Age < 14] <- "child"
train$Agegroup[train$Age >= 14] <- "adult"
test$Agegroup[test$Age < 14] <- "child"
test$Agegroup[test$Age >= 14] <- "adult"
table(train$Agegroup, train$Survived)
mosaicplot(table(train$Agegroup, train$Survived), main = "Age Group by Survival", shade = TRUE)
ggplot(train, aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Sex) + theme_few()
library(dplyr)
full1 <- bind_rows(select(train, Sex, Title, Age), select(test, Sex, Title, Age))
female_age <- full1 %>% filter(Sex == "female")
plot(female_age$Title, female_age$Age)
b <- female_age[female_age$Title == "Mrs", ]
min(b$Age)
train$Mother <- "Not Mother"
train$Mother[train$Sex == "female" & train$Parch > 0 & train$Age > min(b$Age) & train$Title != "Miss"] <- "Mother"
test$Mother <- "Not Mother"
test$Mother[test$Sex == "female" & test$Parch > 0 & test$Age > min(b$Age) & test$Title != "Miss"] <- "Mother"
table(train$Mother, train$Survived)
train$Agegroup <- factor(train$Agegroup)
train$Mother <- factor(train$Mother)
test$Agegroup <- factor(test$Agegroup)
test$Mother <- factor(test$Mother)
table(is.na(train$Embarked))
table(is.na(test$Embarked))
library(dplyr)
full <- bind_rows(select(train, Embarked, Pclass, Fare), select(test, Embarked, Pclass, Fare))
embark_fare <- full %>% filter(Embarked == "NA")
library(scales)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
train[is.na(train$Embarked), ]
train$Embarked[c(62, 830)] <- "C"
table(is.na(train$Embarked))
table(is.na(train$Fare))
table(is.na(test$Fare))
test[is.na(test$Fare), ]
ggplot(full[full$Pclass == "3" & full$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", lwd = 1) + scale_x_continuous(labels = dollar_format()) + theme_few()
a <- full[full$Pclass == "3" & full$Embarked == "S", ]
a <- a[is.na(a$Fare) == FALSE, ]
median(a$Fare)
test$Fare[153] <- median(a$Fare)
library(caret)
control <- trainControl(method = "cv", number = 10)
metric <- "Accuracy"
set.seed(7)
fit.lda <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data = train, method = "lda", metric = metric, trControl = control)
set.seed(7)
fit.cart <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data = train, method = "rpart", metric = metric, trControl = control)
set.seed(7)
fit.knn <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data = train, method = "knn", metric = metric, trControl = control)
set.seed(7)
fit.svm <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data = train, method = "svmRadial", metric = metric, trControl = control)
set.seed(7)
fit.rf <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Agegroup + Mother, data = train, method = "rf", metric = metric, trControl = control)
results <- resamples(list(lda = fit.lda, cart = fit.cart, knn = fit.knn, svm = fit.svm, rf = fit.rf))
summary(results)
dotplot(results)
print(fit.rf)
varImportance <- data.frame(varImp(fit.rf)$importance)
varImportance$Vars <- row.names(varImportance)
varImportance[order(-varImportance$Overall), ]
rankImportance <- varImportance %>% mutate(Rank = paste0("#", dense_rank(desc(varImportance$Overall))))
prediction <- predict(fit.rf, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
