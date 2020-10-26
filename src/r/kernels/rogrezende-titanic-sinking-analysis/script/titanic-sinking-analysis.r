library("ggplot2")
library("ggthemes")
library("scales")
library("dplyr")
library("VIM")
library("mice")
library("randomForest")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)
str(full)
MData <- sapply(full, function(x) sum(is.na(x)))
MData[MData > 0]
MData
aggr_plot <- aggr(full, col = c("navyblue", "red"), numbers = TRUE, sortVars = TRUE, labels = names(full), cex.axis = 0.7, gap = 3, ylab = c("Histogram of missing data", "Pattern"))
TMPfull <- mice(full, m = 5, maxit = 50, meth = "pmm", seed = 500)
TMPComplete <- complete(TMPfull)
full$Age[1:30]
TMPComplete$Age[1:30]
full$Age <- TMPComplete$Age
NData <- sapply(full, function(x) sum(is.na(x)))
NData[NData > 0]
FareM <- full[is.na(full$Fare), ]
FareM
full$Fare[1044] <- median(full[full$Pclass == "3" & full$Embarked == "S", ]$Fare, na.rm = TRUE)
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
table(full$Sex, full$Title)
Rare <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title %in% Rare] <- "Rare"
table(full$Sex, full$Title)
table(full$Sex, full$Title)
full$Surname <- sapply(full$Name, function(x) strsplit(x, split = "[,.]")[[1]][1])
nr_surname <- nlevels(factor(full$Surname))
nr_surname
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep = "_")
full$Survived[full$Survived == "0"] <- "Died"
full$Survived[full$Survived == "1"] <- "Survived"
ggplot(full[1:891, ], aes(x = Fsize, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Family Size") + theme_few()
full$FsizeD[full$Fsize == 1] <- "singleton"
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- "small"
full$FsizeD[full$Fsize > 4] <- "large"
mosaicplot(table(full$FsizeD, full$Survived), main = "Family Size by Survival", shade = TRUE)
full[full$Embarked != "C" & full$Embarked != "Q" & full$Embarked != "S", ]
embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)
ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 2) + scale_y_continuous(labels = dollar_format()) + theme_few()
full$Embarked[c(62, 830)] <- "C"
ggplot(full[1:891, ], aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Sex) + theme_few()
ggplot(full[1:891, ], aes(x = Pclass, fill = factor(Survived), label = Pclass)) + geom_bar(stat = "count", position = "dodge") + scale_x_continuous(breaks = c(1:11)) + labs(x = "Survival by Class") + theme_few()
md.pattern(full)
train <- full[1:891, ]
test <- full[892:1309, ]
train$Survived[train$Survived == "Died"] <- 0
train$Survived[train$Survived == "Survived"] <- 1
factor_vars <- c("PassengerId", "Pclass", "Sex", "Embarked", "Age", "SibSp", "Parch", "Fare", "Title", "Surname", "Family", "FsizeD")
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(754)
rf_model <- randomForest(factor(Survived) ~ Pclass + Age + SibSp + Parch + Fare + Fsize, data = train, importance = TRUE)
plot(rf_model, ylim = c(0, 0.36))
legend("topright", colnames(rf_model$err.rate), col = 1:3, fill = 1:3)
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
