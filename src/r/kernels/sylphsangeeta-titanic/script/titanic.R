library(dplyr)
train_df <- read.csv("../input/train.csv", stringsAsFactors = F)
cat(paste("\ntest data structure having rows :\n", nrow(train_df)))
str(train_df)
test_df <- read.csv("../input/test.csv", stringsAsFactors = F)
cat(paste("\ntest data structure having rows :\n", nrow(test_df)))
str(test_df)
cat("\nSummary Of Pclass\n")
summary(train_df$Pclass)
cat("\nSummary Of Age\n")
summary(train_df$Age)
cat("\nSummary Of Sibsp A\n")
summary(train_df$SibSp)
cat("\nSummary Of Parch A\n")
summary(train_df$Parch)
cat("\nSummary Of Fare A\n")
summary(train_df$Fare)
fare_single_passenger <- subset(train_df, (train_df$SibSp == 0) & (train_df$Parch == 0), select = c(Pclass, Fare))
fare_single_passenger
fare_single_passenger <- aggregate(fare_single_passenger[, 2], list(fare_single_passenger$Pclass), mean)
cat(paste("\n\nAverage Fare of First Class ", fare_single_passenger[1, 2]))
cat(paste("\n\nAverage Fare of Second Class ", fare_single_passenger[2, 2]))
cat(paste("\n\nAverage Fare of Third Class ", fare_single_passenger[3, 2]))
library(ggplot2)
ggplot(train_df, aes(x = Pclass, y = Survived, fill = Pclass)) + geom_bar(stat = "identity", width = 0.3) + scale_x_continuous(breaks = c(1, 2, 3), labels = c("1", "2", "3"))
ggplot(train_df, aes(x = Age, y = Survived, fill = Age)) + geom_bar(stat = "identity", width = 0.5) + scale_x_continuous(breaks = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80), labels = c("10", "15", "20", "25", "30", "35", "40", "45", "50", "55", "60", "65", "70", "75", "80"))
ggplot(train_df, aes(x = SibSp, y = Survived, fill = SibSp)) + geom_bar(stat = "identity", width = 0.3)
ggplot(train_df, aes(x = Parch, y = Survived, fill = Parch)) + geom_bar(stat = "identity", width = 0.3)
ggplot(train_df, aes(x = Fare, y = Survived, fill = Fare)) + geom_bar(stat = "identity", width = 0.5)
ggplot(train_df, aes(x = Embarked, y = Survived, fill = Embarked)) + geom_bar(stat = "identity", width = 0.3)
ggplot(train_df, aes(x = Sex, y = Survived, fill = Sex)) + geom_bar(stat = "identity", width = 0.5)
lenofUniqueTicekt <- length(unique(train_df$Ticket))
cat(paste("Number of uniques Ticket", lenofUniqueTicekt))
FamGp_df <- train_df[duplicated(train_df[, "Ticket"]) | duplicated(train_df[, "Ticket"], fromLast = TRUE), ] %>% arrange(Ticket) %>% group_by(Ticket) %>% summarise(groupFmSize = n(), Survived = sum(Survived))
FamGp_df
training_model <- glm(Survived ~ . - (Name + PassengerId + Ticket + Cabin), family = binomial(link = "logit"), data = train_df, maxit = 50)
summary(training_model)
library(ggplot2)
ggplot(train_df, aes(Age, fill = factor(Survived))) + geom_histogram() + facet_grid(. ~ Sex) + theme_dark()
library(dplyr)
titanic_df <- bind_rows(train_df, test_df)
cat(paste("\ncomplete  data structure having rows :", nrow(titanic_df)))
str(titanic_df)
library(dplyr)
nrow((titanic_df))
FamGp_df <- titanic_df[duplicated(titanic_df[, "Ticket"]) | duplicated(titanic_df[, "Ticket"], fromLast = TRUE), ] %>% arrange(Ticket) %>% group_by(Ticket) %>% summarise(groupFmSize = n(), Survived = sum(Survived))
FamGp_df <- FamGp_df[, c("Ticket", "groupFmSize")]
FamGp_df
library(dplyr)
titanic_df <- merge(x = titanic_df, y = FamGp_df, by = "Ticket", all = TRUE)
cat(paste("\ncomplete  data structure having rows :", nrow(titanic_df)))
col_order <- c("PassengerId", "Survived", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "groupFmSize", "Fare", "Cabin", "Embarked")
titanic_df <- titanic_df[, col_order] %>% arrange(PassengerId)
titanic_df
titanic_df$groupFmSize[is.na(titanic_df$groupFmSize)] <- 0
titanic_df
library(VIM)
mice_plot <- aggr(titanic_df[, c("Age")], col = c("blue", "red"), numbers = TRUE, sortVars = TRUE, labels = c("Age"), cex.axis = 0.7, gap = 3, ylab = c("Missing data", "Pattern"))
library(Hmisc)
titanic_df$Age <- with(titanic_df, impute(Age, median))
titanic_df[, c("Age")]
titanic_df$AgeCategory[titanic_df$Age <= 18] <- "child"
titanic_df$AgeCategory[titanic_df$Age > 18 & titanic_df$Sex == "female"] <- "FemaleAdult"
titanic_df$AgeCategory[titanic_df$Age > 18 & titanic_df$Sex == "male"] <- "maleAdult"
titanic_df$AgeCategory <- factor(titanic_df$AgeCategory)
head(titanic_df[, c("PassengerId", "Age", "Sex", "AgeCategory")], 50)
final_train_df <- titanic_df[1:nrow(train_df), ]
final_test_df <- titanic_df[nrow(train_df) + 1:nrow(test_df), ]
head(final_train_df)
head(final_test_df)
library("rpart")
library("rpart.plot")
rtree_fit <- rpart(Survived ~ Pclass + Age + factor(AgeCategory) + Sex + groupFmSize, final_train_df, method = "class")
summary(rtree_fit)
rpart.plot(rtree_fit, extra = 104, box.palette = "GnBu", branch.lty = 3, shadow.col = "gray", nn = TRUE)
library(MASS)
survivalPrediction <- predict(rtree_fit, final_test_df, type = "class")
str(survivalPrediction)
tableP <- table(survivalPrediction)
pct <- round(tableP/sum(tableP) * 100)
label <- c("0", "1")
lbls <- paste(label, "-", pct, "%")
pie(tableP, col = c("red", "yellow"), labels = lbls)
predicted_df <- data.frame(PassengerID = final_test_df$PassengerId, Survived = survivalPrediction)
head(predicted_df, 200)
write.csv(predicted_df, file = "Titanic_Prediction_Rpart.csv", row.names = F)
