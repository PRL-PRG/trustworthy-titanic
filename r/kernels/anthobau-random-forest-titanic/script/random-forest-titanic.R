library(dplyr)
library(ggplot2)
library(gridExtra)
library(mice)
library(randomForest)
train <- read.table("../input/train.csv", sep = ",", header = TRUE)
test <- read.table("../input/test.csv", sep = ",", header = TRUE)
full <- bind_rows(train, test)
str(train, give.attr = FALSE)
group_colors <- c(`0` = "tomato", `1` = "limegreen", male = "skyblue", female = "pink")
train$Survived <- factor(train$Survived)
ggplot(train, aes(x = Survived)) + geom_bar(fill = c(`0` = "tomato", `1` = "limegreen")) + labs(title = "Survival on the Titanic", x = "Survival", y = "Number of Passengers")
ggplot(train, aes(x = Sex)) + geom_bar(aes(fill = Survived), position = "fill") + scale_fill_manual(values = group_colors) + labs(title = "Survival by Sex", x = "Sex", y = "Proportion of Passengers")
ggplot(train, aes(x = Age)) + geom_histogram(aes(fill = Sex), binwidth = 2) + scale_fill_manual(values = group_colors) + labs(title = "Distribution of Passenger Age by Sex", x = "Age", y = "Number of Passengers")
ggplot(train, aes(x = Age)) + geom_histogram(aes(fill = Survived), binwidth = 2) + scale_fill_manual(values = group_colors) + labs(title = "Distribution of Passenger Age by Survival", x = "Age", y = "Number of Passengers")
ggplot(train, aes(x = Pclass)) + geom_bar(aes(fill = Survived)) + scale_fill_manual(values = group_colors) + labs(title = "Distribution of Passenger Class by Survival", x = "Passenger Class", y = "Number of Passengers")
ggplot(train, aes(x = Fare)) + geom_histogram(aes(fill = Survived), binwidth = 10) + scale_fill_manual(values = group_colors) + labs(title = "Distribution of Journey Fare by Survival", x = "Fare Paid", y = "Number of Passengers")
ggplot(train, aes(x = Embarked)) + geom_bar(aes(fill = Survived)) + scale_fill_manual(values = group_colors) + labs(title = "Distribution of Journey Origin by Survival", x = "Origin (port of embarkment)", y = "Number of Passengers")
test$Survived <- NA
combined <- bind_rows(train, test)
str(combined, give.attr = FALSE)
sapply(combined, function(x) sum(is.na(x)))
combined$Survived <- factor(combined$Survived)
combined$Pclass <- factor(combined$Pclass)
combined$Sex <- factor(combined$Sex)
combined$Embarked <- factor(combined$Embarked)
str(combined, give.attr = FALSE)
set.seed(1234)
imputes <- mice(combined[c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")], method = "rf")
imputes_output <- complete(imputes)
impute_age <- ggplot(imputes_output, aes(x = Age)) + geom_histogram(binwidth = 2, fill = "thistle") + labs(x = "Imputed Age")
age <- ggplot(train, aes(x = Age)) + geom_histogram(binwidth = 2)
grid.arrange(age, impute_age, ncol = 2)
impute_embarked <- ggplot(imputes_output, aes(x = Embarked)) + geom_bar(fill = "thistle") + labs(x = "Imputed Origin")
embarked <- ggplot(train, aes(x = Embarked)) + geom_bar() + labs(x = "Origin")
grid.arrange(embarked, impute_embarked, ncol = 2)
combined$Age <- imputes_output$Age
combined$Fare <- imputes_output$Fare
combined$Embarked <- imputes_output$Embarked
sapply(combined, function(x) sum(is.na(x)))
combined$Title <- factor(gsub("(.*, )|(\\..*)", "", combined$Name))
table(combined$Title)
combined$FamSize <- combined$SibSp + combined$Parch + 1
ggplot(combined, aes(x = FamSize)) + geom_bar() + labs(x = "Family Size", y = "Number of Passengers", title = "Family Size of Passengers")
combined$child <- NA
combined$child[combined$Age <= 16] <- TRUE
combined$child[combined$Age > 16] <- FALSE
str(combined, give.attr = FALSE)
combined$FamilySize <- combined$Parch + combined$SibSp + 1
combined$Mother <- 0
combined$Mother[combined$Sex == "female" & combined$Parch > 0 & combined$Age > 18 & combined$Title != "Miss"] <- 1
combined$Child <- 0
combined$Child[combined$Parch > 0 & combined$Age <= 12] <- 1
train <- combined[1:891, ]
test <- combined[892:1309, ]
rf_titanic <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamSize + child, data = train, ntree = 1000)
rf_titanic
plot(rf_titanic)
vimp <- importance(rf_titanic)
vimp_df <- data.frame(Var = row.names(vimp), vimp)
vimp_df %>% arrange(desc(MeanDecreaseGini))
predicted <- predict(rf_titanic, newdata = test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = predicted)
write.csv(solution, "../input/gender_submission.csv", row.names = FALSE)
