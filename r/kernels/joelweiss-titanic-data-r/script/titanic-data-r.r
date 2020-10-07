library(ggplot2)
library(readr)
system("ls ../input")
train <- read.csv("../input/train.csv", header = TRUE, stringsAsFactors = F)
test <- read.csv("../input/test.csv", header = TRUE, stringsAsFactors = F)
dim(train)
dim(test)
library("dplyr")
data <- bind_rows(train, test)
dim(data)
str(data)
data$Title <- gsub("(.*, )|(\\..*)", "", data$Name)
rare_title <- c("Dona", "Lady", "the Countess", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer")
data$Title[data$Title == "Mlle"] <- "Miss"
data$Title[data$Title == "Ms"] <- "Miss"
data$Title[data$Title == "Mme"] <- "Mrs"
data$Title[data$Title %in% rare_title] <- "Rare Title"
data$Embarked[c(62, 830)] <- "C"
data$Fare[1044] <- median(data[data$Pclass == "3" & data$Embarked == "S", ]$Fare, na.rm = TRUE)
data$Fare[data$Fare < 3] = NA
data$Fare[data$Fare > 300] <- median(data[data$Pclass == "1" & data$Embarked == "C", ]$Fare, na.rm = TRUE)
library("mice")
factor_vars <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
mice_mod <- mice(data[, names(data) %in% factor_vars], method = "rf")
mice_output <- complete(mice_mod)
for (ivar in factor_vars) {
    data[, ivar] <- mice_output[, ivar]
}
data$FamilySize <- data$SibSp + data$Parch + 1
data$Fare_Per_Person <- data$Fare/as.numeric(data$FamilySize)
data$Deck <- sapply(data$Cabin, function(x) strsplit(x, NULL)[[1]][1])
data$Deck[is.na(data$Deck)] <- "Unknown"
data$Age_Group[data$Age <= 10] <- "Kid"
data$Age_Group[data$Age <= 20 & data$Age > 10] <- "Teen"
data$Age_Group[data$Age <= 30 & data$Age > 20] <- "Young"
data$Age_Group[data$Age <= 40 & data$Age > 30] <- "Adult"
data$Age_Group[data$Age <= 50 & data$Age > 40] <- "Senior"
data$Age_Group[data$Age > 50] <- "old"
factor_vars <- c("Pclass", "SibSp", "Parch", "FamilySize")
data[factor_vars] <- lapply(data[factor_vars], function(x) as.factor(x))
factor_vars <- c("Name", "Sex", "Ticket", "Cabin", "Embarked", "Title", "Deck", "Age_Group")
data[factor_vars] <- lapply(data[factor_vars], function(x) as.factor(x))
train <- data[1:891, ]
test <- data[892:1309, ]
train$PassengerId <- NULL
train$Name <- NULL
train$Ticket <- NULL
train$Cabin <- NULL
library("randomForest")
rf_model <- randomForest(factor(Survived) ~ ., data = train)
importance(rf_model)
prediction <- predict(rf_model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "rf_mod_Solution.csv", row.names = F)
