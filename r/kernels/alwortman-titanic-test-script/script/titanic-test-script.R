library(randomForest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(dplyr)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train$family_size <- train$SibSp + train$Parch + 1
test$family_size <- test$SibSp + test$Parch + 1
test$Survived <- NA
combi <- rbind(train, test)
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][2]
})
combi$Title <- sub(" ", "", combi$Title)
combi$Title[combi$Title %in% c("Mme", "Mlle")] <- "Mlle"
combi$Title[combi$Title %in% c("Capt", "Don", "Major", "Jonkheer", "Sir")] <- "Sir"
combi$Title[combi$Title %in% c("Dona", "Lady", "the Countess")] <- "Lady"
combi$Title <- factor(combi$Title)
all_data = combi
all_data$Embarked[c(62, 830)] = "S"
all_data$Embarked <- factor(combi$Embarked)
all_data$Fare[1044] <- median(combi$Fare, na.rm = TRUE)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data = all_data[!is.na(all_data$Age), ], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age), ])
train <- all_data[1:891, ]
test <- all_data[892:1309, ]
set.seed(111)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train, importance = TRUE, ntree = 1000)
my_prediction <- predict(my_forest, test)
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
my_solution_df <- tbl_df(my_solution)
