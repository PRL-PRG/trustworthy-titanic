train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0
train$Child[is.na(train$Age)] <- NA
test$Child[test$Age < 18] <- 1
test$Child[test$Age >= 18] <- 0
test$Child[is.na(test$Age)] <- NA
library(rpart)
train$family_size <- train$SibSp + train$Parch + 1
test$family_size <- test$SibSp + test$Parch + 1
extractTitle <- function(Name) {
    Name <- as.character(Name)
    if (length(grep("Miss.", Name)) > 0) {
        return("Miss.")
    }
    else if (length(grep("Master.", Name)) > 0) {
        return("Master.")
    }
    else if (length(grep("Mrs.", Name)) > 0) {
        return("Mrs.")
    }
    else if (length(grep("Mr.", Name)) > 0) {
        return("Mr.")
    }
    else if (length(grep("Rev.", Name)) > 0) {
        return("Rev.")
    }
    else if (length(grep("Dr.", Name)) > 0) {
        return("Dr.")
    }
    else if (length(grep("Lady.", Name)) > 0) {
        return("Lady.")
    }
    else if (length(grep("Mlle.", Name)) > 0) {
        return("Mlle.")
    }
    else {
        return("Other")
    }
}
titles <- NULL
for (i in 1:nrow(train)) {
    titles <- c(titles, extractTitle(train[i, 4]))
}
train$Title <- as.factor(titles)
titles <- NULL
for (i in 1:nrow(test)) {
    titles <- c(titles, extractTitle(test[i, 3]))
}
test$Title <- as.factor(titles)
test$Survived <- rep("None", nrow(test))
test <- test[c("PassengerId", "Survived", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked", "Child", "family_size", "Title")]
all_data <- rbind(train, test)
all_data$Embarked[c(62, 830)] = "S"
all_data$Embarked <- factor(all_data$Embarked)
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data = all_data[!is.na(all_data$Age), ], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age), ])
train <- all_data[1:891, ]
test <- all_data[892:1309, ]
library(randomForest)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + family_size, data = train, importance = TRUE, ntree = 2000)
my_prediction <- predict(my_forest, test)
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
