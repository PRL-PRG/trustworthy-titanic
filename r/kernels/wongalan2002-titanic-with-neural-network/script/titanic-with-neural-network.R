library(randomForest)
library(readr)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train_temp <- train
train$Survived <- NULL
all_data <- rbind(train, test)
all_data$Embarked[c(62, 830)] <- "S"
all_data$Embarked <- factor(all_data$Embarked)
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)
all_data$Title <- sapply(all_data$Name, FUN = function(x) {
    strsplit(toString(x), split = "[,.]")[[1]][2]
})
all_data$Title <- as.factor(all_data$Title)
library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title, data = all_data[!is.na(all_data$Age), ], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age), ])
train <- all_data[1:891, ]
test <- all_data[892:1309, ]
train$Survived <- train_temp$Survived
library(nnet)
model_final <- nnet(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train, size = 35, maxit = 10000)
my_prediction <- predict(model_final, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
