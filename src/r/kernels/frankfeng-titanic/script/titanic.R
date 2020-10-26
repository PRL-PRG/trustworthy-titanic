library(randomForest)
library(rpart)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
test$Survived <- 0
all_data <- rbind(train, test)
all_data$Embarked[c(62, 830)] <- "S"
all_data$Embarked <- factor(all_data$Embarked)
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)
library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = all_data[!is.na(all_data$Age), ], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age), ])
train <- all_data[1:891, ]
test <- all_data[892:1309, ]
my_tree <- randomForest(as.factor(Survived) ~ Age + Sex + Pclass + SibSp + Parch + Embarked + Fare, data = train, importance = TRUE, ntree = 1000)
my_prediction <- predict(my_tree, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
