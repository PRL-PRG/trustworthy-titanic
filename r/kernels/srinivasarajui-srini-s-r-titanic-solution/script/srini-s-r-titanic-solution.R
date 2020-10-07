train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)
train
test
str(train)
str(test)
table(train$Survived)
prop.table(table(train$Survived))
table(train$Sex, train$Survived)
prop.table(table(train$Sex, train$Survived), margin = 1)
prop.table(table(train$Sex, train$Survived), margin = 2)
str(train)
str(test)
train$Child <- NA
train$Child[train$Age < 18] <- "Child"
train$Child[train$Age >= 18] <- "Adult"
prop.table(table(train$Child, train$Survived), margin = 1)
str(train)
str(test)
test_one <- test
test_one$Survived <- 0
test_one$Survived[test_one$Sex == "female"] <- 1
library(rpart)
str(train)
str(test)
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
plot(my_tree_two)
text(my_tree_two)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(my_tree_two)
my_prediction <- predict(my_tree_two, test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
nrow(my_solution)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
fancyRpartPlot(my_tree_three)
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, data = train_two, method = "class")
fancyRpartPlot(my_tree_four)
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train_new, method = "class")
fancyRpartPlot(my_tree_five)
my_prediction <- predict(my_tree_five, test_new, type = "class")
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
all_data
all_data$Embarked[c(62, 830)] <- "S"
all_data$Embarked <- factor(all_data$Embarked)
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)
library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size, data = all_data[!is.na(all_data$Age), ], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age), ])
train <- all_data[1:891, ]
test <- all_data[892:1309, ]
str(train)
str(test)
library(randomForest)
str(train)
str(test)
set.seed(111)
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train, importance = TRUE, ntree = 1000)
my_prediction <- predict(my_forest, test)
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution2.csv", row.names = FALSE)
