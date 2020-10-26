library(dplyr)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
dim(train)
dim(test)
full <- bind_rows(train, test)
dim(full)
head(full)
str(train)
head(train)
str(test)
head(test)
library(rpart)
decision_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(decision_tree)
prediction_1 <- predict(decision_tree, newdata = test, type = "class")
solution_1 <- data.frame(PassengerId = test$PassengerId, Survived = prediction_1)
write.csv(solution_1, file = "my_solution.csv", row.names = FALSE)
summary(full)
id <- full[which(is.na(full$Fare)), 1]
full[id, ]
median_fare <- full %>% filter(Pclass == "3" & Embarked == "S") %>% summarise(missing_fare = median(Fare, na.rm = TRUE))
median_fare
full$Embarked[full$Embarked == ""] <- NA
full[which(is.na(full$Embarked)), 1]
full$Cabin[full$Cabin == ""] <- NA
full[which(is.na(full$Cabin)), 1]
library(caret)
controlParameters <- trainControl(method = "cv", number = 10, repeats = 10, verboseIter = TRUE)
decision_tree_model <- train(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = full, trControl = controlParameters, method = "rpart", na.action = na.omit)
train_1 <- data.frame(train$Pclass, train$Sex, train$Age, train$SibSp, train$Parch, train$Fare, train$Embarked)
head(train_1)
plot(train)
summary(solution_1)
