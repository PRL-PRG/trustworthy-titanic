pacman::p_load(randomForest, ggplot2)
train <- read.csv("../input/train.csv", na.strings = c("", "NA", NA))
test <- read.csv("../input/test.csv")
colnames(train) <- tolower(colnames(train))
str(train)
train$pclass <- factor(train$pclass)
table(train$survived)
table(train$embarked)
levels(train$embarked) <- gsub("C", "Cherbourg", levels(train$embarked))
levels(train$embarked) <- gsub("Q", "Queenstown", levels(train$embarked))
levels(train$embarked) <- gsub("S", "Southampton", levels(train$embarked))
summary(train$fare)
ggplot(train, aes(x = train$fare)) + geom_histogram()
ggplot(train, aes(pclass, fare)) + geom_boxplot()
logistic <- glm(survived ~ sex * age * fare, family = binomial(link = "logit"), data = train)
predictions <- as.data.frame(predict(logistic, type = "response"))
summary(logistic)
train.age <- train[!is.na(train$age), ]
train.age$prediction <- predict(logistic, type = "response")
cor.test(train.age$survived, train.age$prediction)
train.age$pred.binary <- train.age$prediction
train.age$pred.binary[train.age$pred.binary < 0.5] <- 0
train.age$pred.binary[train.age$pred.binary > 0.5] <- 1
cor.test(train.age$survived, train.age$pred.binary)
colnames(test) <- tolower(colnames(test))
test$prediction <- predict(logistic, newdata = test, type = "response")
test$prediction[test$prediction < 0.5] <- 0
test$prediction[test$prediction > 0.5] <- 1
test$prediction[is.na(test$prediction)] <- 0
colnames(test)
submit.1 <- test[, c(1, 12)]
colnames(submit.1) <- c("PassengerId", "Survived")
write.csv(submit.1, file = "submit.1.csv")
