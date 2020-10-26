library(ggplot2)
library(readr)
df_train <- read.csv("../input/train.csv", na.strings = "")
df_test <- read.csv("../input/test.csv", na.strings = "")
summary(df_train)
head(df_train)
ggplot(data = df_train, mapping = aes(Age)) + geom_histogram(aes(fill = as.factor(Survived), y = ..density..)) + geom_density()
df_train <- subset(df_train, select = -c(Cabin, Name, Ticket, PassengerId, Age))
summary(df_train)
df_train$Embarked[is.na(df_train$Embarked)] <- names(sort(table(df_train$Embarked), decreasing = TRUE)[1])
summary(df_train)
library(reshape)
df_train_melt <- melt(df_train, id.vars = c("Survived", "Sex", "Embarked"))
ggplot(data = df_train_melt, mapping = aes(x = value, fill = as.factor(Survived), color = as.factor(Survived))) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = "free_x")
ggplot(data = df_train, mapping = aes(Sex)) + geom_bar(aes(fill = as.factor(Survived)))
library(rpart.plot)
mytree <- rpart(Survived ~ ., data = df_train, method = "class")
rpart.plot(mytree, type = 4, extra = 101)
library(caret)
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_glm_Sex <- train(as.factor(Survived) ~ Sex, data = df_train, method = "glm", family = "binomial", trControl = ctrl, tuneLength = 5)
mod_glm_Sex
mod_glm_all <- train(as.factor(Survived) ~ ., data = df_train, method = "glm", family = "binomial", trControl = ctrl, tuneLength = 5)
mod_glm_all
mod_rf_all <- train(as.factor(Survived) ~ ., data = df_train, method = "rf", trControl = ctrl, tuneLength = 5)
mod_rf_all
summary(df_test)
df_test$Fare[is.na(df_test$Fare)] <- mean(df_test$Fare, na.rm = T)
summary(df_test)
predictions_rf2 <- cbind(df_test$PassengerId, as.numeric(predict(mod_rf_all, newdata = df_test)) - 1)
colnames(predictions_rf2) <- c("PassengerId", "Survived")
write.csv(file = "prediction_rf2.csv", predictions_rf2, row.names = F, quote = FALSE)