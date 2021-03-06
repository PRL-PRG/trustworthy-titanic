library(tidyverse)
library(rpart)
library(randomForest)
library(modelr)
library(party)
library(xlsx)
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")
summary(train)
str(train)
head(train)
tail(train)
prop.table(table(train$Survived))
prop.table(table(train$Sex))
prop_sexo <- prop.table(table(train$Sex, train$Survived), 1)
prop_sexo
summary(train$Age)
train <- train %>% mutate(Child = ifelse(train$Age < 18, 1, 0))
test <- test %>% mutate(Child = ifelse(test$Age < 18, 1, 0))
aggregate(data = train, Survived ~ Child + Sex, FUN = sum)
prop_sexo_edad <- aggregate(data = train, Survived ~ Child + Sex, FUN = function(x) {
    sum(x)/length(x)
})
prop_sexo_edad
fit <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Child, data = train)
plot(fit, uniform = TRUE, main = "primer intento")
text(fit, all = TRUE, use.n = TRUE, cex = 0.6)
split_data <- resample_partition(data = train, c(split_test = 0.3, split_train = 0.7))
fit2 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Child, data = split_data$split_train)
mae(model = fit2, data = split_data$split_test)
get_mae <- function(maxdepth, target, predictors, train_data, test_data) {
    predictors <- paste(predictors, collapse = "+")
    formula <- as.formula(paste(target, "~", predictors, sep = ""))
    model <- rpart(formula, data = train_data, control = rpart.control(maxdepth = maxdepth))
    mae <- mae(model, test_data)
    return(mae)
}
target <- "Survived"
predictors <- c("Pclass", "Sex", "SibSp", "Parch", "Fare", "Embarked", "Child")
for (i in 1:10) {
    mae <- get_mae(maxdepth = i, target = target, predictors = predictors, train_data = split_data$split_train, test_data = split_data$split_test)
    print(glue::glue("Maxdepth: ", i, "\t MAE: ", mae))
}
set.seed(450)
fit3 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Child, data = train)
prediction <- predict(fit3, test)
pred <- as.numeric(prediction)
col <- ifelse(pred > 0.5, 1, 0)
submit <- data.frame(PassengerId = test$PassengerId, Survived = col)
write_csv(submit, "submission.csv")
