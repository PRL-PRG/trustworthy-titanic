options(echo = FALSE)
library(e1071)
library(caret)
library(tidyverse)
list.files(path = "../input")
df <- read.csv("../input/train.csv")
set.seed(42)
idx_train <- createDataPartition(df$PassengerId, p = 0.5, list = FALSE, times = 1)
df_train <- df[idx_train, ]
df_valid <- df[-idx_train, ]
model <- svm(x = df_train$Fare, y = as.factor(df_train$Survived), scale = FALSE, na.action = na.omit, type = "nu-classification", kernel = "radial", gamma = 100, nu = 0.7)
yhat <- predict(model, df_valid$Fare)
res <- confusionMatrix(as.factor(yhat), as.factor(df_valid$Survived))
acc <- unname(res$overall[1])
acc
df_test <- read.csv("../input/test.csv")
df_test$Fare[is.na(df_test$Fare)] <- 0
yhat <- predict(model, df_test$Fare)
res <- df_test %>% select(PassengerId) %>% mutate(Survived = yhat)
head(res)
write.csv(res, file = "my_submission.csv", row.names = F)
