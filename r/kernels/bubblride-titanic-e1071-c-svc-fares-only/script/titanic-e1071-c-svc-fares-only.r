
options(echo=FALSE)
library(e1071)
library(caret)
library(tidyverse) 
list.files(path = "../input")

df <- read.csv('../input/train.csv')

#head(df)
str(df)

print('No Feature Engineering :(')

set.seed(42)
idx_train <- createDataPartition(df$PassengerId, p=0.5, list=FALSE, times=1)
#idx_train
#nrow(idx_train)
df_train <- df[idx_train,]
df_valid <- df[-idx_train,]
#head(df_train)

model <- svm(x = df_train$Fare, 
             y = as.factor(df_train$Survived), 
             scale = FALSE,
             na.action = na.omit,
             type = "C-classification",
             kernel ="radial", 
             gamma = 2,
             cost=10)

summary(model)

yhat <- predict(model, df_valid$Fare)
#length(yhat)

res <- confusionMatrix(as.factor(yhat), as.factor(df_valid$Survived))
acc <- unname(res$overall[1])
res

df_test <- read.csv('../input/test.csv')
#nrow(df_test)
df_test$Fare[ is.na(df_test$Fare) ] <- 0
#df_test$Fare

yhat <- predict(model, df_test$Fare)
#length(yhat)

res <- df_test %>% select(PassengerId) %>% mutate(Survived = yhat)
head(res)

write.csv(res, file = 'my_submission.csv', row.names = F)
