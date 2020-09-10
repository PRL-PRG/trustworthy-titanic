
options(echo=FALSE)
library(e1071)
library(caret)
library(tidyverse) 
list.files(path = "../input")

df_raw <- read.csv('../input/train.csv')
#head(df_raw)
str(df_raw)

df <- df_raw %>% select(PassengerId, Survived, Fare, Pclass, Age, Sex, SibSp, Parch) %>% 
    mutate(x1 = Fare / which.max(Fare),
           x2 = ifelse(Age <= 25, Age/25., 1.-(Age - 25)/Age),  #69%
           x3 = ifelse(Sex=="male", -1, 1), 
           x4 = log(1+SibSp) - log(1+Parch),
           x5 = Parch / which.max(Parch) )

#x1 = Fare / which.max(Fare)
#x1 = Fare * (1+Pclass) / which.max(Fare * (1+Pclass)
#x2 = log(abs(Age - 25) + 1)
#x2 = ifelse(Age <= 25, Age/25., 1.-(Age - 25)/Age)

#head(df)
#library(ggplot2)
#ggplot(df, aes(x=Age, y=x2)) + geom_point()

set.seed(42)
idx_train <- createDataPartition(df$PassengerId, p=0.5, list=FALSE, times=1)
#idx_train
#nrow(idx_train)
df_train <- df[idx_train,]
df_valid <- df[-idx_train,]
#head(df_train)

model <- svm(x = df_train %>% select(x1,x2,x3,x4,x5), 
             y = as.factor(df_train$Survived), 
             scale = FALSE,
             na.action = na.omit,
             type = "C-classification",
             kernel ="radial", 
             gamma = 1,
             cost = 50)
#summary(model)

yhat <- predict(model, df_valid %>% select(x1,x2,x3,x4,x5), na.action = na.exclude)
#length(yhat)

res <- confusionMatrix(as.factor(yhat), as.factor(df_valid$Survived))
#res

acc <- unname(res$overall[1])
acc

df_test <- read.csv('../input/test.csv')

df_test <- df_test %>% select(PassengerId, Fare, Pclass, Age, Sex, SibSp, Parch) %>% 
    mutate(x1 = Fare / which.max(Fare),
           x2 = ifelse(Age <= 25, Age/25., 1.-(Age - 25)/Age),  #69%
           x3 = ifelse(Sex=="male", -1, 1), 
           x4 = log(1+SibSp) - log(1+Parch),
           x5 = Parch / which.max(Parch) )

df_test[is.na(df_test)] <- 0
head(df_test)

yhat <- predict(model, df_test %>% select(x1,x2,x3,x4,x5))
#length(yhat)

res <- df_test %>% select(PassengerId) %>% mutate(Survived = yhat)
head(res)

write.csv(res, file = 'my_submission.csv', row.names = F)
