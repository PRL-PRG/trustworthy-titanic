library(dplyr); library(ggplot2)

#loading the training and test data

train <- read.csv('../input/train.csv')
test <- read.csv('../input/test.csv')

# comparing training and test sets

colnames(train); colnames(test)

# the simplest possible model

train %>% filter(Survived==0) %>% summarize(length(Survived)/891)


#gender model

gender_model <- lm(Survived~Sex, data=train)
summary(gender_model)

train$gender_model_pred <- predict(gender_model, train)
train$gender_model_pred <- round(train$gender_model_pred)
                                                                                  
train %>% filter(Survived==gender_model_pred) %>% summarize(length(Survived)/891)

#submitting to kaggle                                                                         

test$Survived <- round(predict(gender_model, test))
submit <- test %>% select(PassengerId, Survived)
write.csv(submit, file = "gender_model.csv", row.names = FALSE) 
                                                              
#https://www.kaggle.com/c/titanic
                                                                                  
# residuals

ggplot(train, aes(gender_model_pred, Survived))+geom_jitter()

ggplot(train, aes(gender_model_pred, Survived-gender_model_pred))+geom_jitter()
                                                                                  
#Passenger class

ggplot(train, aes(Pclass, Survived-gender_model_pred))+geom_jitter()
gender_pclass_model <- lm(Survived~Sex+Pclass, data=train)
train$gender_pclass_model_pred <- round(predict(gender_pclass_model, train))
train %>% filter(gender_pclass_model_pred != gender_model_pred)
