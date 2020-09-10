library(h2o)

## Creating an instance of H20 serveur
h2o.init()

## Loading data
train_data= read.csv("../input/train.csv")
test_data= read.csv("../input/test.csv")

## Selection and factorization of Variable
colSums(is.na(train_data))

train_data$Survived = as.factor(train_data$Survived)
train_data$Pclass = as.factor(train_data$Pclass)
train_data$Sex = as.factor(train_data$Sex)
train_data$Embarked = as.factor(train_data$Embarked)
train_data$Age = as.factor(train_data$Age)


test_data$Pclass = as.factor(test_data$Pclass)
test_data$Sex = as.factor(test_data$Sex)
test_data$Embarked = as.factor(test_data$Embarked)
test_data$Age = as.factor(test_data$Age)


train_data$PassengerId=NULL

## Transform in H2O object
train_d = as.h2o(train_data)
test_d = as.h2o(test_data)

## Modelization
aml <- h2o.automl(y = "Survived", training_frame = train_d, max_runtime_secs = 1500,stopping_metric = "misclassification")


aml@leaderboard
aml@leader


### prediction
pred = h2o.predict(aml@leader,test_d)
head(pred)

pred = as.data.frame(pred)

### SOLUTION ###
solution <- data.frame(PassengerID = test_data$PassengerId, Survived = pred$predict)
write.csv(solution, file = 'submission.csv', row.names = F)