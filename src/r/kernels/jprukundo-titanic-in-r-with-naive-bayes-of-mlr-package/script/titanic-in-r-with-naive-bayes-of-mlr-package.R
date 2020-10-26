library(mlr)
train = read.csv("../input/train.csv", header = TRUE)
test = read.csv("../input/test.csv", header = TRUE)
features = c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")
featuresTarget = c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Survived")
train_data = train[, featuresTarget]
test_data = test[, features]
PassengerId = test[, "PassengerId"]
task = makeClassifTask(data = train_data, target = "Survived")
lrn = makeLearner("classif.naiveBayes")
model = train(lrn, task)
pred = as.data.frame(predict(model, newdata = test_data))
colnames(pred) = c("Survived")
PassengerIdPred = cbind(PassengerId, pred)
write.csv(PassengerIdPred, file = "PassengerId Survived.csv", row.names = FALSE)
head(PassengerIdPred)
