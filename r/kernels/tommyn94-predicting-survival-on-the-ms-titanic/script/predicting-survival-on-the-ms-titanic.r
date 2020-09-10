
suppressMessages({
    library(caret)
    library(caretEnsemble)
    library(naniar)
    library(ggplot2)
    library(doParallel)
    options(repr.plot.width=7, repr.plot.height=3, warn = -1)
})
trainingFile = "../input/train.csv"
training = read.csv(trainingFile, na.strings = "")
dim(training)


head(training)

str(training)

training$Survived = factor(training$Survived, labels = c("no", "yes"))

miss_var_summary(training)

training$Embarked[is.na(training$Embarked)] = names(which.max(table(training$Embarked)))

with(training, table(Sex, Survived))

ggplot(training, aes(Age, color = Survived)) + geom_density() + facet_wrap(~ Sex)
ggplot(training, aes(Fare, color = Survived)) + geom_density() + facet_wrap(~ Sex)

ggplot(training, aes(Survived, SibSp)) + geom_count()
ggplot(training, aes(Survived, Parch)) + geom_count()

with(training, table(Embarked, Survived))
with(training, table(Pclass, Survived))
with(training, table(Embarked, Pclass))

ggplot(training, aes(Embarked, Fare)) + geom_boxplot()

with(training, table(Title, Survived))
with(training, table(Title, Sex))

cl = makePSOCKcluster(4)
registerDoParallel(cl)

p = dummyVars(~ Age + Embarked + Pclass + Sex + SibSp + Parch + Fare, training)
predictors = predict(p, training)
target = training$Survived

set.seed(418)

myFolds = createMultiFolds(training$Survived, 5, 3)
myControl = trainControl(method = "repeatedcv", number = 5, repeats = 3, index = myFolds, savePredictions = TRUE,
                        classProbs = TRUE, search = "random")
methods = c("ranger", "knn", "svmRadial", "glmnet", "xgbTree")
models = caretEnsemble::caretList(predictors, target, methodList = methods, trControl = myControl,
                                  preProcess = "knnImpute", tuneLength = 15)

bwplot(resamples(models))
summary(resamples(models))

cor(predict(models, predictors))

models2stack = models[c(1, 4)]
class(models2stack) = "caretList"
stackControl = trainControl("repeatedcv", 5, 3, search = "random", savePredictions = TRUE, classProbs = TRUE)
stack = caretEnsemble::caretStack(models2stack, method = "xgbTree", trControl = stackControl, tuneLength = 30)
stopCluster(cl)
