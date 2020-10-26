suppressMessages(library(data.table))
suppressMessages(library(tidyverse))
suppressMessages(library(caret))
train <- fread("../input/train.csv")
test <- fread("../input/test.csv")
names(train)
names(test)
head(train)
head(test)
head(train)
options(repr.plot.width = 8, repr.plot.height = 6)
options(warn = -1)
ggplot(data = melt(train), mapping = aes(x = value)) + geom_histogram(bins = 20) + facet_wrap(~variable, scales = "free_x")
train[, .N, by = Embarked][order(-N)]
train[, .N, by = SibSp][order(-N)]
options(repr.plot.width = 4, repr.plot.height = 3)
train[, .N, by = c("Survived", "Sex")][order(Survived, -N)]
ggplot(data = train, aes(x = Survived, fill = Sex)) + geom_bar()
options(repr.plot.width = 4, repr.plot.height = 3)
train[, .N, by = c("Survived", "Pclass")][order(Survived, -N)]
ggplot(data = train, aes(x = Survived, fill = as.character(Pclass))) + geom_bar()
ggplot(train, aes(x = Age)) + geom_density(aes(group = Survived, color = Survived))
ggplot(train, aes(x = Pclass)) + geom_density(aes(group = Survived, color = Survived))
summary(test$Age)
summary(train$Age)
ggplot(train, aes(x = Age)) + geom_histogram(binwidth = 5, aes(group = Survived, color = Survived))
addcolumns <- function(dt) {
    dt[Embarked == "", `:=`(Embarked, "S")]
    dt[, `:=`(Age_categories_missing, ifelse(is.na(Age), 1, 0))]
    dt[, `:=`(Age_categories_infant, ifelse(Age_categories_missing == 0 & (Age > 0 & Age < 5), 1, 0))]
    dt[, `:=`(Age_categories_child, ifelse(Age_categories_missing == 0 & (Age >= 5 & Age <= 12), 1, 0))]
    dt[, `:=`(Age_categories_teenager, ifelse(Age_categories_missing == 0 & (Age >= 12 & Age <= 18), 1, 0))]
    dt[, `:=`(Age_categories_young_adult, ifelse(Age_categories_missing == 0 & (Age >= 18 & Age <= 35), 1, 0))]
    dt[, `:=`(Age_categories_adult, ifelse(Age_categories_missing == 0 & (Age >= 35 & Age <= 60), 1, 0))]
    dt[, `:=`(Age_categories_senior, ifelse(Age_categories_missing == 0 & (Age >= 60 & Age <= 100), 1, 0))]
    dt[, `:=`(Sex_male, ifelse(Sex == "male", 1, 0))]
    dt[, `:=`(Sex_female, ifelse(Sex == "female", 1, 0))]
    dmy <- dummyVars(" ~ .", data = dt[, c("Embarked")])
    trsf <- data.frame(predict(dmy, newdata = dt))
    dt <- cbind(dt, trsf)
    for (i in 1:3) {
        col <- paste("Pclass_", i, sep = "")
        dt[, `:=`(as.character(col), ifelse(Pclass == i, 1, 0))]
    }
    return(dt)
}
train <- addcolumns(train)
test <- addcolumns(test)
ncol(train)
ncol(test)
names(train)
names(train[, 13:27])
names(test[, 12:26])
dataset <- train[, c(2, 13:27)]
validation_index <- createDataPartition(dataset$Survived, p = 0.8, list = FALSE)
validation <- dataset[-validation_index, ]
dataset <- dataset[validation_index, ]
dim(dataset)
dataset$Survived <- factor(dataset$Survived)
dataset[, `:=`(Survived, ifelse(Survived == 1, "YES", "NO"))]
validation[, `:=`(Survived, ifelse(Survived == 1, "YES", "NO"))]
names(getModelInfo())
getModelInfo()$gbm$type
control <- trainControl(method = "cv", number = 5, classProbs = TRUE)
metric <- "Accuracy"
set.seed(123)
modelXgbTree <- train(Survived ~ ., data = dataset, method = "xgbTree", trControl = control, metric = metric)
set.seed(123)
modelXgbLinear <- train(Survived ~ ., data = dataset, method = "xgbLinear", trControl = control, metric = metric)
set.seed(123)
modelLda <- train(Survived ~ ., data = dataset, method = "lda", trControl = control, metric = metric)
set.seed(123)
modelLogitBoost <- train(Survived ~ ., data = dataset, method = "LogitBoost", trControl = control, metric = metric)
set.seed(123)
modelavNNet <- train(Survived ~ ., data = dataset, method = "avNNet", trControl = control, metric = metric)
set.seed(123)
modelGbm <- train(Survived ~ ., data = dataset, method = "gbm", trControl = control, metric = metric)
set.seed(123)
modelRf <- train(Survived ~ ., data = dataset, method = "rf", trControl = control, metric = metric)
set.seed(123)
modelKnn <- train(Survived ~ ., data = dataset, method = "knn", trControl = control, metric = metric)
results <- resamples(list(RF = modelRf, GBM = modelGbm, LDA = modelLda, KNN = modelKnn, LB = modelLogitBoost, ANN = modelavNNet, XGBT = modelXgbTree, XGBL = modelXgbLinear))
summary(results)
bwplot(results)
dotplot(results)
lda.confusion <- predict(modelLda, newdata = validation)
confusionMatrix(data = lda.confusion, as.factor(validation$Survived))
rf.confusion <- predict(modelRf, newdata = validation)
confusionMatrix(data = rf.confusion, as.factor(validation$Survived))
gbm.confusion <- predict(modelGbm, newdata = validation)
confusionMatrix(data = gbm.confusion, as.factor(validation$Survived))
knn.confusion <- predict(modelKnn, newdata = validation)
confusionMatrix(data = knn.confusion, as.factor(validation$Survived))
print("modelavNNet")
ann.confusion <- predict(modelavNNet, newdata = validation)
confusionMatrix(data = ann.confusion, as.factor(validation$Survived))
print("modelXgbLinear")
xgbl.confusion <- predict(modelXgbLinear, newdata = validation)
confusionMatrix(data = xgbl.confusion, as.factor(validation$Survived))
print("modelXgbTree")
xgbt.confusion <- predict(modelXgbTree, newdata = validation)
confusionMatrix(data = xgbt.confusion, as.factor(validation$Survived))
test.set <- test[, c(12:26)]
nrow(test.set)
names(test.set)
testResult <- predict(modelXgbTree, newdata = test.set)
end.result <- data.frame(test$PassengerId, data.frame(testResult))
end.result$testResult <- ifelse(end.result$testResult == "YES", 1, 0)
names(end.result) <- c("PassengerId", "Survived")
end.result
write.csv(end.result, file = "submission.csv", row.names = FALSE)
