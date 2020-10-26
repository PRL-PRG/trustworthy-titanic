library("ggplot2")
library("caret")
library("mice")
library("dplyr")
train <- read.csv("../input/train.csv", na.strings = c("", "NA"))
test <- read.csv("../input/test.csv")
summary(train)
tapply(train$Survived, train$Pclass, mean)
msex <- matrix(c(length(train$Sex[train$Survived == "0" & train$Sex == "male"]), length(train$Sex[train$Survived == "1" & train$Sex == "male"]), length(train$Sex[train$Survived == "0" & train$Sex == "female"]), length(train$Sex[train$Survived == "1" & train$Sex == "female"])), nrow = 2, ncol = 2)
colnames(msex) <- c("male", "female")
rownames(msex) <- c("0", "1")
barplot(msex)
df.sexage <- data.frame(Sex = train$Sex, Age = train$Age, Survived = train$Survived)
list.sexage <- split(df.sexage, df.sexage[, c("Sex", "Survived")])
boxplot(list.sexage[[1]]$Age, list.sexage[[3]]$Age, list.sexage[[2]]$Age, list.sexage[[4]]$Age, ylab = "Age", names = names(list.sexage), col = topo.colors(4))
legend("topleft", legend = c("female - died", "female - survived", "male - died", "male - survived"), fill = topo.colors(4))
tapply(train$Survived, train$Embarked, mean)
plot(train$SibSp, col = as.factor(train$Survived), pch = 19)
plot(train$Parch, col = as.factor(train$Survived), pch = 19)
training <- subset(train, select = -c(PassengerId, Name, Ticket, Cabin))
training$Pclass <- as.factor(training$Pclass)
c(sum(is.na(train$Survived)), sum(is.na(train$Pclass)), sum(is.na(train$Sex)), sum(is.na(train$Age)), sum(is.na(train$SibSp)), sum(is.na(train$Parch)), sum(is.na(train$Fare)), sum(is.na(train$Embarked)))
nanEmb <- is.na(training$Embarked)
training <- training[!nanEmb, ]
training.t <- select(training, -(Survived))
preObj <- preProcess(training.t, method = "knnImpute")
age <- predict(preObj, training.t)$Age
age.test <- predict(preObj, test)$Age
fare <- predict(preObj, training.t)$Fare
fare.test <- predict(preObj, test)$Fare
training$Age <- age
training$Fare <- fare
test$Age <- age.test
test$Fare <- fare.test
dummies <- dummyVars(~., data = training)
training <- data.frame((predict(dummies, newdata = training)))
test$Pclass <- as.factor(test$Pclass)
dummies.test <- dummyVars(~., subset(test, select = -c(Name, Ticket, Cabin)))
testing <- data.frame((predict(dummies.test, newdata = subset(test, select = -c(Name, Ticket, Cabin)))))
inTrain <- createDataPartition(y = training$Survived, p = 0.7, list = FALSE)
trainingRF <- training[inTrain, ]
testingRF <- training[-inTrain, ]
rf_model <- train(factor(Survived) ~ ., data = trainingRF, method = "rf", prox = TRUE)
predRF <- predict(rf_model, testingRF)
table(predRF, testingRF$Survived)
df <- as.data.frame(table(predRF, testingRF$Survived))
df <- data.frame(Predicted = df$predRF, Actual = df$Var2, Freq = df$Freq)
ggplot(data = df, mapping = aes(x = df$Actual, y = df$Predicted)) + geom_tile(aes(fill = df$Freq), colour = "white") + geom_text(aes(label = sprintf("%0.2f", df$Freq)), vjust = 1) + scale_fill_gradient(low = "blue", high = "red") + theme_bw() + theme(legend.position = "none") + ggtitle("Random Forest- prediction [%]") + xlab("Actual") + ylab("Predicted")
importance <- varImp(rf_model$finalModel)
importance <- data.frame(Predictor = rownames(importance), Value = importance$Overall)
importance <- importance[order(importance$Value, decreasing = TRUE), ]
bp <- barplot(importance$Value, names.arg = importance$Predictor, ylab = "[%]")
text(x = bp, y = importance$Value, label = round(importance$Value), pos = 1, cex = 0.8)
predRF.test <- predict(rf_model, testing)
prediction <- data.frame(PassengerID = test$PassengerId, Survived = predRF.test)
write.csv(prediction, file = "prediction_test.csv", row.names = F)
