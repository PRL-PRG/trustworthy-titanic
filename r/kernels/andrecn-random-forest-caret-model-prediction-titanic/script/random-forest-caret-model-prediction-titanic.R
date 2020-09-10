# machine learning - Titanic Dataset
library('ggplot2')
library('caret')
library('mice')
library('dplyr')

train <- read.csv("../input/train.csv", na.strings = c("", "NA"))
test <- read.csv("../input/test.csv")

# exploring data
summary(train)
# 60% of first class pass. have survived, and third class only a quarter
tapply(train$Survived,train$Pclass,mean)
# sex variable is a strong predictor, due the difference between male/female survivors
msex <- matrix(c(length(train$Sex[train$Survived=="0" & train$Sex == "male"]),
                 length(train$Sex[train$Survived=="1" & train$Sex == "male"]),
                 length(train$Sex[train$Survived=="0" & train$Sex == "female"]),
                 length(train$Sex[train$Survived=="1" & train$Sex == "female"])), nrow = 2, ncol =2)
colnames(msex) <- c("male","female")
rownames(msex) <- c("0", "1")
barplot(msex)
# apparently column Age, even when combined with Sex, seems not to be a strong predictor, as thought
df.sexage <- data.frame(Sex = train$Sex, Age = train$Age, Survived = train$Survived)
list.sexage <- split(df.sexage,df.sexage[, c("Sex","Survived")])
boxplot(list.sexage[[1]]$Age, list.sexage[[3]]$Age, list.sexage[[2]]$Age, list.sexage[[4]]$Age, ylab = "Age", names = names(list.sexage), col = topo.colors(4))
legend("topleft", legend = c("female - died","female - survived","male - died","male - survived" ), fill = topo.colors(4))
# Embarked variable equals "Q" and "C" have 33~38% of survived = 1, and Embarked equals "C" almost half.
tapply(train$Survived,train$Embarked,mean)
# SibSp and Parch variables seem to predict better if values are grater than 2/3
plot(train$SibSp, col = as.factor(train$Survived), pch = 19)
plot(train$Parch, col = as.factor(train$Survived), pch = 19)

#removing non predictors
training <- subset(train, select = -c(PassengerId, Name, Ticket, Cabin))
training$Pclass <- as.factor(training$Pclass)

# dealing with nanvalues
c(sum(is.na(train$Survived)),sum(is.na(train$Pclass)),sum(is.na(train$Sex)),sum(is.na(train$Age)),
  sum(is.na(train$SibSp)),sum(is.na(train$Parch)),sum(is.na(train$Fare)),sum(is.na(train$Embarked)))
# Apparently only Age and Embarked variables have NaN values
# Age has significant NaN, Embarked only 2 rows
nanEmb <- is.na(training$Embarked)
training <- training[!nanEmb ,]

# Imputing missing Age/Fare values (test and train data)
# This code algo standardizes the values of Age and Fare
training.t <- select(training, -(Survived))
preObj <- preProcess(training.t, method = "knnImpute")
age <- predict(preObj, training.t)$Age
age.test <- predict(preObj, test)$Age
fare <- predict(preObj, training.t)$Fare
fare.test <- predict(preObj, test)$Fare
# plot(density(age, na.rm = TRUE), main = "Density for age values before/after imputation [train]")
# lines(density(training$Age, na.rm = TRUE), col = "blue")
# legend("topright", legend = c("Imputed data","Original data"), fill = c("black","blue"))
# plot(density(age.test, na.rm = TRUE), main = "Density for age values before/after imputation [test]")
# lines(density(test$Age, na.rm = TRUE), col = "blue")
# legend("topright", legend = c("Imputed data","Original data"), fill = c("black","blue"))
training$Age <- age
training$Fare <- fare
test$Age <- age.test
test$Fare <- fare.test

#making some dummies for test and training data
dummies <- dummyVars( ~., data=training)
training <- data.frame((predict(dummies, newdata = training)))
test$Pclass <- as.factor(test$Pclass)
dummies.test <- dummyVars( ~., subset(test, select = -c(Name, Ticket, Cabin)))
testing <- data.frame((predict(dummies.test, newdata = subset(test, select = -c(Name, Ticket, Cabin)))))

# Random Forest Model (caret)
inTrain <- createDataPartition(y=training$Survived, p=0.7, list = FALSE)
trainingRF <- training[inTrain,]
testingRF <- training[-inTrain,]

rf_model <- train(factor(Survived) ~., data = trainingRF, method = "rf", prox = TRUE)
predRF<-predict(rf_model, testingRF)
table(predRF,testingRF$Survived)

df<-as.data.frame(table(predRF,testingRF$Survived))
df<-data.frame(Predicted = df$predRF, Actual = df$Var2, Freq = df$Freq)

# Plotting confusion matrix
ggplot(data =  df, mapping = aes(x = df$Actual, y = df$Predicted)) +
    geom_tile(aes(fill = df$Freq), colour = "white") +
    geom_text(aes(label = sprintf("%0.2f", df$Freq)), vjust = 1) +
    scale_fill_gradient(low = "blue", high = "red") +
    theme_bw() + theme(legend.position = "none") +
    ggtitle("Random Forest- prediction [%]") + xlab("Actual") + ylab("Predicted")

# Plotting importance of predictors
importance <- varImp(rf_model$finalModel)
importance <- data.frame(Predictor = rownames(importance), Value = importance$Overall)
importance <- importance[order(importance$Value, decreasing = TRUE),]
bp <- barplot(importance$Value, names.arg = importance$Predictor, ylab = "[%]")
text(x = bp, y = importance$Value, label = round(importance$Value), pos = 1, cex = 0.8)

# prediction for test matrix
predRF.test<-predict(rf_model, testing)

# saving prediction data.frame
prediction <- data.frame(PassengerID = test$PassengerId, Survived = predRF.test)
write.csv(prediction, file = 'prediction_test.csv', row.names = F)

# observation:
# doing the same preprocess but changing the model, GBM 
# GBM_model <- train(factor(Survived) ~., data = trainingGBM, method = "gbm")
# the result is, practically the same, ~76% score
# after standardizing Age and Fare predictors, score increased to 77$
# after doing some dummies, score remains pratically the same
