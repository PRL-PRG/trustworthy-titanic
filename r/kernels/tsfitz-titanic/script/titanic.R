library(caret)

data = read.csv('../input/train.csv', header=T, sep=',')
test = read.csv('../input/test.csv', header=T, sep=',')
summary(data)
table(data$Sex, data$Survived)
data$Child=0
data$Child[data$Age<18]= 1
summary(data)
table(data$Child, data$Survived)

#InTrain= createDataPartition(y=data$Survived, p=0.7, list=FALSE)
#train= data[InTrain,]
#test= data[-InTrain,]
#train_X= train[, c("Pclass", "SibSp", "Fare")]
#test_X=   test[, c("Pclass", "SibSp", "Fare")]
#train_y= train[, c("Survived")]
#test_y=   test[, c("Survived")]
#library(class)
#knn_pred = knn(train= train_X, test=test_X, cl=train_y, k=3)
#library(gmodels)
#CrossTable(x=test_y, y=knn_pred, prop.chisq= FALSE)

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

fit= rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked, data=data, method="class")
fancyRpartPlot(fit)
summary(fit)

pred = predict(fit, test, type = "class")
head(pred)

PassengerId = test$PassengerId
Survived = pred

solution <- data.frame(PassengerId, Survived)

head(solution)

write.csv(solution, file = "gender_submission.csv",row.names=F)