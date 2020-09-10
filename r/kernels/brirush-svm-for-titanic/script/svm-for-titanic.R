
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(e1071)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

train[is.na(train)] <-0
test[is.na(test)]  <-0

# We can inspect the train data. The results of this are printed in the log tab below
str(train)

str(test)

SVMmodel<-svm(as.factor(Survived) ~ Age+SibSp+Parch+Sex+Fare, data = train, cost = 100, gamma = 1)

prediction<-predict(SVMmodel, test)

output<-data.frame(test$PassengerId, prediction)

colnames(output)=cbind("PassengerId","Survived")

write.csv(output, file = 'Rushton_Solution.csv', row.names = F)