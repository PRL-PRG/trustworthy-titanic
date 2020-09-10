
# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# Massaging the code


#Putting in the variables that cause the least problems
train<-train[, c("Survived","Fare","SibSp", "Parch", "Sex")]

test<-test[, c("Fare","SibSp","PassengerId", "Parch", "Sex")]

test$Fare<-as.numeric(test$Fare)

train$Fare<-as.numeric(train$Fare)

summary(train)
summary(test)

#Need the Survived column to be a factor

train$Survived<-as.factor(train$Survived)

test[is.na(test)]<-0
train[is.na(train)]<-0

#Create the random forest

train.rf<-randomForest(Survived ~ Fare+SibSp+Parch+Sex, data=train, type = "response")

#Run the prediction

train.prediction<-predict(train.rf, test)

train.prediction<-as.numeric(levels(train.prediction))[train.prediction]

train.prediction[is.na(train.prediction)]<-0

#Bugshooting stuff

typeof(train.prediction)

summary(train.prediction)

str(train.prediction)

#Putting the output in the correct form

output<-data.frame(test$PassengerId,train.prediction)

colnames(output)<-cbind("PassengerId","Survived")

write.csv(output, file = 'Rushton_Solution.csv', row.names = F)
