#Titanic - Isaac CART decision tree classifier

#import library
library(rpart)

#import training data
train.data <- read.csv("../input/train.csv", header = T)

#show training data
str(train.data)

#build decision tree classifier
classifier <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                    data=train.data,
                    method="class")

#show decision tree classifier summary
summary(classifier)

#visually show decision tree classifier
plot(classifier)
text(classifier)

#make prediction on training data (test the model)
classifier.predict <- predict(classifier, train.data, type="class")

#show confusion matrix for error rate
table(classifier.predict, train.data$Survived)

#error rate can be seen as (28 + 115) / (521 + 28 + 115 + 227) = 16.04% for the training set, accuracy of 83.96%

#import test data
test.data <- read.csv("../input/test.csv", header = T)

#use classifier on test data
classifier.predict <- predict(classifier, test.data, type="class")

#save output into CSV file
write.csv(classifier.predict, file = "titanicCARTSolution.csv", row.names = FALSE)