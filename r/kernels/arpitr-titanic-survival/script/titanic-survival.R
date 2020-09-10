
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
#library(randomForest)

# The train and test data is stored in the ../input directory
train_data <- read.csv("../input/train.csv")
test_data  <- read.csv("../input/test.csv")

#Exploring the data
str(train_data)
head(train_data)
tail(train_data)

#Age column have some missing values
summary(train_data$Age)

#Imputing the missing values from Age columns as replace them with mean
train_data$Age[is.na(train_data$Age)] <- mean(train_data$Age, na.rm = TRUE)

test_data$Age[is.na(test_data$Age)] <- mean(test_data$Age, na.rm = TRUE)
test_data$Fare[is.na(test_data$Fare)] <- mean(test_data$Fare, na.rm = TRUE)
#Survived column is integer class type
class(train_data$Survived)
levels(as.factor(train_data$Survived))

#Converting it to factor with yes and no level
head(train_data$Survived)
train_data$Survived <- ifelse(train_data$Survived == 1, "yes", "no")
train_data$Survived <- as.factor(train_data$Survived)
head(train_data$Survived)
class(train_data$Survived)

library(rpart)

table(as.factor(train_data$Survived))
train_data$Survived <- as.factor(train_data$Survived)
str(train_data$Survived)

#Identity columns like passenger id, name, cabin ignored for predictor variables
tree <- rpart(formula = Survived ~ Sex+Age+SibSp+Parch+Fare+Embarked,
              data = train_data,
              method = "class")

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(tree)

test_data$Survived <- 0

Prediction <- predict(tree, test_data, type="class")
prop.table(table(Prediction))

test_data$Survived <- Prediction

survived <- ifelse(test_data$Survived == "no", 0,1)
submit <- data.frame(PassengerId = test_data$PassengerId, Survived = survived)
write.csv(submit, file = "arpit_titanic.csv", row.names = FALSE)

#Accuracy of the model
library(caret)
confusionMatrix(Prediction, test_data$Survived)

#submit <- data.frame(PassengerId = test_data$PassengerId, Survived = test_data$Survived)
#write.csv(submit, file = "train_survival.csv", row.names = FALSE)

#confusionMatrix(Prediction, test_data$Survived, positive = levels(test_data$Survived)[2])

#library(randomForest)
#randomForest(formula = Survived ~ Sex+Age+SibSp+Parch+Fare+Embarked,
#             data = train_data)
