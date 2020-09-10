library(caret)
library(randomForest)

trainSet <- read.table("../input/train.csv", sep=",", header=TRUE)
testSet <- read.table("../input/test.csv", sep=",", header=TRUE)
trainSet$Survived <- factor(trainSet$Survived)
set.seed(42)

# Convert Survived to Factor
trainSet$Survived <- factor(trainSet$Survived)
# Set a random seed (so you will get the same results as me)
set.seed(42)

# Train the model using a "random forest" algorithm
model <- train(Survived ~ Pclass + Sex + SibSp +
            Embarked + Parch + Fare, # Survived is a function of the variables we decided to include
            data = trainSet, # Use the trainSet dataframe as the training data
            method = "rf",# Use the "random forest" algorithm
            trControl = trainControl(method = "cv", # Use cross-validation
                                    number = 5)) # Use 5 folds for cross-validation


testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)

testSet$Survived <-  predict(model, newdata = testSet)

submission <- testSet[c("PassengerId", "Survived")]
write.table(submission, file="submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")
