
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(dplyr)
library(corrplot)
library(data.table)
library(rpart)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
setDT(train)
train$IsTrain <- TRUE

test  <- read.csv("../input/test.csv")
setDT(test)
test$IsTrain <- FALSE
test$Survived <- NA

# Clean and then categorically cast
full <- rbind(train, test)
head(full$Fare)
full$Fare[which(is.na(full$Fare))] <- 
full$Pclass <- as.factor(full$Pclass)
full$Embarked <- as.factor(full$Embarked)
full$Sex <- as.factor(full$Sex)

train <- full[full$IsTrain == TRUE,]
test <- full[full$IsTrain == FALSE,]

train$Survived <- as.factor(train$Survived)

# Since age has some missing data, predict passenger age based on other factors
predicted_age_train <- rpart(Age ~ Survived + Sex + SibSp + Pclass + Parch + Ticket + Fare + Embarked,
                       data = train[!is.na(train$Age),], method = "anova")
train$Age[is.na(train$Age)] <- predict(predicted_age_train, train[is.na(train$Age),])
predicted_age_test <- rpart(Age ~ Sex + SibSp + Pclass + Parch + Ticket + Fare + Embarked,
                       data = test[!is.na(test$Age),], method = "anova")
test$Age[is.na(test$Age)] <- predict(predicted_age_test, test[is.na(test$Age),])

# Engineer Nannied feature?
model <- randomForest(Survived ~ Age + Sex + SibSp + Pclass + Parch + Fare + Embarked, 
                        data = train, importance = TRUE, ntree = 300, set.seed(111))

predictions <- predict(model, test)

colSums(sapply(test, is.na))

# Record results
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = predictions)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)