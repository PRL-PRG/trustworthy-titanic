# This was my first analysis and submission. The analysis produces 77.033% accuracy when run against the test data. 
# Script for KNN analysis of Titanic: Machine Learning from Disaster Kaggle competition

train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# Remove Name, Ticket and Cabin columns
train <- train[,-c(4,9,11)]
test <- test[,-c(3,8,10)]

# Change Sex to 0 = male, 1 = female
train$Sex <- sapply(as.character(train$Sex), switch, 'male' = 0, 'female' = 1)
test$Sex <- sapply(as.character(test$Sex), switch, 'male' = 0, 'female' = 1)

# Change Embarked column to 0 = 'C', 1 = 'Q', 2 = 'S' and remove NAs
train$Embarked[train$Embarked == ''] <- 'S'
train$Embarked <- sapply(as.character(train$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)

test$Embarked <- sapply(as.character(test$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)

# Remove NAs from Age and Fare columns
train_age <- na.omit(train$Age)
train_age_avg <- mean(train_age)
train$Age[is.na(train$Age)] <- train_age_avg

test_age <- na.omit(test$Age)
test_age_avg <- mean(test_age)
test$Age[is.na(test$Age)] <- test_age_avg

test_fare <- na.omit(test$Fare)
test_fare_avg <- mean(test_fare)
test$Fare[is.na(test$Fare)] <- test_fare_avg

# Change Age to 0 = Adult(>=18), 1 = Child(<18)
train$Age <- ifelse(train$Age<18, 1, 0)
test$Age <- ifelse(test$Age<18, 1, 0)

# Normalize Fare and Class columns
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

train$Pclass = normalize(train$Pclass)
test$Pclass = normalize(test$Pclass)

test_length <- length(test$Fare)
fare <- normalize(c(train$Fare, test$Fare))
train$Fare <- fare[1:(length(fare)-test_length)]
test$Fare <- fare[(length(fare)-test_length + 1): length(fare)]

# Remove and rename the Survived column from train
survived <- train$Survived
passengers <- test$PassengerId
train <- train[,-c(1,2,6,7,9)]
test <- test[,-c(1,5,6,8)]

# Run the KNN function
# The value of K was chosen by the analysis in kaggle_knn_train.R
knn_titanic <- class.knn(train, test, survived, k = 5,  l = 0, prob = FALSE, use.all = TRUE)
submission <- data.frame(PassengerId = passengers,Survived = knn_titanic)
write.csv(submission,'titanic_knn.csv')