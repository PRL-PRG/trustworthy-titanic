library(rpart)
# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 10] <- 1
train$Child[train$Age >= 10] <- 0
test$Child <- NA
test$Child[test$Age < 10] <- 1
test$Child[test$Age >= 10] <- 0

#Create famili_size column
train$family_size <-NA
train$family_size <- train$SibSp + train$Parch + 1
test$family_size <-NA
test$family_size <- test$SibSp + test$Parch + 1

# Passenger on row 62 does not have a value for embarkment.
# Since many passengers embarked at Southampton, we give them the value S.
train$Embarked[c(62, 891)] <- "S"
test$Embarked[c(62, 418)] <- "S"
# Factorize embarkment codes.
train$Embarked <- factor(train$Embarked)
test$Embarked <- factor(test$Embarked)

# Create the tree
my_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size,
                      data = train, method = "class", control = rpart.control(minsplit =50, cp = 0))

# Make prediction
my_prediction <- predict(my_tree, test, type = "class")

# Make results ready for submission
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)