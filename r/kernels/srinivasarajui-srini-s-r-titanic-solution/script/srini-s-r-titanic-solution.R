# Import the training set: train
train_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/train.csv"
train <- read.csv(train_url)
  
# Import the testing set: test
test_url <- "http://s3.amazonaws.com/assets.datacamp.com/course/Kaggle/test.csv"
test <- read.csv(test_url)
  
# Print train and test to the console
train
test

# Your train and test set are still loaded
str(train)
str(test)

# Survival rates in absolute numbers
 table(train$Survived) 

# Survival rates in proportions
prop.table(table(train$Survived))
  
# Two-way comparison: Sex and Survived
table(train$Sex, train$Survived)

# Two-way comparison: row-wise proportions
prop.table(table(train$Sex, train$Survived),margin =1)

# Two-way comparison: column-wise proportions
prop.table(table(train$Sex, train$Survived),margin =2)


# Your train and test set are still loaded in
str(train)
str(test)

# Create the column child, and indicate whether child or no child
train$Child <- NA
train$Child[train$Age < 18] <- "Child"
train$Child[train$Age >= 18] <- "Adult"

# Two-way comparison
prop.table(table(train$Child, train$Survived),margin =1)


# Your train and test set are still loaded in
str(train)
str(test)

# Copy of test
test_one <- test

# Initialize a Survived column to 0
test_one$Survived <- 0

# Set Survived to 1 if Sex equals "female"
test_one$Survived[test_one$Sex=="female"] <- 1

# Load in the R package  
library(rpart)


# Your train and test set are still loaded in
str(train)
str(test)

# Build the decision tree
my_tree_two <- rpart(Survived  ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to build a fancy plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)


# my_tree_two and test are available in the workspace

# Make predictions on the test set
my_prediction <- predict(my_tree_two, test, type = "class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)

# Finish the write.csv() call
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)


# Your train and test set are still loaded in

# Change this command
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
                     data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
  
# Visualize my_tree_three
fancyRpartPlot(my_tree_three)


# train and test are available

# Create train_two
train_two <- train
train_two$family_size <- train_two$SibSp + train_two$Parch + 1

# Finish the command
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size, 
                      data = train_two, method = "class")

# Visualize your new decision tree
fancyRpartPlot(my_tree_four)

# train_new and test_new are available in the workspace

# Finish the command
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title , 
                      data = train_new, method = "class")

# Visualize my_tree_five
fancyRpartPlot(my_tree_five)

# Make prediction
my_prediction <- predict(my_tree_five, test_new, type = "class")

# Make results ready for submission
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

# All data, both training and test set
all_data

# Passenger on row 62 and 830 do not have a value for embarkment.
# Since many passengers embarked at Southampton, we give them the value S.
all_data$Embarked[c(62, 830)] <- "S"

# Factorize embarkment codes.
all_data$Embarked <- factor(all_data$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model.
# This time you give method = "anova" since you are predicting a continuous variable.
library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Split the data back into a train set and a test set
train <- all_data[1:891,]
test <- all_data[892:1309,]


# train and test are available in the workspace
str(train)
str(test)

# Load in the package
library(randomForest)

# Train set and test set
str(train)
str(test)

# Set seed for reproducibility
set.seed(111)

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title,
                          data = train, importance = TRUE, ntree = 1000)

# Make your prediction using the test set
my_prediction <- predict(my_forest, test)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution2.csv", row.names = FALSE)