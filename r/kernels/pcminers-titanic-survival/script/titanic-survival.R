# The train and test data is stored in the ../input directory
library(rpart)

train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# Create train_two
train_two <- train
train_two$family_size <- train$SibSp + train$Parch + 1

# Finish the command
my_tree_four <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + family_size,
                      data = train_two, method = "class")

# Visualize your new decision tree
plot(my_tree_four)



