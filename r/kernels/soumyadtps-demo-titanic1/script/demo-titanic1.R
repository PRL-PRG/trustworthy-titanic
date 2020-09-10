
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest


# The train and test data is stored in the ../input directory

# We can inspect the train data. The results of this are printed in the log tab below


# Here we will plot the passenger survival by class
library(rpart)
train_data <- read.csv("../input/train.csv")
test_data <- read.csv("../input/test.csv")

train_edited_dataframe <- subset(train_data, !(is.na(train_data$Age)> 0)) #filters blank age rows
test_edited_dataframe <- subset(test_data, !(is.na(test_data$Age)> 0)) #filters blank age rows

head(train_edited_dataframe)
tree_model <- rpart(Survived ~ Age + Pclass + Sex, data = train_edited_dataframe, method = "class")
summary(tree_model)
#rpart.plot(tree_model)

predicted_value <- predict(tree_model, test_data, type = "class")
print(predicted_value)
summary(predicted_value)