library(ggplot2)
library(readr)
library(rpart)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
str(train)
str(test)
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")
my_tree_three <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class", control = rpart.control(minsplit = 50, cp = 0))
plot(my_tree_three)
text(my_tree_three)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(my_tree_three)
my_prediction <- predict(my_tree_three, newdata = test, type = "class")
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)
nrow(my_solution)
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
