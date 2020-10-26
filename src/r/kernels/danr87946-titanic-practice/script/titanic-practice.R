library(randomForest)
library(rpart)
library("dplyr")
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)
tree <- rpart(Survived ~ Pclass + Age + Sex + Embarked + SibSp + Parch + Fare, data = train, method = "class")
pred <- predict(tree, test, type = "class")
PassengerId <- test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- pred
write.csv(output.df, file = "out.csv", row.names = FALSE)
