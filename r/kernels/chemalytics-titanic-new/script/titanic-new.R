
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(tree)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
#summary(train)

model.tree <- tree(Survived ~ Sex + Pclass, data = train)
summary(model.tree)

plot(model.tree, type = "uniform")
text(model.tree, cex = 1.0)


prediction <- predict(model.tree, newdata = test)
prediction <- as.data.frame(prediction)
names(prediction) <- c("Survived")
prediction$PassengerId <- test$PassengerId

prediction$Survived <- ifelse(prediction$Survived < 0.5, 0, 1)

#prediction <- subset(prediction, select = c("PassengerId", "Survived")

#prediction
#test$Survived <- prediction$Survived
#test
#PassengerId and Survived

#submit <- subset(test, select = "id")
write.csv(prediction, file = 'rf_mod_Solution.csv', row.names = F)
