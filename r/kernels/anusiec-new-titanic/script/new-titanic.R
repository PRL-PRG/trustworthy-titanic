
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(tree)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)

# Here we will plot the passenger survival by class

model.tree <- tree(Survived ~ Sex + Pclass + Embarked, data = train)
summary(model.tree)

#plot(train[, 3],iris[, 4],xlab="petal length", ylab="petal width")
#partition.tree(model.tree, add = TRUE, cex = 1.0)

plot(model.tree, type = "uniform")
text(model.tree, cex = 1.0)

prediction <- predict(model.tree, newdata = test)
prediction <- as.data.frame(prediction)
 
names(prediction) <- c("Survived")
prediction$Survived <- ifelse(prediction$Survived < 0.5, 0, 1)
  
prediction
 
test$Survived <- prediction$Survived
test
  
NewT<-test[,c(1,12)]
write.csv(NewT, file = 'Anu.csv', row.names = F)
