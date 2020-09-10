
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")


# Here we will plot the passenger survival by class
train$Survived <- factor(train$Survived, levels=c(1,0))
train$Pclass <- as.factor(train$Pclass)
test$Pclass <- factor(test$Pclass)

train2 <- train[1:623,]
cv <- train[624:nrow(train),]


model <- randomForest((Survived) ~ Pclass + Sex + Embarked, data=train)

output <- predict(model,cv)

match <- output[output[1]==cv$Survived]
print(length(match))
print(nrow(cv))
