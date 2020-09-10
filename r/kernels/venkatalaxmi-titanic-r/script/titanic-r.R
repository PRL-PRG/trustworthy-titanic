
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

train$Survived <- factor(train$Survived , levels=c(0,1))
levels(train$Survived) <- c("Survived" , "Died")
train$Pclass <- as.factor(train$Pclass)
levels(train$Pclass) <- c("list Class", "2nd Class" , "3rd class")

test$Pclass <- as.factor(test$Pclass)
levels(test$Pclass) <- c("list Class", "2nd Class" , "3rd class")


train<-subset(train , select = -c(Cabin))
train$Age[which(is.na(train$Age))] <- 29
train$Embarked[which(is.na(train$Embarked))] <- "S"

test<-subset(test , select = -c(Cabin))
test$Age[which(is.na(test$Age))] <- 29
test$Embarked[which(is.na(test$Embarked))] <- "S"
test$Fare[which(is.na(test$Fare))] <- 35


rf_tree<-randomForest(Survived~Age+Pclass+Sex+Fare+Parch, data = train , mtry = 3 , importance = TRUE )
print(rf_tree)
pred <- predict(rf_tree, newdata=test)




