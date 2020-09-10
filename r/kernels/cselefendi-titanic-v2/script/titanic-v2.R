
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)

# Here we will plot the passenger survival by class
train$Survived <- factor(train$Survived, levels=c(1,0))
levels(train$Survived) <- c("Túlélt", "Meghalt")
train$Pclass <- as.factor(train$Pclass)
levels(train$Pclass) <- c("1. osztály", "2. osztály", "3. osztály")
train$Sex <- factor(train$Sex, levels=c("female","male"))
levels(train$Sex) <- c("Nő", "Férfi")
train$Embarked <- factor(train$Embarked, levels=c("S", "C", "Q"))
levels(train$Embarked) <- c("2. Southampton", "3. Cherbourg", "4. Queenstown")
train$AgeBin <- as.factor(round((train$Age-2.5)*2,-1)/2)
train$Nagykoru <- as.factor(train$Age>=18)
levels(train$Nagykoru) <- c("Kiskorú", "Nagykorú")
#table(train$Nagykoru, train$Age)

tree1 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Nagykoru, data = train, method = "class")
fancyRpartPlot(tree1)

tree2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Nagykoru, data = train, method = "class", na.action = na.rpart)
fancyRpartPlot(tree2)