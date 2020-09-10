
library(caret)
library(ggplot2)

list.files(path = "../input")
getwd()
list.files()
data <- read.csv("../input/train.csv")

set.seed(123)
trainIndex <- createDataPartition(data$Survived, p = .8, 
                                  list = FALSE, 
                                  times = 1)

train <- data[ trainIndex,]
test  <- data[-trainIndex,]
head(train)


#Exploratory data analysis
## Scatterplot matrix 1

featurePlot(x=train[,3:5],y=train$Survived, plot="pairs")

## Scatterplot matrix 2

featurePlot(x=train[,7:10],y=train$Survived, plot="pairs")

## Scatterplot matrix 3

featurePlot(x=train[,11:12],y=train$Survived, plot="pairs")


#Data pre-processing
Using K-nearest neighbors to impute missing values.

train <- predict(preProcess(train,method="knnImpute"), train)


fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)
set.seed(128)
glm1 <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
              data = train,
              method = "glm",
              trControl = fitControl)


glm1
glm1$finalModel
summary(glm1$finalModel)

set.seed(1651)
decTreeGrid <-  expand.grid(cp = seq(from=0.01, to=1, length.out=100))

decTree <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
              data = train,
              method = "rpart",
              trControl = fitControl,
              tuneGrid = decTreeGrid)

decTree

set.seed(3189)

treeBag <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
              data = train,
              method = "treebag",
              trControl = fitControl,
              #tuneGrid = decTreeGrid
                )

## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.





