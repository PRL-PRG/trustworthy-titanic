
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)

# Model
dataIn <- train %>% select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(SibSp_bin = ifelse(SibSp %in% c(1,2), 0, 1), Parch_bin = ifelse(Parch %in% c(1,2,3), 0, 1),
         Embarked_bin = ifelse(Embarked %in% c('Q','S'), 0, 1),
         Fare_Bin = cut(Fare, breaks = c(0, 11, 51, 515), right = FALSE)) %>%
  select(-c(SibSp, Parch, Embarked, Fare))

titanic.tree <- rpart(Survived ~ ., data = dataIn, method = 'class')
plot(titanic.tree)
text(titanic.tree)

dataIn2 <- test %>% select(Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(SibSp_bin = ifelse(SibSp %in% c(1,2), 0, 1), Parch_bin = ifelse(Parch %in% c(1,2,3), 0, 1),
         Embarked_bin = ifelse(Embarked %in% c('Q','S'), 0, 1),
         Fare_Bin = cut(Fare, breaks = c(0, 11, 51, 515), right = FALSE)) %>%
  select(-c(SibSp, Parch, Embarked, Fare))
test.pred <- predict(titanic.tree, dataIn2, type = "class")
ans <- data.frame(PassengerId = test$PassengerId, Survived = test.pred)

# Here we will plot the passenger survival by class
train$Survived <- factor(train$Survived, levels=c(1,0))
levels(train$Survived) <- c("Survived", "Died")
train$Pclass <- as.factor(train$Pclass)
levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

png("1_survival_by_class.png", width=800, height=600)
mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
dev.off()
