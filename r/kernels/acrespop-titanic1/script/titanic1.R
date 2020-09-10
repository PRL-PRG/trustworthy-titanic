
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
titanic.train <- read.csv("../input/train.csv")
titanic.test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(titanic.train)

titanic.test$Survived <- rep(0, 418)
submit <- data.frame(
    PassengerId = titanic.test$PassengerId, 
    Survived = titanic.test$Survived
    )
write.csv(submit, file = "EmptyResult.csv", row.names = FALSE)

# Here we will plot the passenger survival by class
#train$Survived <- factor(train$Survived, levels=c(1,0))
#train$Survived
#levels(train$Survived) <- c("Survived", "Died")
#train$Pclass <- as.factor(train$Pclass)
#levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

randomForest(as.factor(Survived) ~ Pclass + Sex + SibSp, data=titanic.train,ntree=5000, importance=TRUE)

#png("1_survival_by_class.png", width=800, height=600)
#mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
#           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
#           off=c(0), cex.axis=1.4)
#dev.off()
