
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)

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


# This script trains a Random Forest model based on the data,
# saves a sample submission, and plots the relative importance
# of the variables in making predictions

# Download 1_random_forest_r_submission.csv from the output below
# and submit it through https://www.kaggle.com/c/titanic-gettingStarted/submissions/attach
# to enter this getting started competition!

library(ggplot2)
library(randomForest)

set.seed(1)
train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
test  <- read.csv("../input/test.csv",  stringsAsFactors=FALSE)

extractFeatures <- function(data) {
  features <- c("Pclass",
                "Age",
                "Sex",
                "Parch",
                "SibSp",
                "Fare",
                "Embarked")
  fea <- data[,features]
  fea$Age[is.na(fea$Age)] <- -1
  fea$Fare[is.na(fea$Fare)] <- median(fea$Fare, na.rm=TRUE)
  fea$Embarked[fea$Embarked==""] = "S"
  fea$Sex      <- as.factor(fea$Sex)
  fea$Embarked <- as.factor(fea$Embarked)
  return(fea)
}

rf <- randomForest(extractFeatures(train), as.factor(train$Survived), ntree=100, importance=TRUE)

submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test))
write.csv(submission, file = "1_random_forest_r_submission.csv", row.names=FALSE)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
     geom_bar(stat="identity", fill="#53cfff") +
     coord_flip() + 
     theme_light(base_size=20) +
     xlab("") +
     ylab("Importance") + 
     ggtitle("Random Forest Feature Importance\n") +
     theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)