
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


test.survived<-data.frame(Survived=rep("None",nrow(test)),test[,])
data.combined<-rbind(train,test.survived)
str(data.combined)
data.combined$Survived<-as.factor(data.combined$Survived)
data.combined$Pclass<-as.factor(data.combined$Pclass)
table(data.combined$Survived)
train$Pclass<-as.numeric(train$Pclass)
library("ggplot2")
ggplot(train,aes(x = Pclass,fill=factor(Survived)))+
  geom_histogram(binwidth=0.5)+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill="Survived")