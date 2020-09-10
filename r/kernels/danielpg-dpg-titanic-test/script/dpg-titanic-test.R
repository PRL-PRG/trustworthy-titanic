
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)

###################################################
# Here we will plot the passenger survival by several factors

# First we transform the Survived column to a categorical variable ->

train$Survived <- factor(train$Survived, levels=c(1,0))
levels(train$Survived) <- c("Survived", "Died")

# Survival by class ->

train$Pclass <- as.factor(train$Pclass)
levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

png("1_survival_by_class.png", width=800, height=600)
mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
           
# Survival by genre ->

train$Sex <- factor(train$Sex, levels=c("male", "female"))
levels(train$Sex) <- c("Male", "Female")

png("1_survival_by_genre.png", width=800, height=600)
mosaicplot(train$Sex ~ train$Survived, main="Passenger Survival by Genre",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)

require (plyr)
require (ggplot2)
require (reshape2)

png("1_genre_by_age.png", width=800, height=600)
ggplot(data=train, aes(x = Age, fill = Sex), main="Passenger Genre by Age") +
  geom_histogram(binwidth = 1)

png("1_survival_by_age.png", width=800, height=600)
ggplot(data=train, aes(x = Age, fill = Survived), main="Passenger Survival by Age") +
  geom_histogram(binwidth = 1)

png("1_survival_by_age_and_genre.png", width=800, height=600)
ggplot(data=train, aes(x = Age, fill = Survived), main="Passenger Survival by Age and Genre") +
  geom_histogram(binwidth = 1) +
  facet_wrap(~Sex)
          
dev.off()
