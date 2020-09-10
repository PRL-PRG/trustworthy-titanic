
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
# summary(train)

# Take a look at the training data set
#str(train)

# Take a look at the test data
str(test)


# How many survived/died in the training set
table(train$Survived)

# Number of female and male passengers in the training set
table(train$Sex)

# Look at the proportion of survived and dead
prop.table(table(train$Survived))

# Comparison of number of females and males that survived
prop.table(table(train$Sex, train$Survived))

# Investigating the train variable Age
summary(train$Age)

# Here we will plot the passenger survival by class
# train$Survived <- factor(train$Survived, levels=c(1,0))
# levels(train$Survived) <- c("Survived", "Died")
# train$Pclass <- as.factor(train$Pclass)
# levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

# png("1_survival_by_class.png", width=800, height=600)
# mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
#           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
#           off=c(0), cex.axis=1.4)
#dev.off()
