
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
#cat ("\n\n Training Set Data stats\n")
#summary(train)

# Take a look at the training data set
#cat ("\n\n Training Set Data structure\n")
#str(train)

# How many survived/died in the training set

#cat ("\n\nhow many died and how many survived\n")
#table(train$Survived)


# Number of female and male passengers in the training set
#cat ("\n\nNumber of female and male passengers in the training set\n")
#table(train$Sex)

# Look at the proportion of survived and dead
#cat ("\n\nPercentage of Dead and Survived\n")
#prop.table(table(train$Survived))

# Comparison of number of females and males that survived
#cat ("\n\nPercentage of females vs males that died/survived\n")
#prop.table(table(train$Sex, train$Survived))

# Investigating the train variable Age
#cat ("\n\nAge stats of Training set\n")
#summary(train$Age)

# Investigating the stats of the Sex variable in train
#cat("\n\n Sex statistics\n")
#summary(train$Sex)

# Investigating the proportion of sex and survived
#cat ("\n\nPercentage of females and males who died/survived")
#prop.table(table(train$Sex, train$Survived),1)


#Investigating the Age field further and converting this to a binary category (child or not child)
cat("\n\nDefining a new variable withing the data frame train called Child and assigning it a 0 or a 1")
train$Child <- 0
train$Child[train$Age < 18] <-1
str(train)




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
