
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library (rpart)
library (caret)
#install.packages('rattle')
#install.packages ('rpart.plot')
#install.packages ('RColorBrewer')
#library(rattle)
library(rpart.plot)
library(RColorBrewer)
library (lattice)
library (ggplot2)



# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
# summary(train)

# Take a look at the training data set
#str(train)

# Take a look at the test data
#str(test)


# Number of female and male passengers in the training set
#table(train$Sex)

# Look at the proportion of survived and dead
#prop.table(table(train$Survived))

# Comparison of number of females and males that survived
#prop.table(table(train$Sex, train$Survived))

# Investigating the train variable Age
#summary(train$Age)


# *********************9-2-2016*************************


# Let us split train into two variables (one of 80%, the other of 20%)
train2 <- train[1:712,]
#train2
str(train2)

train2cv <- train[713:891,]
#train2cv

#How many survived in the train2 data set
#Survived = 278 (39.04%), Died = 434 (60.96%), total 712 in train2 
table(train2$Survived)
prop.table(table(train2$Survived))

#How many males, females in train2 data set
# Females = 256 (35.96%), Males = 456 (64.04%)
table(train2$Sex)
prop.table(table(train2$Sex))

#How many males, females survived
# Females survived = 190 (74.22% of all women)
# Males survived = 88 (19.30% of all males)
table(train2$Sex, train2$Survived)
prop.table(table(train2$Sex, train2$Survived),1)

# Now, to investigate Age
# table(train2$Age,train2$Survived)


fit <- rpart(Survived ~ Sex + Age + SibSp + Pclass + Embarked + Parch,
        data = train2,
       method = "class")
rpart.plot(fit)


#*****************************9-5-2016**********************************









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
