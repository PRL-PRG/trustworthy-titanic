
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)


#############
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
print(date())
print("Prabhat's code")
############

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
str(train)
# 'data.frame':	891 obs. of  12 variables
str(test)
# 'data.frame':	418 obs. of  11 variables:
summary(train)
# Age has 197 NAs. Min age of 0.42, Tickets other 852, Max fare of 512 is to far from Q3, cabin 186 others
summary(test)
# Age has 86 NAs, Min age is 0.17, Tickets - others are 396, Fare 412 is way higer than q3, cabin has 80 others
table(train$Survived)
# 0 549/1 342
table(train$Sex, train$Survived)
# female 0 81, 1 233/male 0 468, 1 109


# Here we will plot the passenger survival by class
#train$Survived <- factor(train$Survived, levels=c(1,0))
#levels(train$Survived) <- c("Survived", "Died")
#train$Pclass <- as.factor(train$Pclass)
#levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

png("1_survival_by_class.png", width=800, height=600)
mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
dev.off()

########### Analyze decision tree ###########
# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to build a fancy plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)


# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)

# Make predictions on the test set
my_prediction <- predict(my_tree_two, newdata = test, type = "class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)

# Finish the write.csv() call
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)

