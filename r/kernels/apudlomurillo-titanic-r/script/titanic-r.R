
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
# library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
#test  <- read.csv("../input/test.csv")

numeric_vars <- train[,c(2,3,6,7,8,10)]

Titanic <- na.omit(numeric_vars)
head(Titanic$Age, 10)

head(Titanic)
cor(Titanic)
plot(Titanic$Age, Titanic$Fare, main = "Correlation between Age and Fare on the Titanic Voyage")
#plot(complete$SibSp, complete$Fare)

# We can inspect the train data. The results of this are printed in the log tab below
#summary(train)

# Here we will plot the passenger survival by class
#train$Survived <- factor(train$Survived, levels=c(1,0))
#levels(train$Survived) <- c("Survived", "Died")
#train$Pclass <- as.factor(train$Pclass)
#levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

#png("1_survival_by_class.png", width=800, height=600)
#mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
#           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
#           off=c(0), cex.axis=1.4)
#dev.off()

#head(train)

#head(train$Age)
#Average.age <- mean(train$Age, na.rm=TRUE)
#(Average.age)
#sd(train$Age, na.rm=TRUE)
#qqnorm(train$Age)

#library(MASS)
#fitdistr(train$Age, "normal")

#print(train$Age) # Missing values have a value of NA 
# Here I am using indexing, so where the variable AGE is missing, the average age will be assigned to that observation
#train$Age[is.na(train$Age)] <- Average.age
#print(train$Age)

#mean(train$Age)
#sd(train$Age)

# From this plot I can see this data is not normally distributed 
#qqnorm(train$Age)

# Pvalue < 0.05 so reject the null. Variable is not normally distributed 
#shapiro.test(train$Age)


#shapiro.test(log(train$Age)) # This does not correct the problem - still not normally distributed 
#hist(train$Age)


#library(Hmisc)
#rcorr(train)
