
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

#Data cleansing

train$Survived<- as.factor(train$Survived)
train$Pclass<- as.factor(train$Pclass)
train$Name<- as.character(train$Name)

test$Pclass<- as.factor(test$Pclass)
test$Name<- as.character(test$Name)

# A priori I think Age, class, sex will determine the chances of survival. 
# Age: Senior citizen might have been given priority
# Class: Lower class is present on lower part of ship where flooding started
# Sex : Female and children were given preference over males

#summary statistics of training data
summary(train)

#summary statistics of training data
summary(test)

# Fare data missing for one passenger who was in class 3 and started from 'S'. So this missing value
# can be replaced by median value of such passengers.

#index of the missing data = which(is.na(test$Fare))
# let us find the mean for such passengers
library(dplyr)    
subset_1 = filter(test, Pclass == 3 & Embarked == "S")
summarise(subset_1,delay = mean(Fare, na.rm = TRUE))

#Plug the mean in the test data
test$Fare[which(is.na(test$Fare))] = 13.91
summary(test)

# From the summary we see that Age has missing values
# I feel we can use the Name title information to generate more relevant information than just
# using sex. I created a function to asses the titles and divide into different categories.
# Miss was used for unmarried ladies. This indicates a teen age or early twenties age
# Woman/Man refer to is female and male
# Master is a male kid.

Titles<-function(Name)
{
  if (length(grep("Miss.",Name))>0) {return("Lady")}
  else if (length(grep("Mrs.",Name))>0) {return("Woman")}
  else if (length(grep("Mr.",Name))>0) {return("Man")}
  else if (length(grep("Master.",Name))>0) {return("Child")}
  else {return("Other")}
}
# creating a column title based on the title present in variable Name
title_data <-NULL
for(i in 1:nrow(train))
{
  title_data<-c(title_data,Titles(train[i,4]))
}
train$title <-as.factor(title_data)
title_data <-NULL
for(i in 1:nrow(test))
{
  title_data<-c(title_data,Titles(test[i,3]))
}
test$title <-as.factor(title_data)

# Create a random forrest model to predict the survival
#random forrest model
rf_data <- train[,c("Pclass","title","Fare")]
rf_newdata <- test[,c("Pclass","title","Fare")]
set.seed(12354)
rf_1 = randomForest(x=rf_data, y=train$Survived,ntree=1000)
rf_1
rf_2 = predict(rf_1,newdata = rf_newdata)
rf_2
# Arranging the solution based on the format asked
my_solution <- data.frame(PassengerID = test$PassengerId, Survived = rf_2)
# Saving the file to required format
write.csv(my_solution, file = "my_solution.csv",row.names = FALSE)


