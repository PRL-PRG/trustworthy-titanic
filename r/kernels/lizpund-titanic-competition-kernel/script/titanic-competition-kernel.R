# Author: Liz Pund (lizpund@gmail.com)
# Date: January 20, 2018
# Purpose: Kaggle-Titanic Dataset Competition

# Set working directory and import datafiles
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

# ATTEMPT 1
# BEGIN by using the target variable as the predictor:

# Get a preliminary understanding of the Survived results in the train file
# Display as a table
table(train$Survived)

# Calclate the the Survived results as proportions by inputting the previous calculation
# Display as a table
prop.table(table(train$Survived))

# Make prediction in Test file
test$Survived <- rep(0, 418)

# Create output file to submit to Kaggle
# Two columns only: PassengerID and Survived predictions
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

# This prediction generated an accuracy score of .62678 (Prediction Attempt #1)
# This makes sense given the proportions of Survived we see in the Train file
# However, this puts us near the bottom of the entries - not close to accurate!

# ATTEMPT 2
# IMPROVE your prediction by using other variables

# Calculate the count of passengers by sex
summary(train$Sex)

# Display the count of passengers by sex as a proportion
prop.table(summary(train$Sex))

# Use a table to display a two-way comparison of Sex and Survived (propotions)
# The first variable is rows, the second variable is columns
prop.table(table(train$Sex, train$Survived))

# This looks messy because by default, the proportion table command takes each entry
# in the table and divides by the total number of passengers.
# What we want to see is the row-wise proportion, ie, the proportion of each sex that 
# survived, as separate groups.
# Use a 1 for proportions across rows; you would us a 2 for columns.
prop.table(table(train$Sex, train$Survived), 1)

# We see that about most women survived and most men did not survive. 
# In our first attempt we predicted that 100% did not survive. 
# So, let's update our prediction based on this information.

# Create (overwrite) the Survived prediction column in the Test file
# Assign 0 to the whole column (this has the same effect as the rep method we used last time)
test$Survived <- 0

# Mark all women as having survived
# The square brackets create a subset of the total dataframe and apply the 1
# only to that subset.
# Here, the double equals sign is a boolean equivalency. It doesn't assign a value.
test$Survived[test$Sex == 'female'] <- 1

# Create output file to submit to Kaggle
# Two columns only: PassengerID and Survived predictions
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allmenperish.csv", row.names = FALSE)

# This prediction generated an accuracy score of .76555 (Prediction Attempt #2)
# Better! But could be better.

# ATTEMPT 3.a
# IMPROVE your prediction by using other variables

# Calculate the count of passengers by age
summary(train$Age)

# Since Age is a continuous variable, we can't easily run proportions on it
# Instead, create a new column, Child, to indicate if the passenger is under 18
train$Child <- 0
train$Child[train$Age < 18] <- 1 

# Create a table with Sex and Age to see the survival proportions for different subsets.
# Proportion table won't work, so use aggregate instead.
# This will display the sum of people of each sex/age combination that survived
# But, it doesn't tell us how many people of each sex/age there were total (survivors and nonsurvivors)
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

# Display the total number of people of each sex/age combination
aggregate(Survived ~ Child + Sex, data=train, FUN=length)

# To generate the proportion (which is what we're after) we need to use this formula
# This formula takes the subset vector as input and applies both the sum and length commands to it, 
# and then does the division to give us a proportion.
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# It appears that, still, most females survived and most males did not, regardless of age, so there is not 
# something to change in our predictions based on this.

# ATTEMPT 3.b
# Since sex did not give us a different prediction, let's look at other variables

# Using the ticket price
# It is a continuous variable, so let's bin it into manageable categories
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

# Run a longer aggregate function to see if there’s anything interesting to work with here
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=(function(x) {sum(x)/length(x)}))

# We see that in all combinations, men mostly do not survive and most women do survive.
# What stands out is that 3rd class women who paid more than $20 for their ticket mostly did NOT survive.
# Update our prediction to reflect this.

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare > 20] <- 0

# Submit the output to Kaggle and see if we did any better.

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allmenandsomewomenperish.csv", row.names = FALSE)

# That did better, with an accuracy score of .77990. 
# It's only a 1.5% score increase but I moved up around 3,000 spots on the leaderboard.

# ATTEMPT 4: Decision Trees - Use machine learning to do the heavy lifting for us
# We will use rpart package

# Import rpart package
library(rpart)

# Build my first model with rpart
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, 
             data=train, 
             method="class")

# Examine the tree
plot(fit)
text(fit)

# Not very insightful or easy to understand
# Install packages to get more useful and visually appealing graphics


# install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')
# library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Now, render the tree with a nicer look

fancyRpartPlot(fit)

# Generate the latest prediction and submit to Kaggle
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

# This jumped me 605 (?) positions up the leaderboard, to place 3,992 with .78468 accuracy

# ATTEMPT 5: Remove the limits on the decision tree. Max it out.

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit=2, cp=0))

fancyRpartPlot(fit)

# It is so complex we can't even read it! 
# Make a prediction from this model

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myseconddtree.csv", row.names = FALSE)

# This did worse than our simple gender split model. It is overfitting.

# ATTEMPT 6: Try reigning in the controls a bit; but keeping them looser than the first dtree (Attempt 4)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class",
             control=rpart.control(minsplit=10, cp=0.005))
new.fit <- prp(fit,snip=TRUE)$obj
fancyRpartPlot(new.fit)

Prediction <- predict(new.fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "mythirddtree.csv", row.names = FALSE)

# This did better than the previous decision tree but still was not my best submission.
# Scored .77033

# ATTEMPT 6: Feature Engineering. 
# We look at the fields that we didn't include in our machine learning model already.

# Let's start with the Name field. What do we notice in here that might have relevance to their survival?
# Their titles indicate something about their class, age, and gender. Can we extract those and use them in our model?

# Take a glance at the first passenger's name - notice in the output it includes "891 Levels:..." this is because the Names column is factors, not strings
train$Name[1]

# We will need to perform the same action on the training and test sets.
# An easy way to do that is to merge both data sets. 
# In order to use rbind, the two datasets must have the same columns. 
# We're lacking "Survived" in our test set, so we create it full of missing values.

test$Survived <- NA
test$Fare2 <- NA
test$Child <- NA

# Then we bind the two sets together in a new dataframe called "combi". They stack in the order we specify.
combi <- rbind(train, test)

# We have to convert the Names column from factors to strings
combi$Name <- as.character(combi$Name)

# Take a second look at the first passenger's name
combi$Name[1]

# Break apart each name at the comma and the full stop using strsplit
strsplit(combi$Name[1], split='[,.]')

# Isolate index number 2 of this new container, which is where the title lives
strsplit(combi$Name[1], split='[,.]')[[1]][2]

# Apply this to the entire dataframe
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x,split='[,.]')[[1]][2]})

# Strip the extra space from beginning of each title
combi$Title <- sub(' ', '', combi$Title)

# View the summary of the new column, Titles
table(combi$Title)

# Notice that there are some Titles that are rare so our model can't do much with them. Combine them together.
# Replace any cell in the Title column that matches 'Mme' or 'Mlle' with 'Mlle'
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

# Final step is to change the variable type back to Factor, since these are essentially categories that we've created
combi$Title <- factor(combi$Title)

# We're done with the passenger title now. 
# What else can we think up?  Family Size. Seems reasonable that a large family may have trouble tracking down little Johnny while the ship is sinking.
# Combine Sibsp and Parch to get the number of family members a passenger is traveling with
# Add the number of siblings, parents, spouses, and children + 1 for the passenger themselves 

combi$FamilySize <- combi$SibSp + combi$Parch + 1

# Anything more about families?
# What else can we think up?