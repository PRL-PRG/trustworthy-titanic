## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.train <- read.csv("../input/train.csv");
titanic.test <- read.csv("../input/test.csv");

head(titanic.train)
tail(titanic.train)
summary(titanic.train)
str(titanic.train)

# Age has some NAs, and there are blank entries for Cabin and Embarked (only two for "Embarked"). As far as the structure is concerned, I'd probably change Survived to a factor and Pclass to a factor. I may consider getting rid of PassengerId, Name, Ticket, and Cabin. I will also need to decide what to do about the 177 NAs for Age, and the two blank entries for Embarked. Seeing as its only two, I may delete the entries where Embarked is left blank, but probably can't do the same for NA age.

head(titanic.test)
tail(titanic.test)
summary(titanic.test)
str(titanic.test)

# Still some NAs for age in the test dataset, as well as one instance where Fare is NA. Still some blanks in Cabin, although all of Embarked looks good now. We also notice that there's no "Survived" column in the test dataset.


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Combine the two datasets and clean together
  # We need a new flag to indicate whether the row is originally from the training data set or the test dataset

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

# Ideally, we would do an rbind() to create one dataset, but we don't have the same columns...test is missing a "Survived" column

names(titanic.train)
names(titanic.test)

titanic.test$Survived <- NA # Keep it as an NA for now
titanic.full <- rbind(titanic.train, titanic.test)

table(titanic.full$IsTrainSet) # Just to make sure we're matching what we had previously

# Look for missing values and replace

summary(titanic.full)

# 263 NAs for Age. Fare has 1 NA. Cabin has 1014 blank rows, so we should remove Cabin. Embarked has 2 blank rows. 

# Embarked

table(titanic.full$Embarked) # A lot of Ss (Southhampton). Replace missing embarked with the mode, which is S.
titanic.full[titanic.full$Embarked == "",]
titanic.full[titanic.full$Embarked == "","Embarked"] <- "S"
titanic.full[titanic.full$Embarked == "",] # Verify that it was updated, which it is.
titanic.full[titanic.full$PassengerId == 62 | titanic.full$PassengerId == 830,] # These were the two that were missing earlier, but now have S.

# Age

table(is.na(titanic.full$Age)) # 263 instances where Age is equal to NA.

# We will replace with the median of titanic.full (for the sake of simplicity here)

median(titanic.full$Age, na.rm = TRUE) # Make sure to remove the NAs
titanic.full[is.na(titanic.full$Age),"Age"] <- median(titanic.full$Age, na.rm = TRUE)

table(titanic.full$Age) # A ton of age 28s, so it worked.

# Fare

titanic.full[is.na(titanic.full$Fare),] # Mr. Thomas Storey has NA fare.

# We could do a number of things (decision tree, regression), but we will just do median fare again for the sake of simplicity.

median(titanic.full$Fare, na.rm = TRUE)

titanic.full[is.na(titanic.full$Fare),"Fare"] <- median(titanic.full$Fare, na.rm = TRUE)

# Cabin

titanic.full$Cabin <- NULL

# Change the types of the columns if needed

str(titanic.full)

titanic.full$PassengerId <- as.factor(titanic.full$PassengerId)
titanic.full$Pclass <- as.factor(titanic.full$Pclass)

# Split the dataset back into training and test data sets

titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE,]

# Change the Survived column into a factor

titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
g <- ggplot(data = titanic.train, aes(x = Sex, fill = Survived)) + geom_bar()
g # We can see that there were more males on the Titanic that females (just from the actual count), and we can see that a much higher number of females survived

g <- ggplot(data = titanic.train, aes(x = Pclass, fill = Survived)) + geom_bar()
g # First class passengers survived at a higher rate

g <- ggplot(data = titanic.train, aes(x = Embarked, fill = Survived)) + geom_bar(position = "dodge")
g # Southhampton was the most common embarking point (which we knew). It's somewhat difficult to tell whether this impacted survival or not, let's make a table.
table(titanic.train$Embarked, titanic.train$Survived) 

g <- ggplot(data = titanic.train, aes(x = SibSp, fill = Survived)) + geom_bar()
g # A lot of single folks on the boat, and that didn't help their chances for survival.

g <- ggplot(data = titanic.train, aes(x = SibSp, fill = Survived)) + geom_bar() + facet_grid(~Sex)
g # A lot of single males, and a lot did not make it.

g <- ggplot(data = titanic.train, aes(x = SibSp, fill = Survived)) + geom_bar() + facet_wrap(~Pclass)
g # Maybe slightly better for Pclass == 1.

g <- ggplot(data = titanic.train, aes(x = Age)) + geom_density()
g # Big spike at 28, which makes sense since we put them all at 28 where we had NAs.

g <- ggplot(data = titanic.train, aes(x = Fare)) + geom_density() + facet_grid(~ Pclass)
g

g <- ggplot(titanic.train, aes(x = Pclass, y = Fare)) +
  geom_boxplot()
g

g <- ggplot(titanic.train, aes(x = Pclass, y = Fare, color = Pclass)) +
  geom_boxplot() + geom_jitter() # Now we can really see the spread of how expensive the first class tickets were.
g

g <- ggplot(titanic.train, aes(x = Pclass, y = Fare, color = Survived)) +
  geom_boxplot() + geom_jitter() + ggtitle("Titanic Survival by Class")# Now we can really see the spread of how expensive the first class tickets were.
g


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest)

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

titanic.model <- randomForest(formula = survived.formula, 
                              data = titanic.train, 
                              ntree = 500,
                              mtry = 3,
                              nodesize = 0.01 * nrow(titanic.train))

Survived <- predict(titanic.model, newdata = titanic.test)

# PassengerID and Survived

PassengerID <- titanic.test$PassengerId

output.df <- as.data.frame(PassengerID)
output.df$Survived <- Survived

write.csv(output.df, "titanic_kaggle_submission.csv",row.names = FALSE)

