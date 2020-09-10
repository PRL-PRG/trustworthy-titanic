# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(dplyr)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

#Create Family Size variable
train$family_size <- train$SibSp + train$Parch + 1
test$family_size <- test$SibSp + test$Parch + 1

#Create Survived variable column in test and fill with NA
test$Survived <- NA

#Temporarily combine train and test to do formatting
combi <- rbind(train, test)

#Format combi
combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Jonkheer', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess')] <- 'Lady'
combi$Title <- factor(combi$Title)

all_data = combi

#Fill NAs in Embarked variable
all_data$Embarked[c(62,830)] = "S"
all_data$Embarked <- factor(combi$Embarked)

#Fill NAs in Fare variable
all_data$Fare[1044] <- median(combi$Fare, na.rm=TRUE)

#Predict age for missing ages
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       
                       data=all_data[!is.na(all_data$Age),], method="anova")

all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

#Reseparate train and test
train <- all_data[1:891,]
test <- all_data[892:1309,]

# Set seed for reproducibility
set.seed(111)

# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = train, importance = TRUE, ntree = 1000)

# Make your prediction using the test set
my_prediction <- predict(my_forest, test)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
my_solution_df <- tbl_df(my_solution)