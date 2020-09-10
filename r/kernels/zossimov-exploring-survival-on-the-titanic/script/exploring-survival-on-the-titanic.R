## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library(rpart)
library(dplyr)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)
full  <- bind_rows(train, test) # bind training & test data



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Build the model (note: not all possible variables are used)
# Build the decision tree
tree_model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Titles Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
table(full$Sex, full$Title)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

full01 <- full
full01$Sex <- as.factor(full01$Sex)
full01$Embarked <- as.factor(full01$Embarked)
full01$Title <- as.factor(full01$Title)

train_new <- full01[full01$PassengerId %in% train$PassengerId,]
str(train_new)

test_new <- full01[full01$PassengerId %in% test$PassengerId,]
str(test_new)

# Finish the command
my_tree_five <- rpart(Survived ~ Pclass + Sex + Age + SibSp + 
                        Parch + Fare + Embarked + Title,
                      data = train_new, method = "class")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Make predictions on the test set
my_prediction <- predict(tree_model, newdata = test, type = "class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution to validate format is right
nrow(my_solution);ncol(my_solution)

# Write the solution to file
write.csv(my_solution, file = 'tree_basic_Solution.csv', row.names = FALSE)

# Make prediction with alt model
my_prediction <- predict(my_tree_five, test_new, type = "class")

# Make results ready for submission
my_solution <- data.frame(PassengerId = test_new$PassengerId, Survived = my_prediction)

# Write the solution to file
write.csv(my_solution, file = 'tree_title_Solution.csv', row.names = FALSE)



