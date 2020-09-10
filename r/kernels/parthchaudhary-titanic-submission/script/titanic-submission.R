# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# Create the column child, and indicate whether child or no child
train$Child[train$Age < 18] <- 1
train$Child[train$Age >= 18] <- 0    
train$Child[is.na(train$Age)] <- NA

test$Child[test$Age < 18] <- 1
test$Child[test$Age >= 18] <- 0    
test$Child[is.na(test$Age)] <- NA

# Load in the R package  
library(rpart)

# create a new variable
train$family_size <- train$SibSp + train$Parch + 1 
test$family_size <- test$SibSp + test$Parch + 1 


#Extract title to create a new feature

extractTitle <- function(Name) { 
      Name <- as.character(Name) 
      
      if (length(grep("Miss.", Name)) > 0) { 
            return ("Miss.") 
      } else if (length(grep("Master.", Name)) > 0) { 
            return ("Master.") 
      } else if (length(grep("Mrs.", Name)) > 0) { 
            return ("Mrs.") 
      } else if (length(grep("Mr.", Name)) > 0) { 
            return ("Mr.") 
      } else if (length(grep("Rev.", Name)) > 0) { 
            return ("Rev.") 
      } else if (length(grep("Dr.", Name)) > 0) { 
            return ("Dr.") 
      } else if (length(grep("Lady.", Name)) > 0) { 
            return ("Lady.") 
      } else if (length(grep("Mlle.", Name)) > 0) { 
            return ("Mlle.") 
      }
      else { 
            return ("Other") 
      } 
}


titles <- NULL 
for (i in 1:nrow(train)) { 
      titles <- c(titles, extractTitle(train[i,4])) 
} 
train$Title <- as.factor(titles)

titles <- NULL 
for (i in 1:nrow(test)) { 
      titles <- c(titles, extractTitle(test[i,3])) 
} 
test$Title <- as.factor(titles)


# we need to combine the two data sets to get started with random forest

#But first we need to have the same number of columns. So we'll add a new column called "Survived" and populate it with "none"
test$Survived <- rep("None", nrow(test))

#rearranging the test dataset column before R binding it with the train dataset
test<-test[c("PassengerId" ,"Survived", "Pclass", "Name","Sex","Age","SibSp","Parch","Ticket","Fare","Cabin","Embarked","Child","family_size","Title")]

all_data <- rbind(train,test)


# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
# We code all embarkment codes as factors.
all_data$Embarked[c(62,830)] = "S"
all_data$Embarked <- factor(all_data$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm=TRUE)

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method="anova" since you are predicting a continuous variable.
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + family_size,
                       data=all_data[!is.na(all_data$Age),], method="anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Split the data back into a train set and a test set
train <- all_data[1:891,]
test <- all_data[892:1309,]

# Load in the package
library(randomForest)


# Apply the Random Forest Algorithm
my_forest <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + family_size, 
                          data=train, importance=TRUE, ntree=2000)

# Make your prediction using the test set
my_prediction <- predict(my_forest, test)

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
