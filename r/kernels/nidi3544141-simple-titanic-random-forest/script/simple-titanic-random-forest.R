


# Any results you write to the current directory are saved as output.
# package
library("randomForest")
library("rpart")



# loading the dataset
train_Titanic<- read.csv("../input/train.csv")
test_Titanic <- read.csv("../input/test.csv")
survived <- train_Titanic[,2]
all_titanic <- rbind(train_Titanic[,-2], test_Titanic)

# exploring data

str(all_titanic)
class(all_titanic)
dim(all_titanic)
str(all_titanic)
#na 
any(is.na(all_titanic))
table(is.na(all_titanic))
sapply(all_titanic, function(x) {sum(is.na(x))})


#creating new features
all_titanic$family_size <- all_titanic$SibSp + all_titanic$Parch + 1



# Factorize and numerize features
all_titanic$Embarked <- factor(all_titanic$Embarked)
all_titanic$Name <- as.numeric(all_titanic$Name)
all_titanic$Ticket <- as.numeric(all_titanic$Ticket)
all_titanic$Cabin <- as.numeric(all_titanic$Cabin)
# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_titanic$Fare[1044] <- median(all_titanic$Fare, na.rm = TRUE)

# Dealing with NA missing values (Age)

predicted_age_titanic <- rpart(Age ~ ., data = all_titanic[!is.na(all_titanic$Age),], method = "anova")
all_titanic$Age[is.na(all_titanic$Age)] <- predict(predicted_age_titanic, all_titanic[is.na(all_titanic$Age),])

# Spliting the data back into a train set and a test set
train_Titanic <- all_titanic[1:891,]
test_Titanic <- all_titanic[892:1309,]

# Set seed for reproducibility
set.seed(111)

# Applying the Random Forest Algorithm
my_forest <- randomForest(as.factor(survived) ~ .,
                          data = train_Titanic, importance = TRUE, ntree = 1000)

# Making prediction 
my_prediction <- predict(my_forest, test_Titanic)


#submission
submission <- data.frame(PassengerId = test_Titanic$PassengerId, survived= my_prediction)
write.csv(submission, file='submission_titanicrandomforest.csv', row.names=FALSE, quote=FALSE)