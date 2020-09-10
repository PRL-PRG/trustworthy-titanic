
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(readr)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

train_temp <- train
train$Survived <- NULL

all_data <-rbind(train, test)

# Passenger on row 62 and 830 do not have a value for embarkment. 
# Since many passengers embarked at Southampton, we give them the value S.
all_data$Embarked[c(62, 830)] <- "S"

# Factorize embarkment codes.
all_data$Embarked <- factor(all_data$Embarked)

# Passenger on row 1044 has an NA Fare value. Let's replace it with the median fare value.
all_data$Fare[1044] <- median(all_data$Fare, na.rm = TRUE)

# Filter out the title
all_data$Title <- sapply(all_data$Name, FUN=function(x) {strsplit(toString(x), split='[,.]')[[1]][2]})
all_data$Title <- as.factor(all_data$Title)
#all_data$FamilySize <- all_data$SibSp + all_data$Parch + 1

# How to fill in missing Age values?
# We make a prediction of a passengers Age using the other variables and a decision tree model. 
# This time you give method = "anova" since you are predicting a continuous variable.
library(rpart)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title,
                       data = all_data[!is.na(all_data$Age),], method = "anova")
all_data$Age[is.na(all_data$Age)] <- predict(predicted_age, all_data[is.na(all_data$Age),])

# Split the data back into a train set and a test set
train <- all_data[1:891,]
test <- all_data[892:1309,]

train$Survived <- train_temp$Survived
#trainlabel <- train_temp$Survived

library(nnet)
model_final <- nnet(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked +Title , data = train,size=35, maxit=10000)
#Calculating Final Accuracies
my_prediction <- predict(model_final,test,type = "class")

# Create a data frame with two columns: PassengerId & Survived. Survived contains your predictions
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)


# Write your solution away to a csv file with the name my_solution.csv
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)