
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(rpart)

train <- read.csv("../input/train.csv", header = TRUE, stringsAsFactors = FALSE)
train$IsTrain <- TRUE
paste('Train Survived Pourcentage:', 100*(nrow(train[train$Survived == 1.0,]) / nrow(train)))
test <- read.csv("../input/test.csv", header = TRUE, stringsAsFactors = FALSE)
test$IsTrain <- FALSE

# Add Survived to test dataset
test$Survived <- NA
full <- rbind(train, test)

#str(full)
#summary(full)
#colSums(is.na(full))
#table(full$Sex)
print(full$Pclass)
#print(full$Sex)
#print(full$Embarked)
variables_should_factors <- c('Pclass', 'Sex', 'Embarked')
full[variables_should_factors] <- lapply(full[variables_should_factors], 
function(x) as.factor(x))
print(full$Pclass)
#print(full$Sex)
#print(full$Embarked)

summary(full$Age)
age_model <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data = full[!is.na(full$Age),], method = "anova")

full$Age[is.na(full$Age)] <- predict(age_model, full[is.na(full$Age),])
summary(full$Age)

# Here we will plot the passenger survival by class
#train$Survived <- factor(train$Survived, levels=c(1,0))
#levels(train$Survived) <- c("Survived", "Died")
#train$Pclass <- as.factor(train$Pclass)
#levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

