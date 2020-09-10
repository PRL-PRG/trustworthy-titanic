# Very much a work-in-progress
# I know I can merge then split the datasets
# to cut down on the duplicate code.
# and I WILL!
#
#
library(caret)
library(randomForest)
library(stringr)

trainSet <- read.table("../input/train.csv", sep = ",", header = TRUE)
testSet <- read.table("../input/test.csv", sep = ",", header = TRUE)
head(trainSet)


head(testSet)


table(trainSet[,c("Survived", "Pclass")])

## ----, warning = FALSE, message = FALSE----------------------------------
# Comparing Age and Survived: The boxplots are very similar between Age
# for survivors and those who died. 
library(fields)
bplot.xy(trainSet$Survived, trainSet$Age)
# Also, there are lots of NA's. Exclude this variable
##
testSet$Nlen <- str_length(testSet$Name)
trainSet$Nlen <- str_length(trainSet$Name)
trainSet$Fam <- trainSet$Parch + trainSet$SibSp
testSet$Fam <- testSet$Parch + testSet$SibSp
trainSet$Survived <- factor(trainSet$Survived)
##
trainSet$Title<-regmatches(as.character(trainSet$Name),regexpr("\\,[A-z ]{1,20}\\.", as.character(trainSet$Name)))
trainSet$Title<-unlist(lapply(trainSet$Title,FUN=function(x) substr(x, 3, nchar(x)-1)))
table(trainSet$Title)
trainSet$Title[which(trainSet$Title %in% c("Mme", "Mlle"))] <- "Miss"
trainSet$Title[which(trainSet$Title %in% c("Lady", "Ms", "the Countess", "Dona"))] <- "Mrs"
trainSet$Title[which(trainSet$Title=="Dr" & trainSet$Sex=="female")] <- "Mrs"
trainSet$Title[which(trainSet$Title=="Dr" & trainSet$Sex=="male")] <- "Mr"
trainSet$Title[which(trainSet$Title %in% c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir"))] <- "Mr"
trainSet$Title<-as.factor(trainSet$Title) #convert to factor variable 
#
testSet$Title<-regmatches(as.character(testSet$Name),regexpr("\\,[A-z ]{1,20}\\.", as.character(testSet$Name)))
testSet$Title<-unlist(lapply(testSet$Title,FUN=function(x) substr(x, 3, nchar(x)-1)))
table(testSet$Title)
testSet$Title[which(testSet$Title %in% c("Mme", "Mlle"))] <- "Miss"
testSet$Title[which(testSet$Title %in% c("Lady", "Ms", "the Countess", "Dona"))] <- "Mrs"
testSet$Title[which(testSet$Title=="Dr" & testSet$Sex=="female")] <- "Mrs"
testSet$Title[which(testSet$Title=="Dr" & testSet$Sex=="male")] <- "Mr"
testSet$Title[which(testSet$Title %in% c("Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir"))] <- "Mr"
testSet$Title<-as.factor(testSet$Title) #convert to factor variable 
# Set a random seed (so you will get the same results as me)
set.seed(421)
# Train the model using a "random forest" algorithm
model <- train(Survived ~ Pclass + Sex + Fam + Nlen +   
                 Embarked + Fare + Title, # Survived is a function of the variables we decided to include
               data = trainSet, # Use the trainSet dataframe as the training data
               method = "rf",# Use the "random forest" algorithm
               trControl = trainControl(method = "cv", # Use cross-validation
                                        number = 5) # Use 5 folds for cross-validation
)

print(model)

testSet$Fare <- ifelse(is.na(testSet$Fare), mean(testSet$Fare, na.rm = TRUE), testSet$Fare)

testSet$Survived <- predict(model, newdata = testSet)

submission <- testSet[,c("PassengerId", "Survived")]
write.table(submission, file = "submission.csv", col.names = TRUE, row.names = FALSE, sep = ",")



