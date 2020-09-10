# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(rpart)
library(randomForest)
library(e1071)
library(party)


# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

list.files("../input")

# Any results you write to the current directory are saved as output.

#Checking the training dataset 
trainData <- read.table("../input/train.csv", sep=",", header=TRUE)
head(trainData)
str(trainData)
table(trainData$Survived)
prop.table(table(trainData$Survived))

#Checking the test dataset
testData <- read.table("../input/test.csv", sep=",", header=T)
str(testData)
testData$Survived <- rep(0, nrow(testData))
submit <- data.frame(PassengerId = testData$PassengerId, Survived = testData$Survived)
#try the submission (Every one die)
write.csv(submit, file = "./test1.csv", row.names= F)

summary(trainData$Sex)
prop.table(table(trainData$Sex, trainData$Survived))
prop.table(table(trainData$Sex, trainData$Survived),1)

#Test 2: set all female & children passengers as having survived; 
testData$Survived <- 0
testData$Survived[testData$Sex == 'female'] <- 1 
testData$Survived[testData$Age < 18] <- 1 
submit <- data.frame(PassengerId = testData$PassengerId, Survived = testData$Survived)
write.csv(submit, file ="./test2.csv", row.names = F)

#Analysis by Age&Sex	
summary(trainData$Age)
summary(trainData$Sex)

#Creating a new variable "Child", and assuming that children also survived:
trainData$Child <- 0
trainData$Child[trainData$Age <18] <- 1
aggregate(Survived ~ Child + Sex, data=trainData, FUN=sum)
aggregate(Survived ~ Child + Sex, data=trainData, FUN=length)
aggregate(Survived ~ Child + Sex, data=trainData, FUN= function(x) { sum(x)/length(x) } )


#Investigating now at the passenger class
# Let’s bin the fares into less than $10, between $10 and $20, $20 to $30 and more than $30 and store it to a new variable
trainData$Fare2 <- '30+'
trainData$Fare2[trainData$Fare < 30 & trainData$Fare >= 20] <- '20-30'
trainData$Fare2[trainData$Fare < 20 & trainData$Fare >= 10] <- '10-20'
trainData$Fare2[trainData$Fare < 10] <- '<10'
aggregate(Survived ~ Child + Fare2 + Sex, data = trainData, FUN = function(x) { sum(x)/length(x) } )
aggregate(Survived ~ Fare2 + Pclass + Child + Sex, data = trainData, FUN = function(x) { sum(x)/length(x) } )


fol <- formula(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare)
modelR <- rpart(fol, method="class", data=trainData)
print(modelR)
modelR <- rpart(fol, method="class", data=trainData)
guessR<-predict(modelR,newdata=testData, type="class")
accuracy <- ( sum(guessR == testData$Survived)/nrow(testData) )
print(accuracy)

plot(modelR)
text(modelR)
dev.copy(png, file="./rpart.png", height=480, width=480)
dev.off()

library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(modelR)
dev.copy(png, file = "./rpart2.png", height = 480, width=480)
dev.off()

submit <- data.frame(PassengerId = testData$PassengerId, Survived = guessR)
write.csv(submit, file = "./test2final.csv", row.names = FALSE)

#########################################################

###Feature engineering
#Analysing reamining variables, starting by Name
trainData$Name[1]
#[1] Braund, Mr. Owen Harris
#891 Levels: Abbing, Mr. Anthony ... van Melkebeke, Mr. Philemon

#New data.frame to work
trainData <- read.table("../input/train.csv", sep=",", header=TRUE)
testData$Survived <- NA
combi <- rbind(trainData, testData)

#strings are automatically imported as factors in R, even if it doesn’t make sense. So we need to cast this column back into a text string. To do this we use as.character.
combi$Name <- as.character(combi$Name)
combi$Name[1]

#Splitting Strings through basic RGEX:
strsplit(combi$Name[1], split='[,.]')
#[[1]]
#[1] "Braund"       " Mr"          " Owen Harris"
strsplit(combi$Name[1], split='[,.]')[[1]]
#[1] "Braund"       " Mr"          " Owen Harris"

#String split uses a doubly stacked matrix because it can never be sure that a given regex will have the same number of pieces. If there were more commas or periods in the name, it would create more segments, so it hides them a level deeper to maintain the rectangular types of containers that we are used to in things like spreadsheets, or now dataframes!

#Ripping specific parts
strsplit(combi$Name[1], split='[,.]')[[1]][[2]]
#" Mr"
combi$Title <- sapply(combi$Name, FUN = function(x) { strsplit(x, split='[,.]')[[1]][[2]] })

#R’s apply functions all work in slightly different ways, but sapply will work great here. We feed sapply our vector of names and our function that we just came up with. It runs through the rows of the vector of names, and sends each name to the function. The results of all these string splits are all combined up into a vector as output from the sapply function, which we then store to a new column in our original dataframe, called Title.
#Finally, we may wish to strip off those spaces from the beginning of the titles. Here we can just substitute the first occurrence of a space with nothing. We can use sub for this (gsub would replace all spaces, poor ‘the Countess’ would look strange then though)

combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)
#         Capt           Col           Don          Dona            Dr 
#            1             4             1             1             8 
#     Jonkheer          Lady         Major        Master          Miss 
#            1             1             2            61           260 
#         Mlle           Mme            Mr           Mrs            Ms 
#            2             1           757           197             2 
#          Rev           Sir  the Countess 
#            8             1             1 

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'

#What have we done here? The %in% operator checks to see if a value is part of the vector we’re comparing it to. So here we are combining two titles, ‘Mme’ and ‘Mlle’, into a new temporary vector using the c() operator and seeing if any of the existing titles in the entire Title column match either of them. We then replace any match with ‘Mlle’.
#Let’s keep looking for redundancy. It seems the very rich are a bit of a problem for our set here too. For the men, we have a handful of titles that only one or two have been blessed with: Captain, Don, Major and Sir. All of these are either military titles, or rich fellas who were born with vast tracts of land. For the ladies, we have Dona, Lady, Jonkheer (*see comments below), and of course our Countess. All of these are again the rich folks, and may have acted somewhat similarly due to their noble birth. Let’s combine these two groups and reduce the number of factor levels to something that a decision tree might make sense of:
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'


#Our final step is to change the variable type back to a factor, as these are essentially categories that we have created:
combi$Title <- factor(combi$Title)

#variables SibSb and Parch that indicate the number of family members the passenger is travelling with. Seems reasonable to assume that a large family might have trouble tracking down little Johnny as they all scramble to get off the sinking ship, so let’s combine the two variables into a new one, FamilySize:
combi$FamilySize <- combi$SibSp + combi$Parch + 1

#Pretty simple! We just add the number of siblings, spouses, parents and children the passenger had with them, and plus one for their own existence of course, and have a new variable indicating the size of the family they travelled with.

#Combining the Surname with the family size to obtain 
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
#combi$Surname <- sub(' ', '', combi$Surname)

#We then want to append the FamilySize variable to the front of it, but as we saw with factors, string operations need strings. So let’s convert the FamilySize variable temporarily to a string and combine it with the Surname to get our new FamilyID variable:
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")

#We used the function paste to bring two strings together, and told it to separate them with nothing through the sep argument. This was stored to a new column called FamilyID. But those three single Johnsons would all have the same Family ID. Given we were originally hypothesising that large families might have trouble sticking together in the panic, let’s knock out any family size of two or less and call it a “small” family. This would fix the Johnson problem too.
combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
table(combi$FamilyID)
famIDs <- data.frame(table(combi$FamilyID))

#Here we see again all those naughty families that didn’t work well with our assumptions, so let’s subset this dataframe to show only those unexpectedly small FamilyID groups.
famIDs <- famIDs[famIDs$Freq <= 2,]

#overwrite any family IDs in our dataset for groups that were not correctly identified 
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- 'Small'
combi$FamilyID <- factor(combi$FamilyID)

#We are now ready to split the test and training sets back into their original states, carrying our fancy new engineered variables with them. The nicest part of what we just did is how the factors are treated in R. Behind the scenes, factors are basically stored as integers, but masked with their text names for us to look at. If you create the above factors on the isolated test and train sets separately, there is no guarantee that both groups exist in both sets.

#Because we built the factors on a single dataframe, and then split it apart after we built them, R will give all factor levels to both new dataframes, even if the factor doesn’t exist in one. It will still have the factor level, but no actual observations of it in the set. Neat trick right? Let me assure you that manually updating factor levels is a pain.

#So let’s break them apart and do some predictions on our new fancy engineered variables:
train <- combi[1:891,]
test <- combi[892:1309,]

#Time to do our predictions! We have a bunch of new variables, so let’s send them to a new decision tree. Last time the default complexity worked out pretty well, so let’s just grow a tree with the vanilla controls and see what it can do
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, method="class")
fancyRpartPlot(fit)

dev.copy(png, file = "./featureEngineering.png", height = 480, width=480)
dev.off()

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "./featureEngineering.csv", row.names = FALSE)

#Your Best Entry
#You improved on your best score by 0.01435. 
#You just moved up 448 positions on the leaderboard. 0.79904


#Final Approach: Random Forests

#First Approach - Bagging:
sample(1:10, replace = TRUE)
#In this simulation, we would still have 10 rows to work with, but rows 1, 2, 9 and 10 are each repeated twice, while rows 4, 5, 6 and 8 are excluded. If you run this command again, you will get a different sample of rows each time. On average, around 37% of the rows will be left out of the bootstrapped sample. With these repeated and omitted rows, each decision tree grown with bagging would evolve slightly differently. If you have very strong features such as gender in our example though, that variable will probably still dominate the first decision in most of your trees.

#Second Approach - The second source of randomness gets past this limitation though. Instead of looking at the entire pool of available variables, Random Forests take only a subset of them, typically the square root of the number available. In our case we have 10 variables, so using a subset of three variables would be reasonable. The selection of available variables is changed for each and every node in the decision trees. This way, many of the trees won’t even have the gender variable available at the first split, and might not even see it until several nodes deep.

#R’s Random Forest algorithm has a few restrictions that we did not have with our decision trees. The big one has been the elephant in the room until now, we have to clean up the missing values in our dataset. rpart has a great advantage in that it can use surrogate variables when it encounters an NA value. In our dataset there are a lot of age values missing. If any of our decision trees split on age, the tree would search for another variable that split in a similar way to age, and use them instead. Random Forests cannot do this, so we need to find a way to manually replace these values. A method we implicitly used in part 2 when we defined the adult/child age buckets was to assume that all missing values were the mean or median of the remaining data. Since then we’ve learned a lot of new skills though, so let’s use a decision tree to fill in those values instead. Let’s pick up where we left off last lesson, and take a look at the combined dataframe’s age variable to see what we’re up against


summary(combi$Age)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.17   21.00   28.00   29.88   39.00   80.00     263 

#263 values out of 1309 were missing this whole time, that’s a whopping 20%! A few new pieces of syntax to use. Instead of subsetting by boolean logic, we can use the R function is.na(), and it’s reciprocal !is.na() (the bang symbol represents ‘not’). This subsets on whether a value is missing or not. We now also want to use the method=”anova” version of our decision tree, as we are not trying to predict a category any more, but a continuous variable. So let’s grow a tree on the subset of the data with the age values available, and then replace those that are missing

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                data=combi[!is.na(combi$Age),], method="anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])

summary(combi$Embarked)
#      C   Q   S 
#  2 270 123 914 

#Embarked has a blank for two passengers. While a blank wouldn’t be a problem for our model like an NA would be, since we’re cleaning anyhow, let’s get rid of it. Because it’s so few observations and such a large majority boarded in Southampton, let’s just replace those two with ‘S’. First we need to find out who they are though! We can use which for this:
which(combi$Embarked == '')
#[1]  62 830

#This gives us the indexes of the blank fields. Then we simply replace those two, and encode it as a factor
combi$Embarked[c(62,830)] = "S"
combi$Embarked <- factor(combi$Embarked)

which(is.na(combi$Fare))
#[1] 1044
combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)
combi$Fare[1044]
#[1] 14.4542

#Okay. Our dataframe is now cleared of NAs. Now on to restriction number two: Random Forests in R can only digest factors with up to 32 levels. Our FamilyID variable had almost double that. We could take two paths forward here, either change these levels to their underlying integers (using the unclass() function) and having the tree treat them as continuous variables, or manually reduce the number of levels to keep it under the threshold.

#Let’s take the second approach. To do this we’ll copy the FamilyID column to a new variable, FamilyID2, and then convert it from a factor back into a character string with as.character(). We can then increase our cut-off to be a “Small” family from 2 to 3 people. Then we just convert it back to a factor and we’re done:

combi$FamilyID2 <- combi$FamilyID
combi$FamilyID2 <- as.character(combi$FamilyID2)
combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
combi$FamilyID2 <- factor(combi$FamilyID2)

#Okay, we’re down to 22 levels so we’re good to split the test and train sets back up as we did last lesson and grow a Random Forest. Install and load the package randomForest:

library(randomForest)
train <- combi[1:891,]
test <- combi[892:1309,]

set.seed(415)
fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize +
                    FamilyID2, data=train, importance=TRUE, ntree=2000)
varImpPlot(fit)
Prediction <- predict(fit, test)
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "./firstforest.csv", row.names = FALSE)

set.seed(415)
#fit <- svm(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
#               data = train)
#Prediction <- predict(fit, test, type="class")
fol <- as.factor(Survived) ~ Pclass + Sex + Age + Fare + Embarked + Title + FamilySize + FamilyID2
fit <- randomForest(fol, data=train, importance=TRUE, ntree=3000)
varImpPlot(fit)
Prediction<-predict(fit,test)
accuracy2RF <- ( sum(Prediction == test$Survived)/nrow(test) )
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "./secondforest.csv", row.names = FALSE)

#folnew <- formula(Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked + FamilySize + FamilyID2)
#modelRnew <- rpart(folnew, method="class", data=train)
#guessRnew<-predict(modelRnew,newdata=test, type="class")
#accuracyRnew <- ( sum(guessRnew == test$Survived)/nrow(test) )
#submit <- data.frame(PassengerId = test$PassengerId, Survived = modelRnew)
#write.csv(submit, file = "./rpartnew.csv", row.names = FALSE)

accuracy2RF
#accuracyRnew

#set.seed(415)
#fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
#               data = train, controls=cforest_unbiased(ntree=2000, mtry=3))
#Prediction <- predict(fit, test, OOB=TRUE, type = "response")
#submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "./secondforest.csv", row.names = FALSE)
