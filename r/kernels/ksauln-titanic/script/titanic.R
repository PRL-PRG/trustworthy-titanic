
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
#summary(train)

# Here we will plot the passenger survival by class
#train$Survived <- factor(train$Survived, levels=c(1,0))
#levels(train$Survived) <- c("Survived", "Died")
#train$Pclass <- as.factor(train$Pclass)
#levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

#png("1_survival_by_class.png", width=800, height=600)
#mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
#           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
#           off=c(0), cex.axis=1.4)
#dev.off()


#plotting train set
plot(train)


#http://trevorstephens.com/kaggle-titanic-tutorial/getting-started-with-r/
#decision trees
#devtools::install_github("jakesherman/packages")
#install.packages(c('rattle','rpart.plot','RColorBrewer'))
#libraries('rpart','rattle','rpart.plot','RColorBrewer')
library('rpart')
library('rattle')
library('rpart.plot')
library('RColorBrewer')

fit <- rpart(Survived~ Pclass +Sex+Age+SibSp+Parch+Fare+Embarked,data=train,method='class')

plot(fit)
text(fit)

fancyRpartPlot(fit)


#make prediction based on first decision tree
prediction <- predict(fit, test, type='class')
#if submiting uncomment below
#submit <- data.frame(PassenderId = test$PassengerId, Survived =Prediction)
#write.csv(sumit, file='MyFirstDecisionTree.csv', row.names=FALSE)


#removing limitaions of rpart decision tree, This is overfitting. Even if 100% accurate on the train data
    #will not be the same for the test data because the train data model has such individualized cases
#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
#               data=train,
#               method="class", 
#               control=rpart.control(minsplit=2, cp=0))
# fancyRpartPlot(fit)

# Next step allows me to prune tree using the GUI
#fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
#               data=train,
#               method="class",
#               control=rpart.control( your controls ))
#               
#new.fit <- prp(fit,snip=TRUE)$obj
#fancyRpartPlot(new.fit)


# Feature Engineering

#binding the train and test set together
test$Survived<-NA
combined <- rbind(train,test)

#converting name back to a string, was imported as a factor

combined$Name <- as.character(combined$Name)

#splitting up the name so we only get the title

combined$Title <- sapply(combined$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})

#stripping off the leading space
combined$Title <- sub(' ','',combined$Title)

table(combined$Title)

#combining a few of the titles that are super unique
    #%in% operator checks to see if a value is part of the vector we’re comparing it to. 
    #So here we are combining two titles, “Mme” and “Mlle”, into a new temporary vector using the 
    #c() operator and seeing if any of the existing titles in the entire Title column match either
    #of them. We then replace any match with “Mlle”

combined$Title[combined$Title %in% c('Mme','Mlle')] <- 'Mlle'
combined$Title[combined$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combined$Title[combined$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'

#change the title back to a factor

combined$Title <- factor(combined$Title)

#combining two factor to create family size

combined$FamilySize <- combined$SibSp + combined$Parch + 1

#combine surname with family size to hopefully be able to group the family together

combined$Surname <- sapply(combined$Name, FUN=function(x) {strsplit(x,split='[,.]')[[1]][1]})
combined$FamilyID <- paste(as.character(combined$FamilySize), combined$Surname)

# label any family size two or less as small

combined$FamilyID[combined$FamilySize <= 2] <- 'Small'
table(combined$FamilyID)

#we see that there are still families with size 1 and 2. Need to fix

famIDs <- data.frame(table(combined$FamilyID))
famIDs <- famIDs[famIDs$Freq <=2,]           #subsets data to get familysizes 2 or less

combined$FamilyID[combined$FamilyID %in% famIDs$Var1] <- 'Small'      #identifies those families from famIDs in the main table and changes their label to small
combined$FamilyID <- factor(combined$FamilyID)     #set the column as a factor

#break the data back into train and test sets
train <- combined[1:891,]
test <- combined[892:1309,]

#Predictions for the new data

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
               data=train, 
               method="class")

fancyRpartPlot(fit)


#Random Forests

# First need to take care of the missinga age values. Will build a function to predict them
summary(combined$Age)

Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + FamilySize,
                    data=combined[!is.na(combined$Age),], method="anova")

combined$Age[is.na(combined$Age)] <- predict(Agefit, combined[is.na(combined$Age),])

#checking summary to see if anything else stands out
summary(combined)

summary(combined$Embarked)

# will take the 2 blanks and convert them to S since there is so many of them already

which(combined$Embarked =="")

combined$Embarked[c(62,830)] <- "S"
combined$Embarked <- factor(combined$Embarked)

#other one was fare
summary(combined$Fare)

which(is.na(combined$Fare))

combined$Fare[1044] <- median(combined$Fare, na.rm=TRUE)

# Random forestes can only handle factors with 32 levels. Need to adjust family id

combined$FamilyID2 <- as.character(combined$FamilyID) 
combined$FamilyID2[combined$FamilySize <= 3] <- 'Small'
combined$FamilyID2 <- factor(combined$FamilyID2)

#break the data back into train and test sets
train <- combined[1:891,]
test <- combined[892:1309,]
#install random forest package
library(randomForest)

#set seed so we can reproduce results
set.seed(415)

#running the model
#we force the model to predict our classification by temporarily changing our target variable to a factor with only two levels using as.factor(). 
#The importance=TRUE argument allows us to inspect variable importance as we’ll see
#the ntree argument specifies how many trees we want to grow

fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID2,
                      data=train, importance=TRUE, ntree=2000)


# looking at the important variables
varImpPlot(fit)

#using out model to predict the test set
prediction <- predict(fit, test)

submit<- data.frame(PassengerId=test$PassengerId, Survived=prediction)
# write.csv(submit, file="firstforest.csv", row.names=FALSE)


# forest of conditional inference trees
library(party)

set.seed(415)

#Inference trees can handle more factors than Random Forests
 fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
                 data = train,  controls=cforest_unbiased(ntree=2000, mtry=3))

Prediction<- predict(fit, test, OOB=TRUE, type='response')

