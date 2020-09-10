
# This R script will run on our backend. You can write arbitrary code here!

# Load packages
library(dplyr)
library(mice)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
library(e1071)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# Bind datasets together
full = bind_rows(train, test)

# Extract Title from name
full$Title = gsub('^(.*, )|(\\..*)$',"", full$Name)
full$Title[full$Title %in% c('Capt','Col','Don','Dona','Jonkheer','Lady','Major','Sir','the Countess')] = 'Catch All'
full$Title[full$Title %in% c('Mlle','Ms','Mme')] = 'Miss'

# Extract ticket length
full$Tlength = nchar(full$Ticket)

# Convert cabin to first letter or unknown
full$Cabin[full$Cabin == ""] = 'U'
full$CabinFirst = strtrim(full$Cabin,1)

# Add a U value to embarked for uknown
which(full$Embarked == "")   #This finds the missing values in Embarked
full[c(62,830),c(12)] = 'U'

# Convert variables to factors
full$Pclass = factor(full$Pclass)
full$Embarked = factor(full$Embarked)
full$Title = factor(full$Title)
full$CabinFirst = factor(full$CabinFirst)


# Impute the missing ages and fare
Isub = full[,c(3,5,6,7,8,9,10,12,13,14,15)]
imputed = complete(mice(Isub))
full$Age = imputed$Age
full$Fare = imputed$Fare

# Break into train and test
train = subset(full, !is.na(Survived))
test = subset(full, is.na(Survived))

# Run a glm model and create predictions
Tlog = glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Tlength+CabinFirst,data=train,family=binomial)
Tlogpred = predict(Tlog,test,type="response")
Tlogsub = data.frame(PassengerID = test$PassengerId, Survived = ifelse(Tlogpred >= 0.5,1,0))
write.csv(Tlogsub, file = 'log1_solution.csv', row.names = F)

# Run a glm model with only sig terms 
Tlog2 = glm(Survived~Pclass+SibSp+Parch+Embarked+Title,data=train,family=binomial)
Tlogpred2 = predict(Tlog2,test,type="response")
Tlogsub2 = data.frame(PassengerID = test$PassengerId, Survived = ifelse(Tlogpred2 >= 0.5,1,0))
write.csv(Tlogsub2,file = 'log2_solution.csv', row.names=F)

# Lets try a decision tree model
Tcart = rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Tlength+CabinFirst,data=train, method="class")
Tcartpred = predict(Tcart,test,type="class")
Tcartsub = data.frame(PassengerID = test$PassengerId, Survived = Tcartpred)
write.csv(Tcartsub,file = 'cart1_solution.csv', row.names=F)

# Lets apply some cross validation
numFolds = trainControl( method = "cv", number = 20 )
cpGrid = expand.grid( .cp = seq(0.001,0.5,0.001))
train(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Tlength+CabinFirst, data = train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
Tcart2 = rpart(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Tlength+CabinFirst,data=train, method="class", cp=0.002)  #0.002 was selected using the best accuracy of the train function
Tcartpred2 = predict(Tcart2,test,type="class")
Tcartsub2 = data.frame(PassengerID = test$PassengerId, Survived = Tcartpred2)
write.csv(Tcartsub2,file = 'cart2_solution.csv', row.names=F)

# Now lets try a random forest
Trf = randomForest(factor(Survived)~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Tlength+CabinFirst, data = train)
Trfpred = predict(Trf,test)
Trfsub = data.frame(PassengerID = test$PassengerId, Survived = Trfpred)
write.csv(Trfsub,file = 'rf1_solution.csv', row.names=F)