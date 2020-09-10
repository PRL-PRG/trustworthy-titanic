library(rpart)
library(rpart.plot)

titanic.train <- read.csv("../input/train.csv" , header = TRUE)
titanic.test <- read.csv("../input/test.csv", header = TRUE)

# typeof(titanic.train$PassengerId)
# typeof(titanic.train$Name)
# typeof(titanic.test$Sex)
# typeof(titanic.train$Survived)
# View(titanic.train)

#calculate the empty cells for AGE
#table(is.na(titanic.train$Age))["TRUE"]

titanic.train$IsTrain <- TRUE
titanic.test$IsTrain <- FALSE

#check the number of col and the col names
ncol(titanic.train)
ncol(titanic.test)
#check the col names
names(titanic.train)
names(titanic.test)

# add the missing column to titanic.test and assign it the default value of NA
titanic.test$Survived <- NA

#merge the two data sets to work with missing values
titanic.whole <- rbind(titanic.train, titanic.test)

tail(titanic.whole)
View(titanic.whole)

#check for missing values
table(is.na(titanic.whole$PassengerId))
table(is.na(titanic.whole$Survived)) # has missing value for test data set that we have to predict
table(is.na(titanic.whole$Pclass))
table(is.na(titanic.whole$Name))
table(is.na(titanic.whole$Sex))
table(is.na(titanic.whole$Age)) # has 263 missing values
table(is.na(titanic.whole$SibSp))
table(is.na(titanic.whole$Parch))
table(is.na(titanic.whole$Ticket))
table(is.na(titanic.whole$Fare)) # has 1 missing value
table(is.na(titanic.whole$Cabin))
table(is.na(titanic.whole$Embarked))

#handle missing values of age using linear regression model for age and predicting age

titanicage <- lm(Age ~. , data = titanic.whole)
summary(titanicage)
titanicage <- lm(Age ~ Survived + Pclass + Sex + SibSp + Parch + Fare + Embarked, data = titanic.whole)
summary(titanicage)
titanicage <- lm(Age ~ Survived + Pclass + Sex + SibSp + Parch + Fare , data = titanic.whole)
summary(titanicage)
titanicage <- lm(Age ~ Survived + Pclass + Sex + SibSp + Fare , data = titanic.whole)
summary(titanicage)
titanicage <- lm(Age ~ Survived + Pclass + SibSp + Fare , data = titanic.whole)
summary(titanicage)
titanicage <- lm(Age ~ Survived + Pclass + SibSp , data = titanic.whole)
summary(titanicage)

for (i in 1:nrow(titanic.whole)){
  if(is.na(titanic.whole[i, "Age"])){
    titanic.whole[i,"Age"] = predict(titanicage, titanic.whole[i,])
  }
}

#handle missing values using median
age.median <- median(titanic.whole$Age, na.rm = TRUE)
titanic.whole[is.na(titanic.whole$Age), "Age"] <- age.median

fare.median <- median(titanic.whole$Fare, na.rm = TRUE)
titanic.whole[is.na(titanic.whole$Fare), "Fare"] <- fare.median


#titanic.whole.training <- titanic.whole[1:891,]
titanic.whole.training <- titanic.whole[titanic.whole$IsTrain==TRUE,]
titanic.whole.test <- titanic.whole[titanic.whole$IsTrain==FALSE,]


trainrand <- runif(nrow(titanic.whole.training))
titanic.whole.training <- titanic.whole.training[order(trainrand),]
titanic.whole.training.train <- titanic.whole.training[1:round(nrow(titanic.whole.training)*0.75),]
titanic.whole.training.valid <- titanic.whole.training[round(nrow(titanic.whole.training)*0.75):nrow(titanic.whole.training) ,]


#titanic.whole.training[titanic.whole.training$Survived == 0, "Sur"] <- "Not Survived"
#titanic.whole.training[titanic.whole.training$Survived == 1, "Sur"] <- "Survived"

# the target variable is categorical, so we have to apply classification technique for prediction

#classification using decission tree

titanictree <- rpart(Survived ~ Age + Sex + SibSp + Parch + Embarked + Pclass + Fare, data = titanic.whole.training.train, method = "class")
rpart.plot(titanictree)
titanictree <- rpart(Survived ~ Age + Sex + SibSp + Parch + Embarked + Pclass , data = titanic.whole.training.train, method = "class")
rpart.plot(titanictree)

# predct the values of train
titanic.whole.training.train$pred <- predict(titanictree, titanic.whole.training.train, type = "class") #create a prediction using our tree
table(Actual = titanic.whole.training.train$Survived, Predicted = titanic.whole.training.train$pred) #create a confusion matrix

# calculate the accuracy/error rate
titanic.whole.training.train$correct <- titanic.whole.training.train$Survived == titanic.whole.training.train$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
traincorrectcount <- length(which(titanic.whole.training.train$correct))
trainincorrectcount <- nrow(titanic.whole.training.train) - traincorrectcount
trainerrorrate <- trainincorrectcount/nrow(titanic.whole.training.train)
trainaccuracy <- 1-trainerrorrate
trainaccuracy 
#although by inlcuding Fare the accuracy of training model increases 
#but the accuracy for validation set decreases becasue of overfitting

#now check for validation set

# predct the values of valid set
titanic.whole.training.valid$pred <- predict(titanictree, titanic.whole.training.valid, type = "class") #create a prediction using our tree
table(Actual = titanic.whole.training.valid$Survived, Predicted = titanic.whole.training.valid$pred) #create a confusion matrix

# calculate the accuracy/error rate
titanic.whole.training.valid$correct <- titanic.whole.training.valid$Survived == titanic.whole.training.valid$pred #create a new colum, TRUE if predicted = actual, otherwise FALSE
validcorrectcount <- length(which(titanic.whole.training.valid$correct))
validincorrectcount <- nrow(titanic.whole.training.valid) - validcorrectcount
validerrorrate <- validincorrectcount/nrow(titanic.whole.training.valid)
validaccuracy <- 1-validerrorrate
validaccuracy # by including Fare in decission tree classification model accuracy is incrasing by 2 percent


#now check for test set

# predct the vales of test
titanic.whole.test$pred <- predict(titanictree, titanic.whole.test, type = "class") #create a prediction using our tree
#View(titanic.whole.test)
write.csv(titanic.whole.test,file = "testPred.csv")