
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test1  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)


#Adding column if data is training dataset 
train$isTrain<- TRUE
test1$isTrain<- FALSE

#Adding Survived column into test dataset
test1$Survived<- NA

#Combined both datasets
titanicFull<-rbind(train,test1)

#Handling NA values, using median of corresponding attribute 
m1<-median(titanicFull$Age, na.rm = T)
titanicFull[is.na(titanicFull$Age),"Age"] <- m1

titanicFull[titanicFull$Embarked=="","Embarked"]<-"S"

m2<- median(titanicFull$Fare, na.rm=T)
titanicFull[is.na(titanicFull$Fare),"Fare"] <- m2

#Converting data into factors
titanicFull$Pclass <- as.ordered(titanicFull$Pclass)
titanicFull$Sex <- as.factor(titanicFull$Sex)
titanicFull$Embarked <- as.factor(titanicFull$Embarked)

#Separting training and test dataset
train<-titanicFull[titanicFull$isTrain== TRUE,]
test1<-titanicFull[titanicFull$isTrain== FALSE,]

#Converting Survived into factor for training
train$Survived<- as.factor(train$Survived)


#Applying randomForest on training data to build a model
fml<- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Fare"
fml<-as.formula(fml)
titanic.model<-randomForest(fml,train, ntree=500, mtry=3, nodesize=0.01*nrow(train) )

#Predicting for 
Survived<-predict(titanic.model,test1)

#Writing into Excel
PassengerId<- test1$PassengerId
op<-as.data.frame(PassengerId)

op$Survived <- Survived

write.csv(op, file= "Titanic_socio.csv", row.names = F)

