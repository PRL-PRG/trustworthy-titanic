## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.train <- read.csv("../input/train.csv");
titanic.test  <- read.csv("../input/test.csv");


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(titanic.train)
tail(titanic.train)
summary(titanic.train)
str(titanic.train)


head(titanic.test)
tail(titanic.test)
summary(titanic.test)
str(titanic.test) #no survived variable in the test set


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
names(titanic.train)
names(titanic.test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.test$Survived <- NA


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.full <- rbind(titanic.train , titanic.test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(titanic.full$IsTrainSet)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(titanic.full$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.full[titanic.full$Embarked == "",] 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(titanic.full$Sex,titanic.full$Embarked)
table(titanic.full$Pclass,titanic.full$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.full[titanic.full$Embarked == "","Embarked"] <- "S"
table(titanic.full$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(is.na(titanic.full$Age))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
median(titanic.full$Age,na.rm = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.males <- titanic.full[titanic.full$Sex == "male",]

median(titanic.males$Age,na.rm = TRUE)

medianAgeMale <- median(titanic.males$Age,na.rm = TRUE)

titanic.full[is.na(titanic.full$Age)&titanic.full$Sex == 'male',
             "Age"] <- medianAgeMale

titanic.females <- titanic.full[titanic.full$Sex == "female",]

median(titanic.females$Age,na.rm = TRUE)

medianAgeFemale <- median(titanic.females$Age,na.rm = TRUE)

titanic.full[is.na(titanic.full$Age)&titanic.full$Sex == 'female',
             "Age"] <- medianAgeFemale


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(is.na(titanic.full$Age))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(is.na(titanic.full$Fare))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library("rpart")

library("rpart.plot")

fare_dt <- rpart(Fare ~ Pclass + Parch + Sex + SibSp,
                 data=titanic.full)

rpart.plot(fare_dt, type=1, extra=101)

MissingFare <- titanic.full[is.na(titanic.full$Fare),]
MissingFare

titanic.full[is.na(titanic.full$Fare),"Fare"] <- 12


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(is.na(titanic.full$Fare))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(titanic.full)
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE,]
nrow(titanic.train)
nrow(titanic.test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.train$Survived <- as.factor(titanic.train$Survived)
str(titanic.train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
g <- ggplot(data=titanic.train, aes(x=Sex,fill=Survived)) + geom_bar()+
  theme_grey(base_size = 14)
g

g <- ggplot(data=titanic.train, aes(x=Pclass,fill=Survived)) + geom_bar()+
  theme_grey(base_size = 14)
g

g <- ggplot(data=titanic.train, aes(x=Embarked,fill=Survived)) + geom_bar() +
  theme_grey(base_size = 14)
g

g <- ggplot(data=titanic.train, aes(x=SibSp,fill=Survived)) + geom_bar() +
  theme_grey(base_size = 14)
g

g <- ggplot(data=titanic.train, aes(x=Age)) + geom_density()
g

g <- ggplot(data=titanic.train, aes(x=Fare)) + geom_density()+
  facet_grid(~Pclass)
g

g <- ggplot(data=titanic.train, aes(x=Pclass,y=Fare))+
  geom_boxplot()

g

# Jitter Plot by Class and Survived
g <- ggplot(data=titanic.train, aes(x=Pclass,y=Fare,
                                    color=Survived))+ geom_jitter() + ggtitle("Titanic Survival by Class")

g


# Jitter Plot by Class, Gender and Survived
#Same chart but for Males only
g <- ggplot(data=titanic.train[titanic.train$Sex == "male",], aes(x=Pclass,y=Fare,
                                    color=Survived))+ geom_jitter() + ggtitle("Titanic Survival by Class: Males Only")

g


#Same chart but for Females only
g <- ggplot(data=titanic.train[titanic.train$Sex == "female",], aes(x=Pclass,y=Fare,
                                    color=Survived))+ geom_jitter() + ggtitle("Titanic Survival by Class: Females Only")

g






## ----message=FALSE, warnings=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library("randomForest")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)


titanic.model <- randomForest(formula=survived.formula,
             data=titanic.train,
             ntree = 500,
             mtry = 3,
             nodesize = 0.01*nrow(titanic.train)
             )

Survived <- predict(titanic.model,newdata=titanic.test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)

output.df$Survived <- Survived

write.csv(output.df,"titanic_kaggle_submission.csv",row.names = FALSE)

