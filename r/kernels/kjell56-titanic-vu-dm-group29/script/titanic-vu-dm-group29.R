## ----load data-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data = read.csv('../input/train.csv', header=TRUE)
test = read.csv('../input/test.csv', header=TRUE)
library(dplyr)
library(ggplot2)
library(dplyr)
library(randomForest)
plot(data)


## ----plot------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data$PassengerId <- as.integer(data$PassengerId)
data$Survived <- as.logical(data$Survived)
data$Pclass <- ordered(data$Pclass, levels=c(1,2,3))
data$Name <- as.character(data$Name)
data$Sex <- as.factor(data$Sex)
data$Age <- as.numeric(data$Age)
data$SibSp <- as.integer(data$SibSp)
data$Parch <- as.integer(data$Parch)
data$Ticket <- as.character(data$Ticket)
data$Fare <- as.numeric(data$Fare)
data$Cabin <- as.character(data$Cabin)
data$Embarked <- as.factor(data$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(Pclass, fill = Survived)) + 
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(Sex, fill = Survived)) + 
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x=Age, fill=Survived)) + geom_histogram(position="dodge", bins = 9) + scale_x_continuous(breaks=seq(0,80, 10))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data[data$Sex=="female",], aes(x=Age, fill=Survived)) + geom_histogram(position="dodge", binwidth = 10) + scale_x_continuous(breaks=seq(0,70, 10))

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data[data$Sex=="male",], aes(x=Age, fill=Survived)) + geom_histogram(position="dodge", bins = 9) + scale_x_continuous(breaks=seq(0,80, 10))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data,aes(x=SibSp, fill=Survived)) + geom_histogram(position="dodge", binwidth=1) + scale_x_continuous(breaks=0:8)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data,aes(x=Parch, fill=Survived)) + geom_histogram(position="dodge", binwidth=1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data,aes(x=Parch+SibSp, fill=Survived)) + geom_histogram(position="dodge", binwidth=1) + scale_x_continuous(breaks=0:10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data[data$Sex=="male",],aes(x=Parch+SibSp, fill=Survived)) + geom_histogram(position="dodge", binwidth=1) + scale_x_continuous(breaks=0:10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data,aes(x=Fare, fill=Survived)) + geom_histogram(position="dodge", binwidth=50)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(data$Fare~data$Pclass)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data[data$Pclass==1,],aes(x=Fare, fill=Survived)) + geom_histogram(position="dodge", binwidth=20)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data[data$Sex=="male",],aes(x=Fare, fill=Survived)) + geom_histogram(position="dodge", binwidth=50) + scale_x_continuous(breaks=seq(0, 550,50))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data[data$Sex=="female",],aes(x=Fare, fill=Survived)) + geom_histogram(position="dodge", binwidth=50, breaks=seq(0,550, 50))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(data$Survived[data$Cabin!=""])
table(data$Survived[data$Cabin==""])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data,aes(x=Embarked, fill=Survived)) + geom_histogram(stat="count",position="dodge")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data$Name <- NULL
data$Ticket <- NULL
prediction = data.frame(PassengerId=data$PassengerId, Survived=data$Survived)
data$Cabin <- NULL
data$PassengerId <- NULL



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(data)
levels(data$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data$Embarked <- gsub('', which.max(data$Embarked), data$Embarked)
list_na <- colnames(data)[ apply(data, 2, anyNA) ]

##Generate mean
nonmissingage <- na.omit(data$Age) 
meanmissingage <- mean(nonmissingage)

##Replace values with mean
df_titanic_replace <- data %>%
   mutate(replace_mean_age  = ifelse(is.na(data$Age), meanmissingage, data$Age))

##Add family size
df_titanic_replace$familiy_size = data$SibSp+data$Parch

##Remove the age
df_titanic_replace$Age <- NULL


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Transform age to categorical
df_titanic_replace$Sex <- as.integer(df_titanic_replace$Sex)
df_titanic_replace[df_titanic_replace=='1C1'] = 1
df_titanic_replace[df_titanic_replace=='1Q1'] = 2
df_titanic_replace[df_titanic_replace=='1S1'] = 3

##Family size
ggplot(data, aes(df_titanic_replace$familiy_size, fill = df_titanic_replace$Survived)) + 
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

##Categorize the ages
df_titanic_replace$agecat = cut(df_titanic_replace$replace_mean_age, c(0,20,40,60,80,100), labels=c(1:5))
df_titanic_replace$replace_mean_age = NULL

##categorize the fare
df_titanic_replace$farecat = cut(df_titanic_replace$Fare, c(-1,0,50,100,150,200,600), labels=c(0:5))
df_titanic_replace$Fare = NULL

##Fareplot
ggplot(data, aes(df_titanic_replace$farecat, fill = df_titanic_replace$Survived)) + 
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")

##Ageplot
ggplot(data, aes(df_titanic_replace$agecat, fill = df_titanic_replace$Survived)) + 
  geom_bar(stat="count", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##remove the sibsp and parch
df_titanic_replace$SibSp = NULL
df_titanic_replace$Parch = NULL
df_titanic_replace$Embarked = as.factor(df_titanic_replace$Embarked)
head(df_titanic_replace)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test$PassengerId <- as.integer(test$PassengerId)
test$Pclass <- ordered(test$Pclass, levels=c(1,2,3))
test$Name <- as.character(test$Name)
test$Sex <- as.factor(test$Sex)
test$Age <- as.numeric(test$Age)
test$SibSp <- as.integer(test$SibSp)
test$Parch <- as.integer(test$Parch)
test$Ticket <- as.character(test$Ticket)
test$Fare <- as.numeric(test$Fare)
test$Embarked <- as.character(test$Embarked)
test$Name <- NULL
test$Ticket <- NULL
test$Cabin <- NULL

list_na <- colnames(test)[ apply(test, 2, anyNA) ]

##Generate age mean
nonmissingage <- na.omit(test$Age) 
meanmissingage <- mean(nonmissingage)

#$Generate fare mean
nomissinfare <- na.omit(test$Fare)
meanmissingfare <- mean(nomissinfare)

##Replace values with mean
df_titanic_replace_test <- test %>%
   mutate(replace_mean_age  = ifelse(is.na(test$Age), meanmissingage, test$Age),
          replace_mean_fare = ifelse(is.na(test$Fare), meanmissingfare, test$Fare))

##Add family size
df_titanic_replace_test$familiy_size = test$SibSp+test$Parch
df_titanic_replace_test$Age <- NULL

##Transform age to categorical
df_titanic_replace_test$Sex <- as.integer(df_titanic_replace_test$Sex)
df_titanic_replace_test[df_titanic_replace_test=='C'] = 1
df_titanic_replace_test[df_titanic_replace_test=='Q'] = 2
df_titanic_replace_test[df_titanic_replace_test=='S'] = 3

##Categorize the ages
df_titanic_replace_test$agecat = cut(df_titanic_replace_test$replace_mean_age, c(0,20,40,60,80,100), labels=c(1:5))
df_titanic_replace_test$replace_mean_age = NULL

##categorize the fare
df_titanic_replace_test$farecat = cut(df_titanic_replace_test$replace_mean_fare, c(-1,0,50,100,150,200,600), labels=c(0:5))
df_titanic_replace_test$Fare = NULL
df_titanic_replace_test$replace_mean_fare = NULL

##remove the sibsp and parch
df_titanic_replace_test$SibSp = NULL
df_titanic_replace_test$Parch = NULL

df_titanic_replace_test$Embarked = as.factor(df_titanic_replace_test$Embarked)
head(df_titanic_replace_test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Doing logistic regression
model <- glm(df_titanic_replace$Survived ~.,family=binomial(link = 'logit'),data=df_titanic_replace)
summary(model)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Predict on training set
baseAcur = 549 / (549 + 342)

predictTrain = predict(model, type = "response")
table(df_titanic_replace$Survived, predictTrain >= 0.5)

accuracy = (244 + 458) / nrow(df_titanic_replace)
sensitivity = 244 / (244 + 98)
specificity = 458 / (458 + 91)

cat("accuracy: ", accuracy, " > ", "baseline: ", baseAcur)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Predict on test set
predictLogistic = predict(model, type = "response", newdata = df_titanic_replace_test)

# no preference over error t = 0.5
df_titanic_replace_test$Survived = as.numeric(predictLogistic >= 0.5)
table(df_titanic_replace_test$Survived)

predictionsLog = data.frame(df_titanic_replace_test[c("PassengerId","Survived")])
write.csv(file = "TitanicLogisticPred", x = predictionsLog, row.names=FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Random forest
require(randomForest)
titanic.randomforest <- randomForest(df_titanic_replace$Survived ~.,
                      data=df_titanic_replace, 
                      importance=TRUE, 
                      ntree=20)
summary(titanic.randomforest)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Predict on training
baseAcur = 549 / (549 + 342)
predictTrain = predict(titanic.randomforest, type = "response")
table(df_titanic_replace$Survived, predictTrain >= 0.5)

accuracy = (229 + 487) / nrow(df_titanic_replace)
sensitivity = 229 / (229 + 113)
specificity = 487 / (487 + 62)
cat("accuracy: ", accuracy, " > ", "baseline: ", baseAcur)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Predict on test set
predictRandomForest <- predict(titanic.randomforest, df_titanic_replace_test)
df_titanic_replace_test$Survived = as.numeric(predictRandomForest >= 0.5)

predictionsFor = data.frame(df_titanic_replace_test[c("PassengerId","Survived")])
write.csv(file = "TitanicRandomForest", x = predictionsFor, row.names=FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
qqnorm(predictLogistic, main="P-Values Logistic")
qqnorm(predictRandomForest, main="P-Values Random Forests")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
##Do a test to test if difference models are significant
sigtest = wilcox.test(predictLogistic, predictRandomForest)
sigtest

