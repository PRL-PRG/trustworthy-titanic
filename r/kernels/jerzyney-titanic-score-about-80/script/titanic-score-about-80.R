## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2); library(caret); library(Hmisc)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData <- read.csv("../input/train.csv", header = TRUE, stringsAsFactors = FALSE)
testData <- read.csv("../input/test.csv", header = TRUE, stringsAsFactors = FALSE)
inTrain <- createDataPartition(y=trainData$Survived, p=0.8, list=FALSE)
trainDataTest <- trainData[-inTrain,]
trainData <- trainData[inTrain,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(trainData)
summary(trainData$Age)


## ----pressure, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(density(trainData$Age, na.rm = TRUE), main="Age density")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t1 <- table(trainData$Survived, trainData$Sex)
barplot(t1, xlab = "Gender", ylab = "Number of People", main = "survived and deceased between male and female")
prop.table(t1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cutAge <- cut2(trainData$Age, g=6)
tableAge <- table(trainData$Survived, cutAge)
prop.table(tableAge)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(trainData$Survived[is.na(trainData$Age)])/sum(is.na(trainData$Age))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t2 <- table(trainData$Survived,trainData$Pclass)
t2/rep(apply(t2, 2, sum), each=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$hasCabin <- "noCabin"
for(i in 1:length(trainData$Survived)){
  if(trainData$Cabin[i]!="") trainData$hasCabin[i] <- "hasCabin"
}
table(trainData$Survived, trainData$hasCabin)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainDataTest$hasCabin <- "noCabin"
for(i in 1:length(trainDataTest$Survived)){
  if(trainDataTest$Cabin[i]!="") trainDataTest$hasCabin[i] <- "hasCabin"
}

testData$hasCabin <- "noCabin"
for(i in 1:length(testData$PassengerId)){
  if(testData$Cabin[i]!="") testData$hasCabin[i] <- "hasCabin"
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cutFare <- cut2(trainData$Fare, g=5)
t3 <- table(trainData$Survived, cutFare)
prop.table(t3)
barplot(t3, xlab="Fare", ylab="count", main="Survival by Fare")
median(trainData$Fare)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$Sex <- gsub("female", 1, trainData$Sex)
trainData$Sex <- gsub("^male", 0, trainData$Sex)
trainData$Sex <- as.numeric(trainData$Sex)
trainDataTest$Sex <- gsub("female", 1, trainDataTest$Sex)
trainDataTest$Sex <- gsub("^male", 0, trainDataTest$Sex)
trainDataTest$Sex <- as.numeric(trainDataTest$Sex)
testData$Sex <- gsub("female", 1, testData$Sex)
testData$Sex <- gsub("^male", 0, testData$Sex)
testData$Sex <- as.numeric(testData$Sex)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
places <- c("S", "C", "Q")
trainData$Embarked <- match(trainData$Embarked, places)
trainDataTest$Embarked <- match(trainDataTest$Embarked, places)
testData$Embarked <- match(testData$Embarked, places)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(trainData$Name)
createTitles <- function(names){
  namesSplit <- strsplit(names, " ")
  commonTitles <- c("Col.", "Dr.", "Major", "Master", "Miss.", "Mr.", "Mrs.", "Rev.")
  titles <- ""
  for(i in 1:length(namesSplit)){
    if(is.na(match(namesSplit[[i]][2], commonTitles))){
      titles <- c(titles,"UT")
    }
       else{
    titles <- c(titles, namesSplit[[i]][2])
       }
  }
  return(as.factor(titles[-1]))
}
Titles <- createTitles(trainData$Name)
summary(Titles)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$Title <- Titles
trainDataTest$Title <- createTitles(trainDataTest$Name)
testData$Title <- createTitles(testData$Name)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$Family <- trainData$SibSp+trainData$Parch
trainDataTest$Family <- trainDataTest$SibSp+trainDataTest$Parch
testData$Family <- testData$SibSp+testData$Parch


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#note that the fuction returns the full ages vector containing the infered values
ageInference <- function(allAges, allTitles){
  for (i in 1:length(allAges)){
    if(is.na(allAges[i])){
      tmpTitle <- allTitles[i]
      allAges[i] <- round(mean(allAges[allTitles == tmpTitle], na.rm=TRUE), digits=2)
    }
  }
  #if there are still missing values, just replace them by the overall mean:
  for(i in 1:length(allAges)){
    if(is.na(allAges[i])) allAges[i] <- round(mean(allAges, na.rm=TRUE), digits=2)
  }
  return(allAges)
}
ageInf <- ageInference(trainData$Age, trainData$Title)
summary(ageInf)
summary(trainData$Age)
trainData$AgeInf <- ageInf
ggplot()+
  geom_density(data=trainData, aes(x=Age), color="red")+
  geom_density(data=trainData, aes(x=AgeInf), color="green")

trainDataTest$AgeInf <- ageInference(trainDataTest$Age, trainDataTest$Title)
testData$AgeInf <- ageInference(testData$Age, testData$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(trainData)
summary(trainDataTest)
summary(testData)
plot(as.factor(trainData$Embarked))

#replace missing Embarked by 1

trainData$Embarked[is.na(trainData$Embarked)] <- 1
trainDataTest$Embarked[is.na(trainDataTest$Embarked)] <- 1
testData$Embarked[is.na(testData$Embarked)] <- 1

#replace missing Fare by overall mean
trainData$Fare[is.na(trainData$Fare)] <- round(mean(trainData$Fare, na.rm=TRUE), digits=2)
trainDataTest$Fare[is.na(trainDataTest$Fare)] <- round(mean(trainDataTest$Fare, na.rm=TRUE), digits=2)
testData$Fare[is.na(testData$Fare)] <- round(mean(testData$Fare, na.rm=TRUE), digits=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$Survived <- as.factor(trainData$Survived)
trainDataTest$Survived <- as.factor(trainDataTest$Survived)

control <- trainControl(method="cv", number=5)
mod1 <- train(trainData$Survived ~ Pclass + Sex + AgeInf + SibSp + Parch + Fare + Embarked + Title + Family + hasCabin, data=trainData, method="rf", trControl=control)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(varImp(mod1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred1 <- predict(mod1, newdata=trainDataTest)
confMat <- confusionMatrix(pred1, trainDataTest$Survived)
confMat$overall
confMat$table

