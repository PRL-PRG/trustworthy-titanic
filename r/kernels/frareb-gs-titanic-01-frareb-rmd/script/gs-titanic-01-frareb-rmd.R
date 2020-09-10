## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# nothing for now


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bddTrain <- read.table("../input/train.csv", header = TRUE, 
  sep = ",", stringsAsFactors = FALSE)
bddTest <- read.table("../input/test.csv", header = TRUE, 
  sep = ",", stringsAsFactors = FALSE)
str(bddTrain)
str(bddTest)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# train
bddTrain$PassengerId <- as.character(bddTrain$PassengerId)
bddTrain$Survived <- as.logical(bddTrain$Survived)
bddTrain$Pclass <- as.factor(bddTrain$Pclass)
bddTrain$Sex <- as.factor(bddTrain$Sex)
bddTrain$Age <- as.integer(bddTrain$Age)
bddTrain$Embarked <- as.factor(bddTrain$Embarked)
str(bddTrain)

# test
bddTest$PassengerId <- as.character(bddTest$PassengerId)
bddTest$Pclass <- as.factor(bddTest$Pclass)
bddTest$Sex <- as.factor(bddTest$Sex)
bddTest$Age <- as.integer(bddTest$Age)
bddTest$Embarked <- as.factor(bddTest$Embarked)
str(bddTest)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# train
bddTrain$Cabin[bddTrain$Cabin == ""] <- NA
tmp <- as.character(bddTrain$Embarked)
tmp[tmp == ""] <- NA
bddTrain$Embarked <- as.factor(tmp)
rm(tmp)

# test
bddTest$Cabin[bddTest$Cabin == ""] <- NA
tmp <- as.character(bddTest$Embarked)
tmp[tmp == ""] <- NA
bddTest$Embarked <- as.factor(tmp)
rm(tmp)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getVarBarplot <- function(myVar, doSort = TRUE){
  meanS <- tapply(bddTrain$Survived, INDEX = bddTrain[myVar], 
    FUN = mean, na.rm = TRUE)
  if(doSort == TRUE){
    nS <- table(bddTrain[myVar])[order(meanS, decreasing = TRUE)]
    meanS <- meanS[order(meanS, decreasing = TRUE)]
  } else {
    nS <- table(bddTrain[myVar])
  }
  bplot <- barplot(meanS, xlab = myVar, ylab = "Mean survival rate")
  text(x = bplot, y = meanS, labels = nS, pos = 1)
}
getVarBarplot(myVar = "Pclass")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# this code is copied with small modifications from: 
# https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic
bddTrain$Title <- gsub('(.*, )|(\\..*)', '', bddTrain$Name)
bddTest$Title <- gsub('(.*, )|(\\..*)', '', bddTest$Name)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
# train
bddTrain$Title[bddTrain$Title == 'Mlle']        <- 'Miss' 
bddTrain$Title[bddTrain$Title == 'Ms']          <- 'Miss'
bddTrain$Title[bddTrain$Title == 'Mme']         <- 'Mrs' 
bddTrain$Title[bddTrain$Title %in% rare_title]  <- 'Rare Title'
table(bddTrain$Title)

# test
bddTest$Title[bddTest$Title == 'Mlle']        <- 'Miss' 
bddTest$Title[bddTest$Title == 'Ms']          <- 'Miss'
bddTest$Title[bddTest$Title == 'Mme']         <- 'Mrs' 
bddTest$Title[bddTest$Title %in% rare_title]  <- 'Rare Title'
table(bddTest$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getVarBarplot(myVar = "Title")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getVarBarplot(myVar = "Sex")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getVarBarplot(myVar = "Age", doSort = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bddTrain$Fsize <- bddTrain$SibSp + bddTrain$Parch
bddTest$Fsize <- bddTest$SibSp + bddTest$Parch
getVarBarplot(myVar = "Fsize", doSort = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cat(paste0("There are ", length(bddTrain$Ticket), " passengers and ", 
  length(unique(bddTrain$Ticket)), " tickets numbers."))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getVarBarplot(myVar = "Fare", doSort = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bddTrain$FareGp <- cut(bddTrain$Fare, c(0, 20, 50, 100, 300, 600))
bddTest$FareGp <- cut(bddTest$Fare, c(0, 20, 50, 100, 300, 600))
getVarBarplot(myVar = "FareGp", doSort = FALSE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bddTrain$CabLetter <- substr(bddTrain$Cabin, start = 1, stop = 1)
bddTest$CabLetter <- substr(bddTest$Cabin, start = 1, stop = 1)
getVarBarplot(myVar = "CabLetter")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bplot <- barplot(c(mean(bddTrain$Survived[is.na(bddTrain$CabLetter)]), 
  mean(bddTrain$Survived[!is.na(bddTrain$CabLetter)])), names.arg = 
    c("NA", "Cabin"))
text(x = bplot, y = c(mean(bddTrain$Survived[is.na(bddTrain$CabLetter)]), 
  mean(bddTrain$Survived[!is.na(bddTrain$CabLetter)])), labels = 
  c(length(bddTrain$Survived[is.na(bddTrain$CabLetter)]), 
    length(bddTrain$Survived[!is.na(bddTrain$CabLetter)])), pos = 1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bddTrain$CabLetterTF <- !is.na(bddTrain$CabLetter)
bddTest$CabLetterTF <- !is.na(bddTest$CabLetter)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(bddTrain$Embarked))
getVarBarplot(myVar = "Embarked")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(bddTrain)

