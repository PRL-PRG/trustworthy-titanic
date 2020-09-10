TRAINDATA <- read.csv('../input/train.csv', stringsAsFactors = F)
TESTDATA  <- read.csv('../input/test.csv', stringsAsFactors = F)

TESTDATA$Survived <- NA
MERGEDATA <- rbind(TRAINDATA, TESTDATA)
head(MERGEDATA)

library(Amelia)
missmap(MERGEDATA, main = "Missing Values", col = c("yellow","black"), legend = F)

MERGEDATA <- within(MERGEDATA, rm("Cabin"))

which(is.na(MERGEDATA$Embarked))
table(MERGEDATA$Embarked)
MERGEDATA$Embarked[c(62, 830)] <- "S"

MERGEDATA$Fare[which(is.na(MERGEDATA$Fare))] <- median(MERGEDATA$Fare, na.rm = T)

library(rpart)
Agefit <- rpart(Age~Pclass+Sex+Fare+Embarked+SibSp+Parch, data= MERGEDATA[!is.na(MERGEDATA$Age),], method = "anova")
MERGEDATA$Age[is.na(MERGEDATA$Age)] <- predict(Agefit, MERGEDATA[is.na(MERGEDATA$Age),])

#split names
MERGEDATA$Name <- as.character(MERGEDATA$Name)
strsplit(MERGEDATA$Name[1], split='[,.]')[[1]][2]

MERGEDATA$Title <- sapply(MERGEDATA$Name, FUN = function(x){
strsplit(x, split='[,.]')[[1]][2]})
MERGEDATA$Title <- sub(' ','', MERGEDATA$Title)
table(MERGEDATA$Title)

MERGEDATA$Title[MERGEDATA$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
MERGEDATA$Title[MERGEDATA$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
MERGEDATA$Title[MERGEDATA$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
table(MERGEDATA$Title)
MERGEDATA$Title <- factor(MERGEDATA$Title)

##
str(MERGEDATA)
MERGEDATA[sapply(MERGEDATA, is.integer)] <- lapply(MERGEDATA[sapply(MERGEDATA, is.integer)], as.factor)
##
TRAINset <- MERGEDATA[1:891,]
TESTset <- MERGEDATA[892:1309,]

TRAINnum <- names(TRAINset)[which(sapply(TRAINset, is.numeric))]
TRAINnum <- TRAINset[,TRAINnum]
TRAINfact <- names(TRAINset)[which(sapply(TRAINset, is.factor))]
TRAINfact <- TRAINset[,TRAINfact]
TRAINset$InSurvive <- as.integer(TRAINset$Survived)

##plotting
library("GGally")
names(TRAINnum)
names(TRAINfact)
names(TRAINset)
ggpairs(data = TRAINnum, columns = c(1,2), title = "main")
ggpairs(data = TRAINfact, columns = c(2,3,4,5,6,8,9), title = "main")
ggpairs(data = TRAINset, columns = c(2,6,10), title = "main")

library(ggplot2)
##Age
g <- ggplot(TRAINset, aes(Age,InSurvive))
g+geom_point(aes(color=Sex), size= 2, alpha = 1/3)+geom_smooth()+facet_grid(.~Pclass)+labs(x = "Age", y= "Survived")

##Fare
g <- ggplot(TRAINset, aes(Fare,InSurvive))
g+geom_point(aes(color=Sex), size= 2, alpha = 1/3)+geom_smooth()+labs(x = "Fare", y= "Survived")+xlim(0,300)

TRAINDATA <- read.csv("train.csv", header = T, na.strings = c(""," ", "na", "NA"))
TESTDATA <- read.csv("test.csv", header = T, na.strings = c(""," ", "na", "NA"))
TESTDATA$Survived <- NA
MERGEDATA <- rbind(TRAINDATA, TESTDATA)
head(MERGEDATA)

library(Amelia)
missmap(MERGEDATA, main = "Missing Values", col = c("yellow","black"), legend = F)

MERGEDATA <- within(MERGEDATA, rm("Cabin"))

which(is.na(MERGEDATA$Embarked))
table(MERGEDATA$Embarked)
MERGEDATA$Embarked[c(62, 830)] <- "S"

MERGEDATA$Fare[which(is.na(MERGEDATA$Fare))] <- median(MERGEDATA$Fare, na.rm = T)

library(rpart)
Agefit <- rpart(Age~Pclass+Sex+Fare+Embarked+SibSp+Parch, data= MERGEDATA[!is.na(MERGEDATA$Age),], method = "anova")
MERGEDATA$Age[is.na(MERGEDATA$Age)] <- predict(Agefit, MERGEDATA[is.na(MERGEDATA$Age),])

#split names
MERGEDATA$Name <- as.character(MERGEDATA$Name)
strsplit(MERGEDATA$Name[1], split='[,.]')[[1]][2]

MERGEDATA$Title <- sapply(MERGEDATA$Name, FUN = function(x){
strsplit(x, split='[,.]')[[1]][2]})
MERGEDATA$Title <- sub(' ','', MERGEDATA$Title)
table(MERGEDATA$Title)

MERGEDATA$Title[MERGEDATA$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
MERGEDATA$Title[MERGEDATA$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
MERGEDATA$Title[MERGEDATA$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
table(MERGEDATA$Title)
MERGEDATA$Title <- factor(MERGEDATA$Title)

##
str(MERGEDATA)
MERGEDATA[sapply(MERGEDATA, is.integer)] <- lapply(MERGEDATA[sapply(MERGEDATA, is.integer)], as.factor)
##
TRAINset <- MERGEDATA[1:891,]
TESTset <- MERGEDATA[892:1309,]

TRAINnum <- names(TRAINset)[which(sapply(TRAINset, is.numeric))]
TRAINnum <- TRAINset[,TRAINnum]
TRAINfact <- names(TRAINset)[which(sapply(TRAINset, is.factor))]
TRAINfact <- TRAINset[,TRAINfact]
TRAINset$InSurvive <- as.integer(TRAINset$Survived)

##plotting
library("GGally")
names(TRAINnum)
names(TRAINfact)
names(TRAINset)
ggpairs(data = TRAINnum, columns = c(1,2), title = "main")
ggpairs(data = TRAINfact, columns = c(2,3,4,5,6,8,9), title = "main")
ggpairs(data = TRAINset, columns = c(2,6,10), title = "main")

library(ggplot2)
##Age
g <- ggplot(TRAINset, aes(Age,InSurvive))
g+geom_point(aes(color=Sex), size= 2, alpha = 1/3)+geom_smooth()+facet_grid(.~Pclass)+labs(x = "Age", y= "Survived")

##Fare
g <- ggplot(TRAINset, aes(Fare,InSurvive))
g+geom_point(aes(color=Sex), size= 2, alpha = 1/3)+geom_smooth()+labs(x = "Fare", y= "Survived")+xlim(0,300)
