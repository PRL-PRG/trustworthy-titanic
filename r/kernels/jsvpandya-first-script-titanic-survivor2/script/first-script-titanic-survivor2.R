TRAINDATA <- read.csv('../input/train.csv', stringsAsFactors = F)
TESTDATA  <- read.csv('../input/test.csv', stringsAsFactors = F)

TESTDATA$Survived <- NA
MERGEDATA <- rbind(TRAINDATA, TESTDATA)

MERGEDATA <- within(MERGEDATA, rm("Cabin"))

which(is.na(MERGEDATA$Embarked))
MERGEDATA$Embarked[c(62, 830)] <- "S"

MERGEDATA$Fare[which(is.na(MERGEDATA$Fare))] <- median(MERGEDATA$Fare, na.rm = T)

library(rpart)
Agefit <- rpart(Age~Pclass+Sex+Fare+Embarked+SibSp+Parch, data= MERGEDATA[!is.na(MERGEDATA$Age),], method = "anova")
MERGEDATA$Age[is.na(MERGEDATA$Age)] <- predict(Agefit, MERGEDATA[is.na(MERGEDATA$Age),])

TRAINset <- MERGEDATA[1:891,]
TESTset <- MERGEDATA[892:1309,]

library(ggplot2)
#Age
g <- ggplot(TRAINset, aes(Age,Survived))
g+geom_point(aes(color=Sex), size= 2, alpha = 1/3)+geom_smooth()+facet_grid(.~Pclass)+labs(x = "Age", y= "Survived")

##Fare
g1 <- ggplot(TRAINset, aes(Fare, Survived))
g1+geom_point(aes(color=Sex), size= 2, alpha = 1/3)+geom_smooth(method = "lm")+labs(x = "Fare", y= "Survived")+xlim(0,75)+facet_grid(.~Embarked)



















