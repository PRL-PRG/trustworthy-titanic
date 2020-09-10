## ---- message = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load packages
library('ggplot2') # visualization
library('dplyr') # data manipulation
library('rpart') # prediction model




## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)
full  <- bind_rows(train, test) # bind training & test data



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full[1:891,], aes(x=Age, fill=factor(Survived))) +
geom_bar(stat='count', position='dodge')



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

full$AgeGroup[full$Age > 0 & full$Age <=10] <- '0-10'
full$AgeGroup[full$Age > 10 & full$Age <=20] <- '10-20'
full$AgeGroup[full$Age > 20 & full$Age <=30] <- '20-30'
full$AgeGroup[full$Age > 30 & full$Age <=40] <- '30-40'
full$AgeGroup[full$Age > 40 & full$Age <=50] <- '40-50'
full$AgeGroup[full$Age > 50 & full$Age <=60] <- '50-60'
full$AgeGroup[full$Age > 60 & full$Age <=70] <- '60-70'
full$AgeGroup[full$Age > 70 & full$Age <=80] <- '70-80'
full$AgeGroup[full$Age > 80 & full$Age <=90] <- '80-90'
full$AgeGroup[full$Age > 90 & full$Age <=100] <- '90-100'
full$AgeGroup[full$Age == ''] <- 'Unknown'

ggplot(full[1:891,], aes(x=AgeGroup, fill=factor(Survived))) +
geom_bar(stat='count', position='dodge')



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$AgeType[full$Age > 0 & full$Age <=10] <- 'Child'
full$AgeType[full$Age > 10 & full$Age <=120] <- 'Adult'

full$AgeType[(is.na(full$Age)) & grepl('Master.', full$Name)] <- 'Child'
full$AgeType[(is.na(full$Age)) & grepl('Miss.', full$Name)&(full$Parch>0)] <- 'Child'
full$AgeType[(is.na(full$Age)) & grepl('Miss.', full$Name)&(full$Parch==0)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Ms.', full$Name)&(full$Parch>0)] <- 'Child'
full$AgeType[(is.na(full$Age)) & grepl('Ms.', full$Name)&(full$Parch==0)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Mlle.', full$Name)&(full$Parch>0)] <- 'Child'
full$AgeType[(is.na(full$Age)) & grepl('Mlle.', full$Name)&(full$Parch==0)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Mrs.', full$Name)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Mr.', full$Name)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Dr.', full$Name)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Rev.', full$Name)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Mme.', full$Name)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Major.', full$Name)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Col.', full$Name)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Capt.', full$Name)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Dona.', full$Name)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Jonkheer.', full$Name)] <- 'Adult'
full$AgeType[(is.na(full$Age)) & grepl('Countess.', full$Name)] <- 'Adult'


ggplot(full[1:891,], aes(x=AgeType, fill=factor(Survived))) +
geom_bar(stat='count', position='dodge')


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full$familySize <- full$SibSp + full$Parch



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full[1:891,], aes(x=familySize, fill=factor(Survived))) +
geom_bar(stat='count', position='dodge') +
scale_x_continuous(breaks=c(1:11)) 

missFamilySize <- sum(is.na(full$familySize))
missFamilySize



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ggplot(full[1:891,], aes(x=Pclass, fill=factor(Survived))) +
geom_bar(stat='count', position='dodge') +
scale_x_continuous(breaks=c(1:11)) 



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

missEmb <- sum(is.na(full$Embarked))
missEmb

full$Name[is.na(full$Embarked)]

ggplot(full[1:891,], aes(x=Embarked, fill=factor(Survived))) +
geom_bar(stat='count', position='dodge')


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


missGen <- sum(is.na(full$Sex))
missGen

ggplot(full[1:891,], aes(x=Sex, fill=factor(Survived))) +
geom_bar(stat='count', position='dodge')



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- full[1:891,]
test <- full[892:1309,]

survival_model <- rpart(factor(Survived) ~ AgeType + familySize + Pclass + Embarked + Sex, data = train)
plot(survival_model, margin=0.1)
text(survival_model, use.n=TRUE, pretty=TRUE, cex=0.8)

predict_survival <- predict(survival_model, test, type = "class")

solution <- data.frame(PassengerID = test$PassengerId, Survived = predict_survival)
write.csv(solution, file = 'survival_model_solution.csv', row.names = F)



