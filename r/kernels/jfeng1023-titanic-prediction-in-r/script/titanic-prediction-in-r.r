
# read data and stack training and testing datasets together
train <- read.csv('../input/train.csv')
test <- read.csv('../input/test.csv')
test$Survived <- NA
combined <- rbind(train, test)
summary(combined)
str(combined)

# change 'Survived' to factor
combined$Survived <- as.factor(combined$Survived)

# change the empty values in 'Embarked' to NA
levels(combined$Embarked)[1] <- NA

# relationship between different categorical variables and survival
with(train, table(Pclass, Survived)) # Pclass=1: more likely to survive
with(train, table(SibSp, Survived)) # SibSp=1: more likely to survive
with(train, table(Parch, Survived)) # Parch=1-3: more likely to survive
with(train, table(Embarked, Survived)) # Embarked=C: more likely to survive

# try creating a new variable, "family", which is the sum of SibSp and Parch
combined$Family <- combined$SibSp + combined$Parch
train$Family <- train$SibSp + train$Parch
with(train, table(Family, Survived)) # 1-3 family members: more likely to survive

# relationship between age, fare and survival
survivors <- subset(train, Survived==1)
victims <- subset(train, Survived==0)
summary(survivors$Age)
summary(victims$Age)
summary(survivors$Fare)
summary(victims$Fare)

# plot age
hist(survivors$Age, breaks=20, col='green', ylim=c(0,80))
par(new=TRUE)
hist(victims$Age, breaks=20, col='#FF000088', axes=FALSE)

# closer look at children (younger than 18)
hist(survivors$Age[which(survivors$Age<=18)], breaks=18, col='green', ylim=c(0,40))
par(new=TRUE)
hist(victims$Age[which(victims$Age<=18)], breaks=18, col='#FF000088', axes=FALSE, main='')

# plot fare (only look at fare <= 50 since there are very few expensive fares)
hist(survivors$Fare[which(survivors$Fare<=50)], breaks=20, col='green', ylim=c(0,200), freq=TRUE)
par(new=TRUE)
hist(victims$Fare[which(survivors$Fare<=50)], breaks=20, col='#FF000088', freq=TRUE, axes=FALSE)

# closer look at fare <= 10
hist(survivors$Fare[which(survivors$Fare<=10)], breaks=10, col='green', ylim=c(0,200), freq=TRUE)
par(new=TRUE)
hist(victims$Fare[which(survivors$Fare<=10)], breaks=10, col='#FF000088', freq=TRUE, axes=FALSE)

# extract title by finding an alphabetical string followed by '.' (e.g. Mr., Miss.)
library(stringr)
combined$title <- str_extract(combined$Name, '[a-zA-Z]+(?=\\.)')
combined$title <- as.factor(combined$title)
summary(combined$title)

# plot the age distribution by title
boxplot(Age~title, data=combined, las=2)

# group all the rare titles together
combined$title <- as.character(combined$title)
combined$title[!(combined$title %in% c('Master', 'Miss', 'Mr', 'Mrs'))] <- 'Other'
combined$title <- as.factor(combined$title)
summary(combined$title)

# get the mean age of each title group
title.age <- aggregate(Age~title, data=combined, median)
title.age

# impute missing ages
for(i in 1:nrow(combined)){
    if(is.na(combined[i,'Age'])){
        combined[i,'Age'] <- title.age$Age[which(title.age$title==combined[i,'title'])]
    }
}
summary(combined$Age)


# impute missing values in 'Fare' and 'Embarked'
library(mice)
init <- mice(combined, maxit=0) 
meth = init$method
predM = init$predictorMatrix

# remove the following variables as predictors
predM[, c("PassengerId", "Name","Ticket","Cabin")]=0    

# specify imputation methods
meth[c('Fare')]='norm'
meth[c('Embarked')]='polyreg'

# impute missing values and get new dataset
set.seed(500)
imputed <- mice(combined, method=meth, m=5, predictorMatrix = predM)
imputed <- complete(imputed)

summary(imputed)

# grouping age, fare, and family based on previous EDA
imputed$AgeGroup <- with(imputed, cut(Age, breaks=c(0,5,15,85),right=FALSE, labels=c('young child','older child','adult')))
summary(imputed$AgeGroup) 

imputed$FareGroup <- with(imputed, cut(Fare, breaks=c(0,7.5,600),right=FALSE,labels=c('low','high')))
summary(imputed$FareGroup) 

imputed$FamilySize <- with(imputed, cut(Family, breaks=c(0,1,4,11),right=FALSE,labels=c('solo','small','large')))
summary(imputed$FamilySize) 

# split traing and testing sets
tr <- imputed[1:nrow(train), ]
te <- imputed[(nrow(train)+1):nrow(imputed),]
summary(tr)
summary(te)

# model with decision tree 
library(caret)
library(C50)

tree <- C5.0(tr[,c('Sex', 'Age', 'Pclass', 'Family', 'SibSp', 'Parch', 'Fare', 'Embarked', 'title', 'AgeGroup', 'FareGroup', 'FamilySize')], tr$Survived)
summary(tree)

# use only the most useful features
tree2 <- C5.0(tr[,c('Sex', 'Pclass', 'Fare', 'Embarked', 'title', 'AgeGroup', 'FamilySize')], tr$Survived)
summary(tree2)

te$Survived <- predict(tree2, te[,c('Sex', 'Pclass', 'Fare', 'Embarked', 'title', 'AgeGroup', 'FamilySize')])
write.csv(te[,c('PassengerId','Survived')], file='tree2.csv', row.names=FALSE)

# recursive partitioning tree
library(rpart)

part <- rpart(Survived~Sex+Pclass+Fare+Embarked+title+AgeGroup+FamilySize, method="class", data=tr)
printcp(part) 
summary(part)

te$Survived <- predict(part, te, type="class")
write.csv(te[,c('PassengerId','Survived')], file='rpart.csv', row.names=FALSE)

# random forest
library(randomForest)
rf <- randomForest(Survived ~ ., data=tr[,c('Survived','Sex', 'Age', 'Pclass', 'Family', 'SibSp', 'Parch', 'Fare', 'Embarked', 'title', 'AgeGroup', 'FareGroup', 'FamilySize')], mtry=9, nodesize=6)
rf

te$Survived <- predict(rf, te[,c('Sex', 'Age', 'Pclass', 'Family', 'SibSp', 'Parch', 'Fare', 'Embarked', 'title', 'AgeGroup', 'FareGroup', 'FamilySize')])
write.csv(te[,c('PassengerId','Survived')], file='rf.csv', row.names=FALSE)

# SVM
library(e1071)
set.seed(2017)

svm.model <- svm(Survived ~ ., data=tr[,c('Survived','Sex', 'Age', 'Pclass', 'Family', 'SibSp', 'Parch', 'Fare', 'Embarked', 'title', 'AgeGroup', 'FareGroup', 'FamilySize')], kernel ="radial")
svm.model

te$Survived <- predict(svm.model, te[,c('Sex', 'Age', 'Pclass', 'Family', 'SibSp', 'Parch', 'Fare', 'Embarked', 'title', 'AgeGroup', 'FareGroup', 'FamilySize')])
write.csv(te[,c('PassengerId','Survived')], file='svm.csv', row.names=FALSE)
