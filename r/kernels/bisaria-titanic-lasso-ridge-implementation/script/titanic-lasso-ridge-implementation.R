## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
titanic.train <- read.csv("../input/train.csv", stringsAsFactor=FALSE)
titanic.test <- read.csv("../input/test.csv", stringsAsFactor=FALSE)



## ----message=F,warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(plyr)
library(rpart)
library(caret)
library(caTools)
library(mice)
library(stringr)
library(Hmisc)
library(ggplot2)
library(vcd)
library(ROCR)
library(pROC)
library(VIM)
library(glmnet)    


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(titanic.train)
summary(titanic.train)
table(titanic.train$Survived, titanic.train$Sex)


## ---- warning=FALSE,fig.height=3,fig.width=7-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic.train, aes(x=Age, y=PassengerId, color = as.factor(Survived))) +                      
    geom_point() + 
    facet_grid(Sex ~.) +
    ggtitle("Survival vs Passenger's Age")+
    xlab("Age") + 
    theme(legend.position = "none")+
    scale_colour_manual(values = c("#FF0000","#0000FF"))


## ---- warning=FALSE,fig.height=3,fig.width=7-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic.train[titanic.train$Embarked != "",], aes(x=Embarked, y=PassengerId)) +  
  geom_tile(aes(fill = as.factor(Survived))) + 
  facet_grid(. ~ Pclass) +
  ggtitle("Survival vs Passenger's Pclass and Port of Embarkation")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#FF0000","#0000FF"))


## ---- warning=FALSE,fig.height=3,fig.width=7-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(titanic.train[titanic.train$Embarked != "",], aes(x=Embarked, y=PassengerId)) +  
  geom_tile(aes(fill = as.factor(Survived))) + 
  facet_grid(. ~ Sex) +
  ggtitle("Survival vs Passenger's Sex and Port of Embarkation")+
  theme(legend.position = "none")+
  scale_fill_manual(values = c("#FF0000","#0000FF"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mosaic(~ Sex + (Age > 15) + (SibSp + Parch > 0) + Survived, data = titanic.train[complete.cases(titanic.train),],
       shade=T, legend=T)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Survived = titanic.train$Survived
titanic.test$Survived = NA
all = rbind(titanic.train, titanic.test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Title = sapply(all$Name,function(x) strsplit(x,', ')[[1]][2])
all$Title = sapply(all$Title,function(x) strsplit(x,'\\. ')[[1]][1])

as.data.frame(
  cbind("Title" = unique(all$Title), 
        "No_of_passengers" = sapply(unique(all$Title), function(x) nrow(all[all$Title == x,])),
        "Age_missing" = sapply(unique(all$Title), function(x) nrow(all[all$Title == x & is.na(all$Age),])),
        "Minimum_Age" = sapply(unique(all$Title), function(x) min(all[all$Title == x,'Age'], na.rm = TRUE)),
        "Maximum_Age" = sapply(unique(all$Title), function(x) max(all[all$Title == x,'Age'], na.rm = TRUE))), row.names = F)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#   Mr:     For men above 14.5 years
#   Master: For boys below and equal to 14.5 years
#   Miss:   For girls below and equal to 14.5 years
#   Ms:     For women above 14.5 years, maybe unmarried
#   Mrs:    For married women above 14.5 years


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

all[(all$Title == "Mr" & all$Age <= 14.5 & !is.na(all$Age)),]$Title = "Master"

all[all$Title == "Capt"|
    all$Title == "Col"|
    all$Title == "Don"|
    all$Title == "Major"|
    all$Title == "Rev"|      
    all$Title == "Jonkheer"|
    all$Title == "Sir",]$Title = "Mr"

# None of these women are travelling with family, hence can be categorised as single women for this analysis
all[all$Title == "Dona"|
      all$Title == "Mlle"|
      all$Title == "Mme",]$Title = "Ms"

# Categories Lady and Countess as a married woman
all[all$Title == "Lady"| all$Title == "the Countess",]$Title = "Mrs"

# Categorise doctors as per their sex
all[all$Title == "Dr" & all$Sex == "female",]$Title = "Ms"
all[all$Title == "Dr" & all$Sex == "male",]$Title = "Mr"



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Title = as.factor(all$Title)
all$Title <- droplevels(all$Title)
summary(all$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$FamilySize = ifelse(all$SibSp + all$Parch + 1 <= 3, 1,0) # Small = 1, Big = 0


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Mother = ifelse(all$Title=="Mrs" & all$Parch > 0, 1,0)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Single = ifelse(all$SibSp + all$Parch + 1 == 1, 1,0) # People travelling alone


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$FamilyName = sapply(all$Name,function(x) strsplit(x,', ')[[1]][1])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Family.Ticket = all[all$Single == 0,c("FamilyName", "Ticket")]
Family.Ticket = Family.Ticket[order(Family.Ticket$FamilyName),]
head(Family.Ticket)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

all$FamilyName  = paste(all$FamilyName , str_sub(all$Ticket,-3,-1), sep="")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$FamilySurvived = 0
# Dataset of passengers with family
Families = all[(all$Parch+all$SibSp) > 0,]

# Group families by their family name and number of survivals in the family
Survival.GroupByFamilyName = aggregate(as.numeric(Families$Survived), by=list("FamilyName" = Families$FamilyName), FUN=sum, na.rm=TRUE)

# Family is considered to have survived if atleast one member survived
FamilyWithSurvival = Survival.GroupByFamilyName[Survival.GroupByFamilyName$x > 0,]$FamilyName
all[apply(all, 1, function(x){ifelse(x["FamilyName"] %in% FamilyWithSurvival,TRUE,FALSE)}),]$FamilySurvived = 1



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$AgeClass = ifelse(all$Age<=10,1,
                      ifelse(all$Age>10 & all$Age<=20,2,
                             ifelse(all$Age>20 & all$Age<=35,3,4)))
all$AgeClass = as.factor(all$AgeClass)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Pclass = as.factor(all$Pclass)
all$Sex = as.factor(all$Sex)
all[all$Embarked == "",]$Embarked = NA
all$Embarked = as.factor(all$Embarked)
all[all$Cabin == "",]$Cabin = NA
all$Cabin = as.factor(all$Cabin)
all$FamilySize = as.factor(all$FamilySize)
all$Mother = as.factor(all$Mother)
all$Single = as.factor(all$Single)
all$FamilyName = as.factor(all$FamilyName)

md.pattern(all[,!names(all) %in% c("Survived", "Name", "PassengerId", "Ticket", "AgeClass")])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Embarked[is.na(all$Embarked)] = 'S'



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fit.Fare = rpart(Fare ~ Pclass + SibSp + Parch + Age + Embarked + Title, 
                 data = all[!is.na(all$Fare),],
                 method = "anova")
all$Fare[is.na(all$Fare)] = predict(fit.Fare, newdata = all[is.na(all$Fare), ])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
marginplot(data.frame(all$Age, all$Pclass))


## ---- warning=FALSE,results='hide'-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

ageData <- mice(all[, !names(all) %in% c("Survived", "Name", "PassengerId", "Ticket", "AgeClass", "Cabin", "FamilyName")],m=8,maxit=8,meth='pmm',seed=251863)

## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Check out the imputed data
head(ageData$imp$Age)

# Check if the imputed data distribution follows the existing age distribution.
ggplot(all,aes(x=Age)) + 
  geom_density(data=data.frame(all$PassengerId, complete(ageData,6)), alpha = 0.2, fill = "blue")+
  geom_density(data=all, alpha = 0.2, fill = "Red")+
  labs(title="Age Distribution")+
  labs(x="Age")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Sixth imputed data is picked up for further analysis based on the density distribution
all.imp <- data.frame(all$PassengerId, complete(ageData,6))

all$Age = all.imp$Age

all[is.na(all$AgeClass),]$AgeClass = ifelse(all[is.na(all$AgeClass),]$Age<=10,1,
                      ifelse(all[is.na(all$AgeClass),]$Age>10 & all[is.na(all$AgeClass),]$Age<=20,2,
                             ifelse(all[is.na(all$AgeClass),]$Age>20 & all[is.na(all$AgeClass),]$Age<=35,3,4)))

# All women above age of 14.5, with title Miss are to be recategorised as Ms.
all[all$Title == "Miss" & all$Age > 14.5,]$Title = "Ms"

# Check if titles and age are as required.
table(all$Title, all$Age > 14.5)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extract single alphabet prefixed to each cabin number provided. Each of these letters represent the part of the deck were these cabins were located.
all$CabinNo = sapply(all$Cabin,function(x) substr(x,1,1))
all$CabinNo[all$CabinNo == ""] = NA
table(is.na(all$CabinNo))

# Dataset of all families with cabin data
familyWithCabinNo = unique(all[!is.na(all$CabinNo) & all$SibSp + all$Parch > 0,c("FamilyName", "CabinNo")])
head(familyWithCabinNo)

# Function to check if these people are travelling with family 
checkIfHasCabin <- function(familyName, CabinNo){   
  ifelse (familyName %in% familyWithCabinNo$FamilyName, familyWithCabinNo$CabinNo, CabinNo)      
}

# Assign same cabin number to those members of a single family, whose cabin number is missing 
all[is.na(all$CabinNo),]$CabinNo = apply(all[ is.na(all$CabinNo),c("FamilyName", "CabinNo")], 1, function(y) checkIfHasCabin(y["FamilyName"], y["CabinNo"]))

table(is.na(all$CabinNo))
table(all$CabinNo, all$Pclass)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Note: This procedure has been taken from script submitted on Kaggle.

# for first class obs
A.1 = round(22/(323-65) * 65)
B.1 = round(65/(323-65) * 65)
C.1 = round(96/(323-65) * 65)
D.1 = round(40/(323-65) * 65)
E.1 = 65 - (A.1+B.1+C.1+D.1)
# for second class
D.2 = round(6/(277-254) * 254)
E.2 = round(4/(277-254) * 254)
F.2 = 254 - (D.2+E.2)
# for third class
E.3 = round(3/(709-691) * 691)
F.3 = round(8/(709-691) * 691)
G.3 = 691 - (E.3+F.3)

set.seed(0)
all[ sample( which( all$Pclass==1 & is.na(all$CabinNo)), A.1 ) , "CabinNo"] <- rep("A", A.1)
all[ sample( which( all$Pclass==1 & is.na(all$CabinNo)), B.1 ) , "CabinNo"] <- rep("B", B.1)
all[ sample( which( all$Pclass==1 & is.na(all$CabinNo)), C.1 ) , "CabinNo"] <- rep("C", C.1)
all[ sample( which( all$Pclass==1 & is.na(all$CabinNo)), D.1 ) , "CabinNo"] <- rep("D", D.1)
all[ sample( which( all$Pclass==1 & is.na(all$CabinNo)), E.1 ) , "CabinNo"] <- rep("E", E.1)

set.seed(0)
all[ sample( which( all$Pclass==2 & is.na(all$CabinNo)), D.2 ) , "CabinNo"] <- rep("D", D.2)
all[ sample( which( all$Pclass==2 & is.na(all$CabinNo)), E.2 ) , "CabinNo"] <- rep("E", E.2)
all[ sample( which( all$Pclass==2 & is.na(all$CabinNo)), F.2 ) , "CabinNo"] <- rep("F", F.2)

set.seed(0)
all[ sample( which( all$Pclass==3 & is.na(all$CabinNo)), E.3 ) , "CabinNo"] <- rep("E", E.3)
all[ sample( which( all$Pclass==3 & is.na(all$CabinNo)), F.3 ) , "CabinNo"] <- rep("F", F.3)
all[ sample( which( all$Pclass==3 & is.na(all$CabinNo)), G.3 ) , "CabinNo"] <- rep("G", G.3)

all$CabinNo = as.factor(all$CabinNo)
table(all$CabinNo, all$Pclass)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Ticket = NULL
all$Name = NULL
all$Cabin = NULL

summary(all)

train = all[1:891,]
test = all[892:1309,]

train$Survived <- as.factor(Survived)
train$Survived <- as.factor(mapvalues(train$Survived, c("0", "1"), c("No","Yes")))
train$PassengerId = NULL


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train.male = subset(train, train$Sex == "male")
train.female = subset(train, train$Sex == "female")
test.male = subset(test, test$Sex == "male")
test.female = subset(test, test$Sex == "female")

train.male$Sex = NULL
train.male$Mother = NULL
train.male$Title = droplevels(train.male$Title)

train.female$Sex = NULL
train.female$Title = droplevels(train.female$Title)

test.male$Sex = NULL
test.male$Mother = NULL
test.male$Title = droplevels(test.male$Title)

test.female$Sex = NULL
test.female$Title = droplevels(test.female$Title)

# MALE
set.seed(100)
splt.m = sample.split(train.male, 0.75)  
cv.train.m = train.male[splt.m,]
cv.test.m = train.male[!splt.m,]

# FEMALE
set.seed(100)
splt.f = sample.split(train.female, 0.75)  
cv.train.f = train.female[splt.f,]
cv.test.f = train.female[!splt.f,]



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
x.m = data.matrix(cv.train.m[,2:14])
y.m = cv.train.m$Survived

set.seed(356)
# 10 fold cross validation
cvfit.m.ridge = cv.glmnet(x.m, y.m, 
                  family = "binomial", 
                  alpha = 0,
                  type.measure = "class")

cvfit.m.lasso = cv.glmnet(x.m, y.m, 
                  family = "binomial", 
                  alpha = 1,
                  type.measure = "class")
par(mfrow=c(1,2))
plot(cvfit.m.ridge, main = "Ridge")
plot(cvfit.m.lasso, main = "Lasso")
coef(cvfit.m.ridge, s = "lambda.min")


## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Prediction on training set
PredTrain.M = predict(cvfit.m.ridge, newx=x.m, type="class")
table(cv.train.m$Survived, PredTrain.M, cv.train.m$Title)

# Prediction on validation set
PredTest.M = predict(cvfit.m.ridge, newx=data.matrix(cv.test.m[,2:14]), type="class")
table(cv.test.m$Survived, PredTest.M, cv.test.m$Title)

# Prediction on test set
PredTest.M = predict(cvfit.m.ridge, newx=data.matrix(test.male[,3:15]), type="class")
table(PredTest.M, test.male$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
x.f = data.matrix(cv.train.f[,2:15])
y.f = cv.train.f$Survived

set.seed(356)
cvfit.f.ridge = cv.glmnet(x.f, y.f, 
                  family = "binomial", 
                  alpha = 0,
                  type.measure = "class")
cvfit.f.lasso = cv.glmnet(x.f, y.f, 
                  family = "binomial", 
                  alpha = 1,
                  type.measure = "class")
par(mfrow=c(1,2))
plot(cvfit.f.ridge, main = "Ridge")
plot(cvfit.f.lasso, main = "Lasso")
coef(cvfit.f.ridge, s = "lambda.min")



## ---- warning=F,message=F--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Ridge Model
# Prediction on training set
PredTrain.F = predict(cvfit.f.ridge, newx=x.f, type="class")
table(cv.train.f$Survived, PredTrain.F, cv.train.f$Title)
confusionMatrix(cv.train.f$Survived, PredTrain.F)

# Prediction on validation set
PredTest.F = predict(cvfit.f.ridge, newx=data.matrix(cv.test.f[,2:15]), type="class")
table(cv.test.f$Survived, PredTest.F, cv.test.f$Title)
confusionMatrix(cv.test.f$Survived, PredTest.F)

# Ridge Model
# Prediction on training set
PredTrain.F = predict(cvfit.f.lasso, newx=x.f, type="class")
table(cv.train.f$Survived, PredTrain.F, cv.train.f$Title)
confusionMatrix(cv.train.f$Survived, PredTrain.F)

# Prediction on validation set
PredTest.F = predict(cvfit.f.lasso, newx=data.matrix(cv.test.f[,2:15]), type="class")
table(cv.test.f$Survived, PredTest.F, cv.test.f$Title)
confusionMatrix(cv.test.f$Survived, PredTest.F)

# Prediction on test set
PredTest.F = predict(cvfit.f.ridge, newx=data.matrix(test.female[,3:16]), type="class")
table(PredTest.F, test.female$Title)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MySubmission.F = data.frame(PassengerId = test.female$PassengerId, Survived = ifelse(PredTest.F == "Yes",1,0)) 
MySubmission.M = data.frame(PassengerId = test.male$PassengerId, Survived = ifelse(PredTest.M == "Yes",1,0))     

MySubmission = rbind(MySubmission.F, MySubmission.M)
MySubmission = MySubmission[order(MySubmission$PassengerId),]
names(MySubmission) = c("PassengerId","Survived")   
table(MySubmission$Survived)

write.csv(MySubmission, file = 'Submission_RIDGE.csv', row.names = F)


