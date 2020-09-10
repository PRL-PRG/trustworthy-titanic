
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(mice)
library(lattice)
# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
test$Survived<-0
dataTita<-rbind(train,test)
#survived       (0 = No; 1 = Yes)
#pclass          Passenger Class   (1 = 1st; 2 = 2nd; 3 = 3rd)
#name            Name
#sex             Sex
#age             Age
#sibsp           Number of Siblings/Spouses Aboard
#parch           Number of Parents/Children Aboard
#ticket          Ticket Number
#fare            Passenger Fare
#cabin           Cabin
#embarked        Port of Embarkation   (C = Cherbourg; Q = Queenstown; S = Southamp

#check,data 
str(dataTita)
dataTita$Name<-as.character(dataTita$Name)
dataTita$title<-sapply(dataTita$Name,FUN=function(x){strsplit(x,"[,.]")[[1]][[2]]})
dataTita$title<-gsub(" ","",dataTita$title)
dataTita$title[dataTita$title%in%c('Mlle','Ms','Lady')]<-'Miss'
dataTita$title[dataTita$title=="Mme"]<-"Mrs"
dataTita$title[dataTita$title %in% c("Capt","Col","Don","Dona","Dr","Jonkheer","Master",
                                     "Major","Rev","Sir","theCountess")]<-"rare title"
dataTita$Family<-apply(dataTita[,names(dataTita)%in%c("SibSp","Parch")],1,sum)

#find missing values
apply(dataTita,2,FUN=function(x){sum(is.na(x))})
apply(dataTita,2,FUN=function(x){sum(x=="")})

#missing value imputation
dataTita$Embarked[c(62,830)]<-"S"
dataTita$Fare[1044]<-mean(dataTita[which(dataTita$Pclass==3 & 
                                           dataTita$PassengerId!=1044),9])
imp<-mice(dataTita[,names(dataTita)%in%c("Pclass","Age","SibSp","Parch","Fare")],
          m=5,meth="rf",seed = 1234)
densityplot(imp)
dataTita<-cbind(dataTita[,!names(dataTita)%in%c("Pclass","Age","SibSp","Parch","Fare")],
                complete(imp,1))

#Predict
dataTita$title<-as.factor(dataTita$title)
dataTita$Embarked<-as.factor(dataTita$Embarked)
dataTita$Sex<-as.factor(dataTita$Sex)
train<-dataTita[1:891,]
test<-dataTita[892:1309,!names(dataTita)%in%c("Survived")]
train$title<-as.factor(train$title)
train$Embarked<-as.factor(train$Embarked)
train$Sex<-as.factor(train$Sex)
train.rf<-randomForest(factor(Survived)~Age+Pclass+Family+Fare+title+Embarked+Sex,
                       data = train,ntree=50)
train.rf
test.rf<-predict(train.rf,test)
testresult<-data.frame(PassengerId=test$PassengerId,Survived=test.rf)
write.csv(testresult,"result.csv",row.names = FALSE)
