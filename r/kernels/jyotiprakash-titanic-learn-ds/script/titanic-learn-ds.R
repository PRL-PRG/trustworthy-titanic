#Titanic Proble, Predicting the survivors
#Loading the test and training files

train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)


#Summary of test and train
summary(test)
summary(train)

#Imputing missing valus of in Train set-Embarked based on other variables
train$Embarked[c(62,830)]='C'
train$Embarked=factor(train$Embarked)

#Adding survived column in test set
test$Survived= NA

#Binding test and train set in a single table
full.data=rbind(test,train)

#Converting as factor
full.data$Pclass=as.factor(full.data$Pclass)
full.data$Sex=as.factor(full.data$Sex)
full.data$Embarked=as.factor(full.data$Embarked)

#Checking structure of data frame
str(full.data)

#Finding colum wise missing values count
colSums(is.na(full.data))

#Table Age=263, Fare=1 and Survived =418

#Display the row of missing fare entry
miss=full.data[is.na(full.data$Fare),]

#Fare can be moddles as lm model depeing on Pclass,Sex,Embarked
train=full.data[!is.na(full.data$Age),] 
model=lm(Fare~Pclass+Embarked+Sex,data=train)
pred=predict(model,newdata=miss)

#Substituting predicted value in missinf=g fare(7.69)
full.data$Fare[is.na(full.data$Fare)]=pred

#Feature Extraction
#Extracting the titles ans Surnames
#Title
full.data$Title <- gsub('(.*, )|(\\..*)', '', full.data$Name)
table(full.data$Title,full.data$Survive)

#Data shows that few titles are less in number and majority of some died while rest survived
#Creating new titles as High, Mid and Low for such rare titles based on survival
#High(Lady,Mlle,MMe,MS,Sir,the countess), Mid(col,Dr,Major,), Low(Capt,Don,Jonkheer)
High=c("Lady","Mlle","Mme","Ms","Sir","the Countess")
Mid=c("Col","Dr","Major")
Low=c("Capt","Don","Jonkheer","Rev","Dona")
full.data$Title[full.data$Title %in% High]='High'
full.data$Title[full.data$Title %in% Mid] = 'Mid'
full.data$Title[full.data$Title %in% Low]= 'Low'

#Grab the surname
#full.data$Surname-sapply(full.data$Name,function(x) strsplit(x, split = '[,.]')[[1]][1])

#Family size as a variable
full.data$Fsize <- full.data$SibSp + full.data$Parch + 1

# Calssifying family as small mid and large
full.data$FsizeD[full.data$Fsize == 1]= "small"
full.data$FsizeD[(full.data$Fsize < 5) & (full.data$Fsize > 1)]= "Mid"
full.data$FsizeD[(full.data$Fsize > 4)]= "large"






#Converting to factor
full.data$Survived=as.factor(full.data$Survived)
full.data$Title=as.factor(full.data$Title)
full.data$Fsize=as.factor(full.data$Fsize)
full.data$FsizeD=as.factor(full.data$FsizeD)

#Splitting data as test and train
train.fin=full.data[!is.na(full.data$Survived),] 
test.fin=full.data[is.na(full.data$Survived),]

#imputing missing data -> Age
library(mice)
set.seed(144)
vars=setdiff(names(train.fin),c("Survived","Name","Cabin","PassengerTd"))
imputed=complete(mice(train.fin[vars]))
train.fin[vars]=imputed[vars]

#Logistic Regression: Train test split
library(caTools)
set.seed(12)
spl=sample.split(train.fin$Survived,SplitRatio=0.6)
train1=subset(train.fin,spl==TRUE)
test1=subset(train.fin,spl==FALSE)

#regression model
#model1=glm(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+FsizeD+Title,data=train1,family="binomial")
#summary(model1)
#pred=predict(model1,newdata=test1,type="response")
#table(test1$Survived,pred>0.5)

#random forest model
library(randomForest)
#train1$Survived=as.factor(train1$Survived)
#test1$Survived=as.factor(test1$Survived)
#model1=randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+FsizeD+Title,data=train1,method="class")
#pred=predict(model1,newdata=test1,type="class")
#table(test1$Survived,pred)

#SVM Model
library(e1071)
model1=svm(Survived~Pclass+Sex+Age+SibSp+Parch+Embarked+FsizeD+Title,data=train1,kernel="linear",cost=0.1,scale=FALSE)
pred=predict(model1,newdata=test1,type="class")
table(test1$Survived,pred)

#test set data inputation

library(mice)
set.seed(144)
#imputed=complete(mice(test))
#test=imputed
vars=setdiff(names(test.fin),c("Survived","Name","Cabin","PassengerTd"))
imput=complete(mice(test.fin[vars]))
test.fin[vars]=imput[vars]

#running prediction on competition test set
pred1=predict(model1,newdata=test.fin,type="class")
#Survived=pred1
PassengerId=test.fin$PassengerId
out=as.data.frame(PassengerId)
str(pred1)

#out$Survived=Survived
#write.csv(out,"OutR1.csv",row.names=FALSE)
#It gives an accuracy of 78.711 on test set