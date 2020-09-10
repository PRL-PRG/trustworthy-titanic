

#Read Dataset
TrainTitanic=read.csv("../input/train.csv", header=TRUE,na.strings=c(""," ","NA"))

#Structure of Data Set
str(TrainTitanic)

#Converting to factor
TrainTitanic$Survived=as.factor(TrainTitanic$Survived)
TrainTitanic$Pclass =as.factor(TrainTitanic$Pclass)

#cheking summary 
summary(TrainTitanic)

#cheking NA's age has 177 mising values,cabin has 687 and embarked has 2
colSums(is.na(TrainTitanic))


#install.packages("car")
#install.packages("ggplot2")

library(car)
library(ggplot2)
library(data.table)

#Imputing mean for age for approximation as of now
TrainTitanic$Age[is.na(TrainTitanic$Age)] = mean(TrainTitanic$Age, na.rm=TRUE)
#Fare plot with survival,
ggplot(TrainTitanic, aes(x=Age,fill=factor(Survived))) + 
  geom_histogram(breaks=seq(0,80,by=4),colour='black') + 
  xlab("Age") + ylab("Passengers")

#younger passengers around 12 and older one seems to be rescued compare to rest of the passengers

#Sex plot with survival,
ggplot(TrainTitanic, aes(x = Survived, fill = Sex)) + geom_bar()

# Females more likely to survive, women and children first it seems

#Fare plot with survival,
ggplot(TrainTitanic, aes(x=Fare,fill=factor(Survived))) + 
  geom_histogram(breaks=seq(0,550,by=10),colour='black') + 
  xlab("Fare") + ylab("Passengers")
#Expensive ticket holder more likely to survive based on status


#Class and fare should co-relate as per the last plot
Fare_mean_Pclass=aggregate(TrainTitanic$Fare, list(TrainTitanic$Pclass), mean)
colnames(Fare_mean_Pclass) <- c("pclass", "mean")
ggplot(Fare_mean_Pclass, aes(x = pclass, y=mean)) +geom_bar(stat="identity")

#age,fare and survival
plot(TrainTitanic$Age, TrainTitanic$Fare, pch = 9, col = c('red', 'green')[TrainTitanic$Survived])

#Red zone shows age 18-50 and lower class(fare)


#Embarked
ggplot(TrainTitanic, aes(x = Survived, fill = Embarked)) + geom_bar()
#Most survivals are from

#Feature engineering, 
#age mean impution is not up to the mark as we did it for all, it should be as per different
#groups child,male,female. This we can done based on the name.

Train=read.csv("../input/train.csv", header=TRUE,na.strings=c(""," ","NA"))

Train$Name=as.character(Train$Name)
#Converting to factor
Train$Survived=as.factor(Train$Survived)
Train$Pclass =as.factor(Train$Pclass)

str(Train)
#Name

for (i in 1:nrow(Train)) {
  Train$Title[i] <- as.character(trimws(strsplit(strsplit(Train$Name[i],",")[[1]][2],"[.]")[[1]][1]))
}

table(Train$Title)

Train$Title=as.factor(Train$Title)

Train$Title=recode(Train$Title,"c('Don','Jonkheer','Lady','Sir','the Countess','Don','Capt','Col','Major','Dr','Rev')='Less';
       c('Miss','Mlle','Ms')='Miss';c('Mrs','Mme')='Mrs'")

#cheking the NA
colSums(is.na(Train))

#Age mean by Sex,Class and Title
aggregate(x=Train$Age,
          by=list(Train$Sex,Train$Pclass,Train$Title),
          FUN=mean,na.rm=TRUE)

#Function to impute Age
Age_Na_Treatment_Train <- function(df) {
  
  for (i in 1:nrow(df)) {
    
    if (is.na(df$Age[i])) {
      
      if(df$Sex[i]=="female" && df$Pclass[i]==1){
        
        if(df$Title[i]=="Less"){
          
          df$Age[i]=43.33
        }
        
        else if (df$Title[i]=="Miss") {
          df$Age[i]=29.74
        }
        
        else if (df$Title[i]=="Mrs") {
          df$Age[i]=40.40
        }
        
      }
      
     else if(df$Sex[i]=="female" && df$Pclass[i]==2){
        
         if (df$Title[i]=="Miss") {
          df$Age[i]=22.56
        }
        
        else if (df$Title[i]=="Mrs") {
          df$Age[i]=33.68
        }
        
        
     }
      
      else if(df$Sex[i]=="female" && df$Pclass[i]==3){
      
        if (df$Title[i]=="Miss") {
          df$Age[i]=16.12
        }
        
        else if (df$Title[i]=="Mrs") {
          df$Age[i]=33.51
        }
        
        
      }
      
      else if(df$Sex[i]=="male" && df$Pclass[i]==1){
        
        if(df$Title[i]=="Less"){
          
          df$Age[i]=48.7272
        }
        
        else if (df$Title[i]=="Master") {
          df$Age[i]=5.30
        }
        
        else if (df$Title[i]=="Mr") {
          df$Age[i]=41.58
        }
        
      }
      
      else if(df$Sex[i]=="male" && df$Pclass[i]==2){
        
        if(df$Title[i]=="Less"){
          
          df$Age[i]=42.00
        }
        
        else if (df$Title[i]=="Master") {
          df$Age[i]=2.25
        }
        
        else if (df$Title[i]=="Mr") {
          df$Age[i]=32.76
        }
        
      }
      
      else if(df$Sex[i]=="male" && df$Pclass[i]==3){
        
         if (df$Title[i]=="Master") {
          df$Age[i]=5.35
        }
        
        else if (df$Title[i]=="Mr") {
          df$Age[i]=28.72
        }
        
      }
      
    }
    
  }
  df
}

#Impute the Age
Train_Age_Imputed=Age_Na_Treatment_Train(Train)

#Cheking the NA now

colSums(is.na(Train_Age_Imputed))

str(Train_Age_Imputed)

#Removing Passenger ID and Name

Train_Name_PassId_removed=Train_Age_Imputed[,c(-1,-4)]

#Cheking the NA now
colSums(is.na(Train_Name_PassId_removed))

#Function for Mode value for embarked
mode_embarked<- function(columnName) {
  
  max_embarked=table (columnName)
  
  names(max_embarked[which.max(max_embarked)])
  
}

#checking the mode
mode_embarked(Train_Name_PassId_removed$Embarked)

#Imputing the mode

Train_Name_PassId_removed$Embarked[is.na(Train_Name_PassId_removed$Embarked)] <- mode_embarked(Train_Name_PassId_removed$Embarked)


#Cheking the NA now
colSums(is.na(Train_Name_PassId_removed))

#Cabin NA values imputed with U=Unknown
Train_Name_PassId_removed$Cabin = factor(Train_Name_PassId_removed$Cabin, levels=c(levels(Train_Name_PassId_removed$Cabin),'U'))

Train_Name_PassId_removed$Cabin[is.na(Train_Name_PassId_removed$Cabin)] = 'U'

#Cheking the NA now
colSums(is.na(Train_Name_PassId_removed))

str(Train_Name_PassId_removed)

#Cabin Encoded

for (i in 1:nrow(Train_Name_PassId_removed)) {
  Train_Name_PassId_removed$Cabin_Encoded[i] <- as.character(trimws(substring(Train_Name_PassId_removed$Cabin[i],1,1 )))
}

Train_Name_PassId_removed$Cabin_Encoded=as.factor(Train_Name_PassId_removed$Cabin_Encoded)

#Cabin Removed
Train_Name_PassId_removed=Train_Name_PassId_removed[,-9]

str(Train_Name_PassId_removed)

#Family Size calculation

for (i in 1:nrow(Train_Name_PassId_removed)) {
  Train_Name_PassId_removed$Family_Size[i] <- Train_Name_PassId_removed$SibSp[i]+Train_Name_PassId_removed$Parch[i]+1
}

str(Train_Name_PassId_removed)

#Family category type

for (i in 1:nrow(Train_Name_PassId_removed)) {
  
  if(Train_Name_PassId_removed$Family_Size[i]==1){
    
    Train_Name_PassId_removed$Family_Type[i]='Single'
  }
  
  else if (Train_Name_PassId_removed$Family_Size[i]>=2 && Train_Name_PassId_removed$Family_Size[i]<=4) {
    Train_Name_PassId_removed$Family_Type[i]='Small'
  }
  
  else{Train_Name_PassId_removed$Family_Type[i]='Large'}
}

Train_Name_PassId_removed$Family_Type=as.factor(Train_Name_PassId_removed$Family_Type)

str(Train_Name_PassId_removed)



#Ticket Feature

#Replacing special characters
#Train_Name_PassId_removed$Ticket=gsub("[.]", "", Train_Name_PassId_removed$Ticket)
#Train_Name_PassId_removed$Ticket=gsub("/", "", Train_Name_PassId_removed$Ticket)

#Creating categories

#for (i in 1:nrow(Train_Name_PassId_removed)) {
 #  Value<- strsplit(Train_Name_PassId_removed$Ticket[i]," ")[[1]]
  
   
 #  if(is.na(as.numeric(Value[2]))){
  #   Train_Name_PassId_removed$Ticket_Cat[i]="xxx"
  # }
     
  # else{
     
   #  Train_Name_PassId_removed$Ticket_Cat[i]=Value[1]
  # }
  
#}

#Train_Name_PassId_removed$Ticket_Cat=as.factor(Train_Name_PassId_removed$Ticket_Cat)

str(Train_Name_PassId_removed)



#Removing ticket now

Train_Name_PassId_removed=Train_Name_PassId_removed[,-7]

#Family Size removed
Train_Name_PassId_removed=Train_Name_PassId_removed[,c(-11)]
str(Train_Name_PassId_removed)
#install.packages("earth")
library(earth)
library(caret)

dummies <- caret::dummyVars(Survived ~ ., data = Train_Name_PassId_removed)

Train_Encoded=predict(dummies, newdata = Train_Name_PassId_removed)

Train_Encoded=as.data.frame(Train_Encoded)

Train_Encoded=cbind(Train_Name_PassId_removed[,1],Train_Encoded)

setnames(Train_Encoded,"Train_Name_PassId_removed[, 1]","Survived")

########################################################################################
#Test DataSet

Test=read.csv("../input/test.csv", header=TRUE,na.strings=c(""," ","NA"))

str(Test)

#Converting to factor
Test$Pclass =as.factor(Test$Pclass)

# Title Creation

Test$Name=as.character(Test$Name)
for (i in 1:nrow(Test)) {
  Test$Title[i] <- as.character(trimws(strsplit(strsplit(Test$Name[i],",")[[1]][2],"[.]")[[1]][1]))
}

table(Test$Title)

#install.packages("car")
library(car)
Test$Title=recode(Test$Title,"c('Don','Jonkheer','Lady','Sir','the Countess','Don','Dona','Capt','Col','Major','Dr','Rev')='Less';
                  c('Miss','Mlle','Ms')='Miss';c('Mrs','Mme')='Mrs'")

Test$Title=as.factor(Test$Title)

#cheking NA

colSums(is.na(Test))

#Computing Mean via Sex,class and title for Test dataset
aggregate(x=Test$Age,
          by=list(Test$Sex,Test$Pclass,Test$Title),
          FUN=mean,na.rm=TRUE)

#Imputation function for age
Age_Na_Treatment_Test <- function(df) {
  
  for (i in 1:nrow(df)) {
    
    if (is.na(df$Age[i])) {
      
      if(df$Sex[i]=="female" && df$Pclass[i]==1){
        
        if (df$Title[i]=="Miss") {
          df$Age[i]=31.42
        }
        
        else if (df$Title[i]=="Mrs") {
          df$Age[i]=45.60
        }
        
        else if (df$Title[i]=="Less") {
          df$Age[i]=39.00
        }
        
      }
      
      else if(df$Sex[i]=="female" && df$Pclass[i]==2){
        
        if (df$Title[i]=="Miss") {
          df$Age[i]=17.37
        }
        
        else if (df$Title[i]=="Mrs") {
          df$Age[i]=33.00
        }
        
        
      }
      
      else if(df$Sex[i]=="female" && df$Pclass[i]==3){
        
        if (df$Title[i]=="Miss") {
          df$Age[i]=19.87
        }
        
        else if (df$Title[i]=="Mrs") {
          df$Age[i]=29.87
        }
        
        
      }
      
      else if(df$Sex[i]=="male" && df$Pclass[i]==1){
        
        if(df$Title[i]=="Less"){
          
          df$Age[i]=51.00
        }
        
        else if (df$Title[i]=="Master") {
          df$Age[i]=9.50
        }
        
        else if (df$Title[i]=="Mr") {
          df$Age[i]=41.20
        }
        
      }
      
      else if(df$Sex[i]=="male" && df$Pclass[i]==2){
        
        if(df$Title[i]=="Less"){
          
          df$Age[i]=35.50
        }
        
        else if (df$Title[i]=="Master") {
          df$Age[i]=5.00
        }
        
        else if (df$Title[i]=="Mr") {
          df$Age[i]=31.71
        }
        
      }
      
      else if(df$Sex[i]=="male" && df$Pclass[i]==3){
        
        if (df$Title[i]=="Master") {
          df$Age[i]=7.45
        }
        
        else if (df$Title[i]=="Mr") {
          df$Age[i]=27.20
        }
        
      }
      
    }
    
  }
  df
}

#Impute the Age
Test_Age_Imputed=Age_Na_Treatment_Test(Test)

#Cheking NA now
colSums(is.na(Test_Age_Imputed))

str(Test_Age_Imputed)

#Removing Passenger Name

Test_Name_removed=Test_Age_Imputed[,-3]

str(Test_Name_removed)


colSums(is.na(Test_Name_removed))


#Cabin NA values imputed with U=Unknown
Test_Name_removed$Cabin = factor(Test_Name_removed$Cabin, levels=c(levels(Test_Name_removed$Cabin),'U'))

Test_Name_removed$Cabin[is.na(Test_Name_removed$Cabin)] = 'U'

#Cheking the NA now
colSums(is.na(Test_Name_removed))

str(Test_Name_removed)


#Cabin Encoded

for (i in 1:nrow(Test_Name_removed)) {
  Test_Name_removed$Cabin_Encoded[i] <- as.character(trimws(substring(Test_Name_removed$Cabin[i],1,1 )))
}

Test_Name_removed$Cabin_Encoded=as.factor(Test_Name_removed$Cabin_Encoded)

#Cabin Removed
Test_Name_PaTest_Age_Imputed=Test_Name_removed[,-9]

str(Test_Name_PaTest_Age_Imputed)

colSums(is.na(Test_Name_PaTest_Age_Imputed))

#Impute Fair by Mean

Test_Name_PaTest_Age_Imputed$Fare[is.na(Test_Name_PaTest_Age_Imputed$Fare)] = mean(Test_Name_PaTest_Age_Imputed$Fare, na.rm=TRUE)

colSums(is.na(Test_Name_PaTest_Age_Imputed))

#Famly size calculation

  for (i in 1:nrow(Test_Name_PaTest_Age_Imputed)) {
    Test_Name_PaTest_Age_Imputed$Family_Size[i] <- Test_Name_PaTest_Age_Imputed$SibSp[i]+Test_Name_PaTest_Age_Imputed$Parch[i]+1
  }

str(Test_Name_PaTest_Age_Imputed)


#Family category
for (i in 1:nrow(Test_Name_PaTest_Age_Imputed)) {
  
  if(Test_Name_PaTest_Age_Imputed$Family_Size[i]==1){
    
    Test_Name_PaTest_Age_Imputed$Family_Type[i]='Single'
  }
  
  else if (Test_Name_PaTest_Age_Imputed$Family_Size[i]>=2 && Test_Name_PaTest_Age_Imputed$Family_Size[i]<=4) {
    Test_Name_PaTest_Age_Imputed$Family_Type[i]='Small'
  }
  
  else{Test_Name_PaTest_Age_Imputed$Family_Type[i]='Large'}
}

Test_Name_PaTest_Age_Imputed$Family_Type=as.factor(Test_Name_PaTest_Age_Imputed$Family_Type)


str(Test_Name_PaTest_Age_Imputed)

colSums(is.na(Test_Name_PaTest_Age_Imputed))

#Ticket Feature
#Special Char removed
#Test_Name_PassId_removed$Ticket=gsub("[.]", "", Test_Name_PassId_removed$Ticket)
#Test_Name_PassId_removed$Ticket=gsub("/", "", Test_Name_PassId_removed$Ticket)

#Ticket Encode
#for (i in 1:nrow(Test_Name_PassId_removed)) {
#  Value<- strsplit(Test_Name_PassId_removed$Ticket[i]," ")[[1]]
  
  
 # if(is.na(as.numeric(Value[2]))){
 #   Test_Name_PassId_removed$Ticket_Cat[i]="xxx"
 # }
  
 # else{
    
 #   Test_Name_PassId_removed$Ticket_Cat[i]=Value[1]
  #}
  
#}


#Test_Name_PassId_removed$Ticket_Cat=as.factor(Test_Name_PassId_removed$Ticket_Cat)

Test_Name_PaTest_Age_Imputed=Test_Name_PaTest_Age_Imputed[,-7]

str(Test_Name_PaTest_Age_Imputed)

Test_Selected=Test_Name_PaTest_Age_Imputed[,c(-11)]

str(Test_Selected)

#One hot encoding

dummies_Test <- caret::dummyVars(PassengerId ~ ., data = Test_Selected)

Test_Encoded=predict(dummies_Test, newdata = Test_Selected)

Test_Encoded = as.data.frame(Test_Encoded)

Test_Encoded=cbind(Test_Selected[,1],Test_Encoded)


setnames(Test_Encoded,"Test_Selected[, 1]","PassengerId")

str(Test_Encoded)



str(Train_Encoded)




library(randomForest)
library(rpart.plot)

# Build random forest model
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)

Train_Encoded_Selected=Train_Encoded[,which(names(Train_Encoded) %in% c("Survived","Sex.female","Title.Mr","Fare","Sex.male","Pclass.3","Age","Title.Miss","Title.Master","Title.Mrs","Cabin_Encoded.U","Embarked.S","Family_Type.Small","Family_Type.Large","Embarked.C","Pclass.2","Pclass.1","Cabin_Encoded.E","Embarked.Q","Family_Type.Small"))]
## Tuning Random Forest
tRF <- tuneRF(x = Train_Encoded_Selected[,-1], 
              y=Train_Encoded_Selected$Survived,
              mtryStart = 4, 
              ntreeTry=151, 
              stepFactor = 1.5, 
              improve = 0.0001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 10, 
              importance=TRUE
)

dim(Train_Encoded_Selected)

#RF <- randomForest(Survived ~ ., data = Train_Encoded, 
#                   ntree=151, mtry = 7, nodesize = 10,importance=TRUE)

#importanceFactor=round(randomForest::importance(RF),2)

#importanceFactor[order(importanceFactor[,3], decreasing = TRUE),]

#Train_Encoded_Selected=Train_Encoded[,which(names(Train_Encoded) %in% c("Survived","Sex.female","Title.Mr","Fare","Sex.male","Pclass.3","Age","Title.Miss","Title.Master","Title.Mrs","Cabin_Encoded.U","Embarked.S","Family_Type.Small","Family_Type.Large","Embarked.C","Pclass.2","Pclass.1","Cabin_Encoded.E","Embarked.Q","Family_Type.Small"))]

randomForestModel <- randomForest(Survived ~ ., data = Train_Encoded_Selected, 
                   ntree=151, mtry = 3, nodesize = 10,importance=TRUE)

#install.packages("data.table")
library(data.table)
#install.packages("gbm")
library(gbm)
#install.packages("ranger")
library(ranger)
#install.packages("caTools")
library(caTools)

Survived = predict(randomForestModel, newdata = Test_Encoded, type = "class")
submitfile = as.data.table(Survived)
submitfile = cbind(Test_Encoded$PassengerId, submitfile)
submitfile
setnames(submitfile,"V1","PassengerId")

submitfile
fwrite(submitfile,"submitFile.csv")

getwd()

###################################################################

# Any results you write to the current directory are saved as output.