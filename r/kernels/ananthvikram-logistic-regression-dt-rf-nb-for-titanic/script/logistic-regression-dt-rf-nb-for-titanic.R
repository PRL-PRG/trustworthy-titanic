################## Setup ######################

#path<-"C:/Users/Ananth.Vikram/Documents/RMS_Titanic"
#setwd(path)
library(dplyr)
library(stringr)
library(ggplot2)
library(rowr)
library(corrplot)
library(ROCR)
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(e1071)#Naive Bayes

#Read data files
trainDS<-read.csv("../input/train.csv", na.strings=c("","NA"),stringsAsFactors = FALSE)
testDS<-read.csv("../input/test.csv", na.strings=c("","NA"),stringsAsFactors = FALSE)
trainDS$SetType<-"Train"
testDS$SetType<-"Test"
testDS$Survived<-NA
#genderDS<-read.csv("gender_submission.csv", na.strings=c("","NA"),stringsAsFactors = FALSE)

fullDS<-rbind(testDS,trainDS)

################## Data Treatment ######################

summary(fullDS)

#Missing Value treatment

fullDS$Age <- ifelse(is.na(fullDS$Age), mean(fullDS$Age, na.rm=TRUE), fullDS$Age)
fullDS$Fare <- ifelse(is.na(fullDS$Fare), median(fullDS$Fare, na.rm=TRUE), fullDS$Fare)
#Function to get mode to treat categorical values
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fullDS$Embarked <- ifelse(is.na(fullDS$Embarked), getmode(fullDS$Embarked), fullDS$Embarked)

summary(fullDS)
#Feature Engineering

title_list=c('Mrs.', 'Mr.', 'Master', 'Miss', 'Major', 'Rev',
             'Dr', 'Ms', 'Mlle','Col', 'Capt', 'Mme', 'Countess',
             'Don', 'Jonkheer','Lady','Sir')

fullDS$Title<-""

#Title
for(i in 1:length(title_list)){
  for(j in 1:nrow(fullDS)){
    
    match<-grepl(title_list[i],fullDS$Name[j] , ignore.case = FALSE, perl = FALSE, 
                 fixed = TRUE, useBytes = FALSE)
    if(match){
      fullDS$Title[j]<-title_list[i]
    }
  }
}

#Family Size

fullDS$FamilySize<-fullDS$SibSp + fullDS$Parch + 1

#Fare per person

fullDS$FarePerPerson<-fullDS$Fare/fullDS$FamilySize

#Title categorization

crew <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'Countess','Sir', 'Jonkheer')

fullDS$Title[fullDS$Title == 'Mlle']<- 'Miss' 
fullDS$Title[fullDS$Title == 'Ms']<- 'Miss'
fullDS$Title[fullDS$Title == 'Mme']<- 'Mrs.' 
fullDS$Title[fullDS$Title %in% royalty]<- 'Royalty'
fullDS$Title[fullDS$Title %in% crew]<- 'Crew'

#Age group

fullDS$AgeGroup<-""
fullDS$AgeGroup[fullDS$Age <13]<- "0 to 12"
fullDS$AgeGroup[fullDS$Age >=13 & fullDS$Age <=19 ]<- "13 to 19"
fullDS$AgeGroup[fullDS$Age >=20 & fullDS$Age <=39 ]<- "20 to 39"
fullDS$AgeGroup[fullDS$Age >=40 & fullDS$Age <=64 ]<- "40 to 64"
fullDS$AgeGroup[fullDS$Age >=65 ]<- "65+"


#Fare group

fullDS$FareGroup[fullDS$FarePerPerson <11]<- "0 to 10"
fullDS$FareGroup[fullDS$FarePerPerson >=11 & fullDS$FarePerPerson <20 ]<- "11 to 20"
fullDS$FareGroup[fullDS$FarePerPerson >=20 & fullDS$FarePerPerson <40 ]<- "20 to 39"
fullDS$FareGroup[fullDS$FarePerPerson >=40 & fullDS$FarePerPerson <60 ]<- "40 to 59"
fullDS$FareGroup[fullDS$FarePerPerson >=60 &  fullDS$FarePerPerson <100]<- "60 to 99"
fullDS$FareGroup[fullDS$FarePerPerson >=100 &  fullDS$FarePerPerson <150]<- "100 to 149"
fullDS$FareGroup[fullDS$FarePerPerson >=150 ]<- "150+"

#Family type

fullDS$FamilyType[fullDS$FamilySize ==1]<- "Single"
fullDS$FamilyType[fullDS$FamilySize ==2 ]<- "Couple"
fullDS$FamilyType[fullDS$FamilySize >=3 & fullDS$FamilySize <6 ]<- "Small"
fullDS$FamilyType[fullDS$FamilySize >=6 ]<- "Big"


################## EDA ######################

#Overall

OvClassDF <- fullDS[which(fullDS$SetType=="Train"),] %>%
  group_by(Survived) %>%
  summarize(Totalcount = n())

OverallSurvivalRate<-(nrow(fullDS[which(fullDS$Survived==1 & fullDS$SetType=="Train"),])/nrow(fullDS[which(fullDS$SetType=="Train"),]))

#OverallSurvivalRate is 38%

#Passenger Class

PClassSv <- fullDS[which(fullDS$Survived==1 & fullDS$SetType=="Train"),] %>%
  group_by(Pclass) %>%
  summarize(Survivalcount = n())

PClassDF <- fullDS[which(fullDS$SetType=="Train"),] %>%
  group_by(Pclass) %>%
  summarize(Totalcount = n())

PClassEDA<-cbind(PClassSv[,c(0,1,2)],PClassDF[,2])
PClassEDA$SurvivalRate<-PClassEDA$Survivalcount/PClassEDA$Totalcount
ggplot(data=PClassEDA, aes(x=Pclass, y=SurvivalRate, fill=Pclass)) +
  geom_bar(stat="identity")

#Higher passenger the class, the more likely passenger is to survive

#Gender

GenderSv <- fullDS[which(fullDS$Survived==1 & fullDS$SetType=="Train"),] %>%
  group_by(Sex) %>%
  summarize(Survivalcount = n())

GenderDF <- fullDS[which(fullDS$SetType=="Train"),] %>%
  group_by(Sex) %>%
  summarize(Totalcount = n())

GenderEDA<-cbind(GenderSv[,c(0,1,2)],GenderDF[,2])
GenderEDA$SurvivalRate<-GenderEDA$Survivalcount/GenderEDA$Totalcount
GenderEDA<-GenderEDA[order(GenderEDA$SurvivalRate),]
GenderEDA$G_O<-1:nrow(GenderEDA)
ggplot(data=GenderEDA, aes(x=Sex, y=SurvivalRate, fill=Sex)) +
  geom_bar(stat="identity")

#Females have higher SR than males

#Embarked

EmbarkedSv <- fullDS[which(fullDS$Survived==1 & fullDS$SetType=="Train"),] %>%
  group_by(Embarked) %>%
  summarize(Survivalcount = n())

EmbarkedDF <- fullDS[which(fullDS$SetType=="Train"),] %>%
  group_by(Embarked) %>%
  summarize(Totalcount = n())

EmbarkedEDA<-cbind(EmbarkedSv[,c(0,1,2)],EmbarkedDF[,2])
EmbarkedEDA$SurvivalRate<-EmbarkedEDA$Survivalcount/EmbarkedEDA$Totalcount
EmbarkedEDA<-EmbarkedEDA[order(EmbarkedEDA$SurvivalRate),]
EmbarkedEDA$E_O<-1:nrow(EmbarkedEDA)
ggplot(data=EmbarkedEDA, aes(x=Embarked, y=SurvivalRate, fill=Embarked)) +
  geom_bar(stat="identity")

#Embarked on Port C are more likely to survive


#title

TitleSv <- fullDS[which(fullDS$Survived==1 & fullDS$SetType=="Train"),] %>%
  group_by(Title) %>%
  summarize(Survivalcount = n())

TitleDF <- fullDS[which(fullDS$SetType=="Train"),] %>%
  group_by(Title) %>%
  summarize(Totalcount = n())

TitleEDA<-merge(TitleSv,TitleDF,by="Title",all=TRUE)
TitleEDA$Survivalcount[is.na(TitleEDA$Survivalcount)]<-0
TitleEDA$SurvivalRate<-TitleEDA$Survivalcount/TitleEDA$Totalcount
TitleEDA<-TitleEDA[order(TitleEDA$SurvivalRate),]
TitleEDA$T_O<-1:nrow(TitleEDA)
ggplot(data=TitleEDA, aes(x=Title, y=SurvivalRate, fill=Title)) +
  geom_bar(stat="identity")

#Age group

AgeGroupSv <- fullDS[which(fullDS$Survived==1 & fullDS$SetType=="Train"),] %>%
  group_by(AgeGroup) %>%
  summarize(Survivalcount = n())

AgeGroupDF <- fullDS[which(fullDS$SetType=="Train"),] %>%
  group_by(AgeGroup) %>%
  summarize(Totalcount = n())

ggplot(data=AgeGroupDF, aes(x=AgeGroup, y=Totalcount, fill=AgeGroup)) +
  geom_bar(stat="identity")

AgeGroupEDA<-merge(AgeGroupSv,AgeGroupDF,by="AgeGroup",all=TRUE)
AgeGroupEDA$Survivalcount[is.na(AgeGroupEDA$Survivalcount)]<-0
AgeGroupEDA$SurvivalRate<-AgeGroupEDA$Survivalcount/AgeGroupEDA$Totalcount
AgeGroupEDA<-AgeGroupEDA[order(AgeGroupEDA$SurvivalRate),]
AgeGroupEDA$AG_O<-1:nrow(AgeGroupEDA)

ggplot(data=AgeGroupEDA, aes(x=AgeGroup, y=SurvivalRate, fill=AgeGroup)) +
  geom_bar(stat="identity")


#Fare group

FareGroupSv <- fullDS[which(fullDS$Survived==1 & fullDS$SetType=="Train"),] %>%
  group_by(FareGroup) %>%
  summarize(Survivalcount = n())

FareGroupDF <- fullDS[which(fullDS$SetType=="Train"),] %>%
  group_by(FareGroup) %>%
  summarize(Totalcount = n())

ggplot(data=FareGroupDF, aes(x=FareGroup, y=Totalcount, fill=FareGroup)) +
  geom_bar(stat="identity")

FareGroupEDA<-merge(FareGroupSv,FareGroupDF,by="FareGroup",all=TRUE)
FareGroupEDA$Survivalcount[is.na(FareGroupEDA$Survivalcount)]<-0
FareGroupEDA$SurvivalRate<-FareGroupEDA$Survivalcount/FareGroupEDA$Totalcount
FareGroupEDA<-FareGroupEDA[order(FareGroupEDA$SurvivalRate),]
FareGroupEDA$FG_O<-1:nrow(FareGroupEDA)

ggplot(data=FareGroupEDA, aes(x=FareGroup, y=SurvivalRate, fill=FareGroup)) +
  geom_bar(stat="identity")


#Family type

FamilyTypeSv <- fullDS[which(fullDS$Survived==1 & fullDS$SetType=="Train"),] %>%
  group_by(FamilyType) %>%
  summarize(Survivalcount = n())

FamilyTypeDF <- fullDS[which(fullDS$SetType=="Train"),] %>%
  group_by(FamilyType) %>%
  summarize(Totalcount = n())

ggplot(data=FamilyTypeDF, aes(x=FamilyType, y=Totalcount, fill=FamilyType)) +
  geom_bar(stat="identity")

FamilyTypeEDA<-merge(FamilyTypeSv,FamilyTypeDF,by="FamilyType",all=TRUE)
FamilyTypeEDA$Survivalcount[is.na(FamilyTypeEDA$Survivalcount)]<-0
FamilyTypeEDA$SurvivalRate<-FamilyTypeEDA$Survivalcount/FamilyTypeEDA$Totalcount
FamilyTypeEDA<-FamilyTypeEDA[order(FamilyTypeEDA$SurvivalRate),]
FamilyTypeEDA$FT_O<-1:nrow(FamilyTypeEDA)
ggplot(data=FamilyTypeEDA, aes(x=FamilyType, y=SurvivalRate, fill=FamilyType)) +
  geom_bar(stat="identity")


########### Data Transformation ###############

fullTransformed<-fullDS[,c("Survived","Pclass","Sex","Embarked","Title","FamilyType","FareGroup","AgeGroup","SetType")]
fullTransformed<-merge(fullTransformed,FamilyTypeEDA[,c("FamilyType","FT_O")], on = FamilyType)
fullTransformed<-merge(fullTransformed,GenderEDA[,c("Sex","G_O")], on = Sex)
fullTransformed<-merge(fullTransformed,FareGroupEDA[,c("FareGroup","FG_O")], on = FareGroup)
fullTransformed<-merge(fullTransformed,EmbarkedEDA[,c("Embarked","E_O")], on = Embarked)
fullTransformed<-merge(fullTransformed,TitleEDA[,c("Title","T_O")], on = Title)
fullTransformed<-merge(fullTransformed,AgeGroupEDA[,c("AgeGroup","AG_O")], on = AgeGroup)
fullTransformed<-fullTransformed[,c("Survived","FG_O","FT_O","E_O","G_O","T_O","AG_O","Pclass","SetType")]
trainADS<-fullTransformed[which(fullTransformed$SetType=="Train"),c("Survived","FG_O","FT_O","E_O","G_O","T_O","AG_O","Pclass")]
testADS<-fullTransformed[which(fullTransformed$SetType=="Test"),c("Survived","FG_O","FT_O","E_O","G_O","T_O","AG_O","Pclass")]


########### Modelling ##########################

Correlation_Matrix<-cor(trainADS)
corrplot(Correlation_Matrix, method="color",addCoef.col = "black",tl.col="black")


#Logistic regression

logistic_regression_model<-glm(Survived~.,data=trainADS,family="binomial")
summary(logistic_regression_model)
anova(logistic_regression_model, test="Chisq")

predict <- predict(logistic_regression_model, trainADS,type = 'response')

confusion_matrix<-table(trainADS$Survived, predict >= 0.5)

#Sensitivity or true positive rate
Sensitivity<-confusion_matrix[2,2] / (confusion_matrix[2,2]+confusion_matrix[2,1])

#Specificity or true negative rate
Specificity<-confusion_matrix[1,1] / (confusion_matrix[1,1]+confusion_matrix[1,2])

#Accuracy = 80.47%
Accuracy<- (confusion_matrix[1,1] + confusion_matrix[2,2])/(confusion_matrix[1,1] + confusion_matrix[2,2]+confusion_matrix[1,2]+confusion_matrix[2,1])

ROCRpred <- prediction(predict, trainADS$Survived)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

predictTest <- predict(logistic_regression_model, testADS,type = 'response')

for(i in 1:length(predictTest)){
testADS$Survived[i]<-ifelse(predictTest[i]>=0.5,1,0)
}

#Decision trees

decision_tree_model <- rpart(Survived ~.,data = trainADS,method="class")

summary(decision_tree_model)

fancyRpartPlot(decision_tree_model)


Prediction_DT <- predict(decision_tree_model, trainADS, type = "class")

confusionMatrix(Prediction_DT,trainADS$Survived )

#Accuracy = 82.94%

predictTestDT <- as.data.frame(predict(decision_tree_model, testADS,type = 'class'))
colnames(predictTestDT)<-"Survived"

testADS$Survived<-NULL
testADS<-cbind(testADS,predictTestDT)


#Random Forest

set.seed(567) #it is a good idea to set the random seed in R before you begin. This makes your results reproducible next time you load the code up, otherwise you can get different classifications for each run.

randomForest_Model <- randomForest(as.factor(Survived) ~.,
                    data=trainADS, 
                    importance=TRUE, 
                    ntree=2000)
summary(randomForest_Model)
varImpPlot(randomForest_Model)
Prediction_RF <- predict(randomForest_Model, trainADS)
confusionMatrix(Prediction_RF,trainADS$Survived )

#Accuracy = 84.96%

PredictTest_RF <- as.data.frame(predict(randomForest_Model, testADS))
colnames(PredictTest_RF)<-"Survived"

testADS$Survived<-NULL
testSet<-fullDS[which(fullDS$SetType=="Test"),]
submission<-cbind(testSet$PassengerId,PredictTest_RF)
colnames(submission)<-c("PassengerId","Survived")
write.csv(submission,"submission.csv",row.names = FALSE)

#Naive Bayes

naiveBayes_model <- naiveBayes(as.factor(Survived)~., data = trainADS)
summary(naiveBayes_model)
Prediction_NB <- predict(naiveBayes_model,trainADS)
confusionMatrix(Prediction_NB,trainADS$Survived )

#Accuracy = 79.57%
