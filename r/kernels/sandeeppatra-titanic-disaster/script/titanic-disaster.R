# Loading the data
```{r, message=FALSE, warning=FALSE}
train <- read.csv(".../input/train.csv", stringsAsFactors = F)
test  <- read.csv(".../input/test.csv", stringsAsFactors = F)

#Add survived variable to test to make it 12 columns(= no. of columns in train)
test.survived=data.frame(Survived=rep("None", nrow(test)), test[,])
View(test.survived)

#Reorder test.survived
test.survived=test.survived[,c(2,1,3:12)]

#Combine data sets
data.combined=rbind(train, test.survived)
str(data.combined)
nrow(data.combined)

#Change into factors
data.combined$Survived=as.factor(data.combined$Survived)
data.combined$Pclass=as.factor(data.combined$Pclass)
str(data.combined)

#Take a look at the gross survival rate
table(data.combined$Survived)

#Distribution across classes
table(data.combined$Pclass)

#Load up ggplot2
library(ggplot2)


#Hypothesis- Rich people survived at a higher rate
train$Pclass= as.factor(train$Pclass)

ggplot(train, aes(x=Pclass, fill=factor(Survived)))+
  geom_bar(width=0.5)+
  xlab("PClass")+
  ylab("Total Count")+
  labs(fill="Survived")

#Get unique names in train and test data sets
length(unique(as.character(data.combined$Name)))

#Get the duplicate names and store them as a vector
dup.names=as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)

#Any correlation between Mr. and Mrs. variable
Miss=data.combined[which(str_detect(data.combined$Name, "Miss.")),]
Miss[1:5,]

#Name title correlates with age
Mrses=data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
Mrses[1:5,]

#Check out males to see if pattern continues
males=data.combined[which(train$Sex=="male"),]
males[1:5,]

# Expand on the relationship between 'Survived'and 'Pclass' by adding the new 
# 'Title' variable to the data set and then explore a 3-d relationship.

#Create utility function to help with title extraction
extractTitle=function(Name){
  Name=as.character(Name)
  
  if(length(grep("Miss.", Name))>0){
    return("Miss.")
  }else if(length(grep("Master.", Name))>0){
    return("Master.")
  }else if(length(grep("Mrs.", Name))>0){
    return("Mrs.")
  }else if(length(grep("Mr.", Name))>0){
    return("Mr.")
  }else{
    return("Other")
  }
}

titles=NULL
for (i in 1:nrow(data.combined)) {
  titles=c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$title=as.factor(titles)
View(data.combined)

#Since we only have survived labels for the train set,
#only use the first 891 rows
ggplot(data.combined[1:891,], aes(x=title, fill=Survived))+
  geom_bar(width = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill="Survived")

#Distribution of Males and Females
table(data.combined$Sex)

#Visualize the 3 way relationship between sex, pClass and survival, compare to analyse
ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived))+
  geom_bar(width = 05)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill="Survived")

#Distribution of age
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

#Relationship between "age", "sex", and pClass and survival
ggplot(data.combined[1:891,], aes(x=Age, fill=Survived))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~Sex+Pclass)+
  xlab("Age")+
  ylab("Total Count")

#Validate that Master is a good proxy for male children
boys= data.combined[which(data.combined$title=="Master."),]
summary(boys$Age)

#Examine Miss.
misses=data.combined[which(data.combined$title=="Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived !="None",], aes(x=Age, fill=Survived))+
  facet_wrap(~Pclass)+
  geom_histogram(binwidth = 0.5)+
  ggtitle("Age for Misses by pClass")+
  xlab("Age")+
  ylab("Total Count")

#Check survival rate of female children
misses.alone=misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age<14.5))

#Analyse the number of siblings/spouses sibsp variable
summary(data.combined$SibSp)

length(unique(data.combined$SibSp))

#Convert into factor
data.combined$SibSp=as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,], aes(x=SibSp, fill=Survived))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~Pclass+title)+
  ggtitle("pClass, title")+
  xlab("sibsp")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

#Analyse the number of  parent/chilrdren parch variable
summary(data.combined$Parch)

length(unique(data.combined$Parch))

#Convert into factor
data.combined$SibSp=as.factor(data.combined$SibSp)

ggplot(data.combined[1:891,], aes(x=Parch, fill=Survived))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~Pclass+title)+
  ggtitle("pClass, title")+
  xlab("Parch")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")

#Feature Engineering. Create a  family size (sibsp+parch) variable
temp.sibsp=c(train$SibSp,test$SibSp)
temp.parch=c(train$Parch,test$Parch)
data.combined$familySize=as.factor(temp.sibsp+temp.parch+1) #children+parents+myself
#Visualize to see if family size is predictive
ggplot(data.combined[1:891,], aes(x=familySize, fill=Survived))+
  geom_histogram(binwidth = 1)+
  facet_wrap(~Pclass+title)+
  ggtitle("Pclass, title")+
  xlab("familySize")+
  ylab("Total Count")+
  labs(fill="Survived")

#Ticket variable
str(data.combined$Ticket)
#Convert ticket variable into character
data.combined$Ticket=as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

#Take a look at the first character
ticket.first.char=ifelse(data.combined$Ticket=="", " ", substr(data.combined$Ticket,1,1))
unique(ticket.first.char)

#Add ticket.first.char to data.combined, as factors
data.combined$ticket.first.char=as.factor(ticket.first.char)

#Survivability by ticket.first.char
ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill=Survived))+
  geom_bar()+
  ggtitle("Survivability by ticket.first.char")+
  xlab("ticket.first.char")+
  ylab("Total Count")+
  ylim(0,350)+
  labs(fill="Survived")

#Plot ticket.firt.char and pClass
ggplot(data.combined[1:891,], aes(x=ticket.first.char, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("PClass")+
  xlab("ticket.first.char")+
  ylab("Total Count")+
  ylim(0,150)+
  labs(fill="Survived")

#Plot ticket.firt.char and pClass and title
ggplot(data.combined[1:891,], aes(ticket.first.char, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+title)+
  ggtitle("pClass, Title")+
  xlab("ticket.first.char")+
  ylab("Total Count")+
  ylim(0,200)+
  labs(fill="Survived")

#Analyze the fares Titanic passangers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined[1:891,], aes(x=Fare, fill=Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass+title)+
  ggtitle("Pclass, Title")+
  xlab("Fare")+
  ylab("Total Count")+
  ylim(0,50)+
  labs(fill="Survived")

#Analysis of cabin variable
str(data.combined$Cabin)
data.combined$Cabin=as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

#Replace blank cabins with "U"
data.combined[which(data.combined$Cabin==""), "Cabin"]="U"

#Take a look at the first character as factor
cabin.first.char=as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)

#Add to combined data set and plot
data.combined$cabin.first.char=cabin.first.char

#High level plot
ggplot(data.combined[1:891,], aes(x=cabin.first.char, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+title)+
  ggtitle("pClass, Title")+
  xlab("cabin.first.char")+
  ylab("Total Count")+
  ylim(0,500)+
  labs(fill="Survived")
  
#Analyse the embarked variable
str(data.combined$Embarked)
levels(data.combined$Embarked)

#Plot data for analysis
ggplot(data.combined[1:891,], aes(x=Embarked, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+title)+
  ggtitle("pClass, Title")+
  xlab("Embarked")+
  ylab("Total Count")+
  ylim(0,300)+
  labs(fill="Survived")


##################################
#
#Exploratory Modelling
#
##################################
#Train a random forest model using pClass and title
rf.train.1=data.combined[1:891, c("Pclass", "title")]
rf.label=as.factor(train$Survived)

set.seed(1234)
rf.1=randomForest::randomForest(x=rf.train.1, y=rf.label, importance=TRUE, ntree=1000)
rf.1
randomForest::varImpPlot(rf.1)

#Train a random forest model using pClass, title and sibsp
rf.train.2=data.combined[1:891, c("Pclass", "title", "SibSp")]
str(rf.train.2)

set.seed(1234)
rf.2=randomForest::randomForest(x=rf.train.2, y=rf.label, importance=TRUE, ntree=1000)
rf.2
randomForest:: varImpPlot(rf.2)

#Train a random forest model using pClass, title and parch
rf.train.3=data.combined[1:891, c("Pclass", "title", "Parch")]
str(rf.train.3)

set.seed(1234)
rf.3=randomForest::randomForest(x=rf.train.3, y=rf.label, importance=TRUE, ntree=1000)
rf.3
randomForest:: varImpPlot(rf.3)

#Train a random forest model using pClass, title sibsp and parch
rf.train.4=data.combined[1:891, c("Pclass", "title", "SibSp", "Parch")]
str(rf.train.4)

set.seed(1234)
rf.4=randomForest::randomForest(x=rf.train.4, y=rf.label, importance=TRUE, ntree=1000)
rf.4  
###### Lowest error rate --- 18.18% ######
randomForest:: varImpPlot(rf.4)

#Train a random forest model using pClass, title and family.size
rf.train.5=data.combined[1:891, c("Pclass", "title", "familySize")]
str(rf.train.5)

set.seed(1234)
rf.5=randomForest::randomForest(x=rf.train.5, y=rf.label, importance=TRUE, ntree=1000)
rf.5
randomForest:: varImpPlot(rf.5)

#Train a random forest model using pClass, title, Sibsp and family.size
rf.train.6=data.combined[1:891, c("Pclass", "title", "SibSp", "familySize")]
str(rf.train.6)

set.seed(1234)
rf.6=randomForest::randomForest(x=rf.train.6, y=rf.label, importance=TRUE, ntree=1000)
rf.6
randomForest:: varImpPlot(rf.6)

#Train a random forest model using pClass, title and family.size
rf.train.7=data.combined[1:891, c("Pclass", "title", "SibSp", "Parch", "familySize")]
str(rf.train.7)

set.seed(1234)
rf.7=randomForest::randomForest(x=rf.train.7, y=rf.label, importance=TRUE, ntree=1000)
rf.7
randomForest:: varImpPlot(rf.7)

#################
#
#Cross Validation
#
#################

#Subset our test records to make predictions
test.submit.df=data.combined[892:1309, c("Pclass","title",  "SibSp", "Parch")]

#Make predictions
rf.4.preds=predict(rf.4, test.submit.df)
table(rf.4.preds)

#Write out a csv file
submit.df=data.frame(PassengerId=rep(892:1309), Survived=rf.4.preds)

write.csv(submit.df, file = "RF_SUBMIT_20161116_1.CSV", row.names = FALSE)

####Let's look into cross validation
library(caret)
library(doSNOW)

#10-fold cross validation
set.seed(2348)
cv.10.folds= createMultiFolds(rf.label, k=10, times = 10)

#Check stratification
table(rf.label)
342/549

table(rf.label[cv.10.folds[[33]]])
308/409

# Set up caret's trainControl object per above.
ctrl.1= trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

# Setup doSNOW package for multicore training
cl= makeCluster(6, type="SOCK")
registerDoSNOW(cl)

# Set seed for reproducibility and train
set.seed(34324)
rf.4.cv.1= train(x=rf.train.4, y=rf.label, method="rf", tuneLength=3, ntree=1000, trControl=ctrl.1)

# Shutdown cluster
stopCluster(cl)

# Check out results
rf.4.cv.1


#Install and load rpart package
library(rpart)
library(rpart.plot)

# 3 fold cross validation repeadted 10 times

#Create utility function
rpart.cv=function(seed, training, labels, ctrl){
  cl= makeCluster(6, type="SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  #Leverage formula interface for training
  rpart.cv=train(x=training, y=labels, method="rpart", tuneLength=30, trControl=ctrl)
  
  #shut down cluster
  stopCluster(cl)
  
  return(rpart.cv)
}

#Grab features
features=c("Pclass", "title", "SibSp", "Parch")
rpart.train.1= data.combined[1:891, features]

#Run cv and check out results
rpart.1.cv.1=rpart.cv(94622, rpart.train.1, rf.label, ctrl.1)
rpart.1.cv.1

#Plot
prp(rpart.1.cv.1$finalModel, type=0, extra=1, under=TRUE)

#PLOT:
#     1- Titles of Mr. and other perished with an accuracy of 83.2%.
#     2- Titles Master, Miss. and Mrs in 1st and 2nd class survived with
#        accuracy of 94.9%.
#     3- Titles Master, Miss. and Mrs in 3rd class with siblings 3,4,5,8 
#        perished with 89% accuracy.
#     4- Titles Master, Miss. and Mrs in 3rd class with siblings not equal
#        to 3,4,5,8 survived with 58% accuracy.