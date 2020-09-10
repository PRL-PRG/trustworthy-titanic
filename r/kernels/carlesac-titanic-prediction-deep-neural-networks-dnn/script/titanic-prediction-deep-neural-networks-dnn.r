
# TITANIC PREDICTION - DNN

#We are going to use the DNN to make our prediction

#Loading libraries
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(plyr)
library(dplyr)
library(mice)
library(tibble)
library(RWeka)
library(neuralnet)
library(deepnet)

#Function
normalize <- function(x) {  return((x - min(x)) / (max(x) - min(x)))}

#Loading data
train=read.csv("../input/train.csv", header=TRUE)
train=na.omit(train) # Deleting NA values from Train
test=read.csv("../input/test.csv",header = TRUE)
test$Survived <- NA # Adding the column Survived to test data frame
titanic=rbind(train,test)


#Review data

#The best way to understand data is plotting some graphs, so let's do it:

head(train)
ggplot(train)+geom_bar(aes(Survived))+facet_wrap(~Sex)
ggplot(train)+geom_bar(aes(Survived))+facet_wrap(~Pclass)
ggplot(train)+geom_bar(aes(Survived))+facet_wrap(~Embarked)

#So we are analyzing the data as it is, without edditing it (remember that we are checking only train dataset as we know if they survived or not)
#As you may see in first graph, sex is an important factor. Majority of females survived. On the other hand, majority of males perished. 
#Reason is clear, so obviously, we will consider this variable when we perform our forecast.
#On second graph we plot Survived vs No survived in function of class. There is also a tendency, majority of passangers of third class perished.
#Probably it was more difficult to escape from bottom floors, so we'll included it.
#Third graph, port where passanger embarked, we see it may exist a tendency, despite the fact that right now we don't see a logic. We will consider it and in case it is usefull or useless, model will tell us.
#Right now, it is not comfortable to display the rest of variables (missing/complex data), process work is needed, so let's start with the hard work.

#Once we got the data, it is time to prepare it for extract the maximum profit from it. It is necessary to process it to allow any AI to get 
# some decent result. 

#Editing data

#We turn Survived into a factor to be able to use later the Rule Learner
titanic$Survived=as.factor(titanic$Survived) 

#We calculated the number of family members from Parch and SibSp variables
#If you did not check it on data explanation, Parch is the number of parents / children and SibSp the number of Siblings and spouses 

titanic$Family=titanic$SibSp+titanic$Parch+1 

# Cabin variable gives us very specific information of passengers location, maybe, to much specific. Unfortunately is not completed for all passengers,
# what makes everything more complicated. 
# To get a more general information, we are going to isolate only the Deck letter (and cross fingers for get significant data)

titanic$Deck=as.factor(substring(titanic$Cabin,1,1)) 

#Analyzing the Name field, we can appreciate all passengers have a title. This can be usefull to classify passengers.
#We get a new column with the title

titanic$title=gsub("^.*, (.*?)\\..*$", "\\1",titanic$Name)

#There are many titles quite similar between them, so we're gonna to simplify them:

titanic$title[titanic$title == 'Mlle']='Miss'
titanic$title[titanic$title == 'Ms']          <- 'Miss'
titanic$title[titanic$title == 'Mme']         <- 'Mrs' 
titanic$title[titanic$title == 'Lady']          <- 'Miss'
titanic$title[titanic$title == 'Dona']          <- 'Miss'
titanic$title[titanic$title == 'Capt']        <- 'Officer' 
titanic$title[titanic$title =='Col']        <- 'Officer' 
titanic$title[titanic$title == 'Major']   <- 'Officer'
titanic$title[titanic$title =='Dr']   <- 'Officer'
titanic$title[titanic$title == 'Rev']   <- 'Officer'
titanic$title[titanic$title == 'Don']   <- 'Officer'
titanic$title[titanic$title == 'Sir']   <- 'Officer'
titanic$title[titanic$title == 'the Countess']   <- 'Officer'
titanic$title[titanic$title == 'Jonkheer']   <- 'Officer'

titanic$title=as.factor(titanic$title)

#Predicting missing ages
#Unfortunately data misses age values. In this case, we cannot just delete rows, as it is mandatory to predict if they survived or not.
#So here we are going to predict the age taking into consideration relevant values:

set.seed(129)
mice_mod <- mice(titanic[, !names(titanic) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
mice_output <- complete(mice_mod)
titanic$Age <- mice_output$Age

#Finally, we consider to add a new variable, Adult. We certainly know from movie the phrase "Women and Children first"
#Our aim is to generate Adult variable to report us whether a passanger is adult or not,
# in fact, specific age is not relevant for us here.

titanic$Adult[titanic$Age<18]=0
titanic$Adult[titanic$Age>17]=1

#We are going to include a variable for repeated tickets (people who bought tickets together)

titanic <- ddply(titanic,.(Ticket),transform,Ticketsize=length(Ticket))
titanic$Ticketsize <- as.factor(titanic$Ticketsize)
titanic <- titanic[order(titanic$PassengerId),]



#Transforming data into binary

#We need to transform the data into binary to allow NN to generate a correct model. It is a very repetitive task, but quite simple:

titanicN=data.frame(titanic$PassengerId)
titanicN$Survived=titanic$Survived
titanicN$Survived=as.numeric(titanicN$Survived)-1

titanicN$Adult=titanic$Adult

titanicN$P1=0
titanicN$P1[titanic$Pclass==1]=1
titanicN$P2=0
titanicN$P2[titanic$Pclass==2]=1
titanicN$P3=0
titanicN$P3[titanic$Pclass==3]=1

titanicN$Sex=0
titanicN$Sex[titanic$Sex=="male"]=1

titanicN$DA=0
titanicN$DA[titanic$Deck=="A"]=1
titanicN$DB=0
titanicN$DB[titanic$Deck=="B"]=1
titanicN$DC=0
titanicN$DC[titanic$Deck=="C"]=1
titanicN$DD=0
titanicN$DD[titanic$Deck=="D"]=1
titanicN$DE=0
titanicN$DE[titanic$Deck=="E"]=1
titanicN$DF=0
titanicN$DF[titanic$Deck=="F"]=1
titanicN$DG=0
titanicN$DG[titanic$Deck=="G"]=1
titanicN$Dm=0
titanicN$Dm[titanic$Deck=="m"]=1
titanicN$DT=0
titanicN$DT[titanic$Deck=="T"]=1
titanicN$F1=0
titanicN$F1[titanic$Family==1]=1
titanicN$F2=0
titanicN$F2[titanic$Family==2]=1
titanicN$F3=0
titanicN$F3[titanic$Family==3]=1
titanicN$F4=0
titanicN$F4[titanic$Family==4]=1
titanicN$F5=0
titanicN$F5[titanic$Family==5]=1

titanicN$F7=0
titanicN$F7[titanic$Family>6]=1

titanicN$Miss=0
titanicN$Miss[titanic$title=='Miss']=1
titanicN$Mrs=0
titanicN$Mrs[titanic$title=='Mrs']=1
titanicN$Officer=0
titanicN$Officer[titanic$title=='Officer']=1
titanicN$Master=0
titanicN$Master[titanic$title=='Master']=1
titanicN$Mr=0
titanicN$Mr[titanic$title=='Mr']=1

titanicN$Q=0
titanicN$Q[titanic$Embarked=='Q']=1
titanicN$S=0
titanicN$S[titanic$Embarked=='S']=1
titanicN$C=0
titanicN$C[titanic$Embarked=='C']=1

titanicN$T0=0
titanicN$T0[titanic$Ticketsize=='0']=1
titanicN$T1=0
titanicN$T1[titanic$Ticketsize=='1']=1
titanicN$T2=0
titanicN$T2[titanic$Ticketsize=='2']=1
titanicN$T3=0
titanicN$T3[titanic$Ticketsize=='3']=1
titanicN$T4=0
titanicN$T4[titanic$Ticketsize=='4']=1
titanicN$T5=0
titanicN$T5[titanic$Ticketsize=='5']=1
titanicN$T6=0
titanicN$T6[titanic$Ticketsize=='6']=1
titanicN$T7=0
titanicN$T7[titanic$Ticketsize=='7']=1
titanicN$T8=0
titanicN$T8[titanic$Ticketsize=='8']=1

titanicN$Fare=(titanic$Fare)
titanicN[867,40]=15.03
titanicN$Fare=normalize(titanicN$Fare)

head(titanicN)

#Dividing the data
#Finally we split into train and test again, taking into consideration that we reduced the number of train values
titanic_train=titanicN[1:714,]
titanic_test=titanicN[715:1132,]


#DNN

x=as.matrix(titanic_train[,3:40])
y=titanic_train[,2]
titanic_DL=dbn.dnn.train(x,y, hidden=c(20,10),activationfun="tanh",batchsize=128, numepochs=100, learningrate = 0.1)
prediction=nn.predict(titanic_DL,titanic_test[,3:40])
prediction

#Writting results

results=data.frame(titanic_test$titanic.PassengerId)
results$Survived=prediction
results$Survived=normalize(results$Survived)
results$Survived=round(results$Survived, digits=0)
colnames(results)=c("PassengerID","Survived")
write.csv(results, file="results.csv",row.names=FALSE)

results
