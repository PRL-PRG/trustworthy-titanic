
# TITANIC PREDICTION - RULE LEARNER

#We are going to use the Rule Learner to make our prediction

#Loading libraries
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(plyr)
library(dplyr)
library(mice)
library(tibble)
library(RWeka)

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
#Third graph, port where passanger embarked, we see it may exist a tendency, despite the fact that right now we don't see a logic. We will consider it and
#in case it is usefull or useless, model will tell us.

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

#Traveling alone
#We also will include a variable of repeated Tickets (people who bought tickets together)

titanic <- ddply(titanic,.(Ticket),transform,Ticketsize=length(Ticket))
titanic$Ticketsize <- as.factor(titanic$Ticketsize)
titanic <- titanic[order(titanic$PassengerId),]

#Finally, we consider to add a new variable, Adult. We certainly know from movie the phrase "Women and Children first"
#Our aim is to generate Adult variable to report us whether a passanger is adult or not,
# in fact, specific age is not relevant for us here.

titanic$Adult[titanic$Age<18]=0
titanic$Adult[titanic$Age>17]=1


#Dividing the data
#Finally we split into train and test again, taking into consideration that we reduced the number of train values
titanic_train=titanic[1:714,]
titanic_test=titanic[715:1132,]


head(titanic_train)
ggplot(titanic_train)+geom_bar(aes(Survived))+facet_wrap(~Adult)
ggplot(titanic_train)+geom_bar(aes(Survived))+facet_wrap(~Family)
ggplot(titanic_train)+geom_bar(aes(Survived))+facet_wrap(~Deck)
ggplot(titanic_train)+geom_bar(aes(Survived))+facet_wrap(~title)


#Let's analyze the rest of data
#Adult variable (younger or older than 18) has tendency
#Family members has an impact on dataset
#Deck variable seems it doesn't affect. We will consider it anyway.
#Title is a representative variable (it is related with Sex in a direct way).

#It is time to analyze all data and get our forecast



#Rule Learner

#Personally I like Rule learner as it is giving us clear information about factors that has an impact on the final results. 

titanic_model_RL <- JRip(Survived ~ Pclass +Sex+ Adult+ Family + Deck + title + Embarked + Ticketsize, data = titanic_train)
prediction_RL=predict(titanic_model_RL,titanic_test)

titanic_model_RL

#Rule learner has generated 4 rules for the data set. The first 3 rules are quite precise. Unfortunately, 4th is just pointing that besides 
#3 previous cases, nobody survived. As you may see, it has a error % of 20%. Rule learner has not been able to generate any rule for this set.



#Writting results

results=data.frame(titanic_test$PassengerId)
results$Survived=prediction_RL
results$Survived=as.integer(results$Survived)-1
colnames(results)=c("PassengerID","Survived")
write.csv(results, file="results.csv",row.names=FALSE)
