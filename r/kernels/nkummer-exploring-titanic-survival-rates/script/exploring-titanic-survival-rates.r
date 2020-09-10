
#include libraries
library(ggplot2) #library for producing plots

system("ls ../input")
#load the training data:
df=read.csv("../input/train.csv",stringsAsFactors=FALSE) 

#Display a summary of all the variables and their type
str(df)


#Change the survived variable to make summary tables prettier:
df$Survived=factor(df$Survived, 
                   levels=c(0,1),
                   labels =c("died","lived"))


df$Sex=factor(df$Sex) #change the gender variable to a factor
table(df$Survived,df$Sex) #See a summary mortality by gender

options(repr.plot.width=5, repr.plot.height=3)#Plot size Options

#Determine age distribution
age_range=cut(df$Age, seq(0,100,10)) #Sub-divide the ange range into 10 year sections
qplot(age_range, xlab="Age Range", main="Age distribution on the Titanic") #plot age distributon

#Determine survival percentage:
ggplot(df, aes(x=Age, fill=Survived))+
  geom_histogram(binwidth = 5,position="fill")+
  ggtitle("Survival percentage amongst the age groups")

#check percentage of unknown age passengers:
print("Survival rate of passengers who's age is unknown:")
table(df$Survived[is.na(df$Age)]) 

#Replace the missing age entries with the average age
df$Age[is.na(df$Age)]=mean(df$Age, na.rm=TRUE)

## Embark Location
The next step is to examine/clean the embark location 

#Explore embark location
df$Embarked[df$Embarked==""]="S" #replace missing values with majority (S), highest chance of being right
df$Embarked=factor(df$Embarked, levels=c("S","C","Q")) #Set as factor in order of S->C->Q
table(df$Survived,df$Embarked) #show summary table of survival chances

print("Survival of people who have parents/children aboard")
table(df$Survived,df$Parch) #parent children

print("Survival of people who have siblings/spouses aboard")
table(df$Survived,df$SibSp) #siblings/spouse

print("Survival rate against class")
table(df$Survived,df$Pclass) #Summary of passenger vs. class

#Show the histogram of the log-fare
hist(log(df$Fare)) #histogram, which looks more normal than the skewed Fare distribution

#Some values have Fare=0, this is not good for the log-fare, so we change these values with
#the mean of the log-fare
df$Fare[df$Fare==0] = mean(log( df$Fare[df$Fare>0])  )
df$logfare=log(df$Fare)

#Show the survival as a function of log Fare
ggplot(df, aes(x=log(Fare), fill=Survived))+
  geom_histogram(binwidth = 0.5,position="fill")+
  ggtitle("Survival likelyhood vs. log-fare")


library(caret) #
library(e1071)
set.seed(3456) #set a seed for reproducible results

trainIndex <- createDataPartition(df$Survived, p = .8,list=FALSE)
df_train=df[trainIndex,]
df_test=df[-trainIndex,]

model.t=train(Survived~Sex+Age+Embarked+logfare+Pclass+SibSp+Parch,
              data=df_train,
              method='glm') #use a generalized linear model

newval=predict(model.t, newdata=df_test)
confusionMatrix(newval, df_test$Survived)
