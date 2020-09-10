
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions

## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 

list.files(path = "../input")

## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.

library(ggplot2)
library(dplyr)
library(GGally)
library(rpart)
library(rpart.plot)
library(randomForest)

test <- read.csv("../input/test.csv",stringsAsFactors = FALSE)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)

LT=dim(train)[1]

#bind the rows without considering the columns as constraint
full <- bind_rows(train,test)

#checking the NA values
colSums(is.na(full)) #SURVIVED, Age and Fare having NA VALUES with count

colSums(full=="") #also helped to know oly NA values and empty string, 
                  #Embarked  and cabin having empty values

#feed values in the embarked column 
full$Embarked[full$Embarked == ""] <- "S" #s Has a lot values in the list

# Let's see how many features we can move to factors

apply(full,2, function(x) length(unique(x)))


factor_variables <- c("Survived","Pclass","Sex","Embarked")

for (i in factor_variables) {
  full[,i] <- as.factor(full[,i])
}

str(full)

#analysis by graph
#sex vs survived with train dataset, sex vs survival
ggplot(data=full[1:LT,],aes(x=Sex,fill=Survived))+geom_bar()

#embarked vs survival
ggplot(data = full[1:LT,],aes(x=Embarked,fill=Survived))+
  geom_bar(position="fill")+ylab("Frequency")

t<-table(full[1:LT,]$Embarked,full[1:LT,]$Survived)
for (i in 1:dim(t)[1]){
  t[i,]<-t[i,]/sum(t[i,])*100
}
t

#Pclass vs survival
ggplot(data = full[1:LT,],aes(x=Pclass,fill=Survived))+
  geom_bar(position="fill")+ylab("Frequency")

#embarked vs survival, facet_wrap with Pclass
ggplot(data = full[1:LT,],aes(x=Embarked,fill=Survived))+
  geom_bar(position="fill")+facet_wrap(~Pclass)

#sibsp vs survival
ggplot(data = full[1:LT,],aes(x=SibSp,fill=Survived))+geom_bar()

#Parch vs survival
ggplot(data = full[1:LT,],aes(x=Parch,fill=Survived))+geom_bar()


#age vs survival, in histogram
ggplot(data = full[!(is.na(full[1:LT,]$Age)),],aes(x=Age,fill=Survived))+geom_histogram(binwidth =3)

#Fare vs survival, in histogram
ggplot(data = full[1:LT,],aes(x=Fare,fill=Survived))+geom_histogram(binwidth =20, position="fill")

#Add the mean value of Fare in the NA values
full$Fare[is.na(full$Fare)] <- mean(full$Fare,na.rm=T)

sum(is.na(full$Age))

#Add the mean value of age in the NA values
#full$Age[is.na(full$Age)] <- mean(full$Age,na.rm=T)
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data = full[!is.na(full$Age),], method = "anova")

full$Age[is.na(full$Age)] <- predict(predicted_age, full[is.na(full$Age),])




#Title column generated from the name
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
full$Title[full$Title == 'Mlle']<- 'Miss' 
full$Title[full$Title == 'Ms']<- 'Miss'
full$Title[full$Title == 'Mme']<- 'Mrs' 
full$Title[full$Title == 'Lady']<- 'Miss'
full$Title[full$Title == 'Dona']<- 'Miss'

#changed the other column values as officer
officer<- c('Capt','Col','Don','Dr','Jonkheer','Major','Rev','Sir','the Countess')
full$Title[full$Title %in% officer]<-'Officer'

full$Title<- as.factor(full$Title)

#Title vs Survival
ggplot(data = full[1:LT,],aes(x=Title,fill=Survived))+geom_bar(position="fill")+ylab("Frequency")







#Model Prediction 
train_im<- full[1:LT,c("Survived","Pclass","Sex","Age","Fare","SibSp","Parch","Title")]
ind<-sample(1:dim(train_im)[1],500) # Sample of 500 out of 891
train1<-train_im[ind,] # The train set of the model
train2<-train_im[-ind,] # The test set of the model

# Let's try to run a logistic regression
model <- glm(Survived ~.,family=binomial(link='logit'),data=train1)
summary(model)

glm(formula = Survived ~ ., family = binomial(link = "logit"),data = train1)

#Survival Prediction
pred.train <- predict(model,train2)
pred.train <- ifelse(pred.train > 0.5,1,0)

mean(pred.train==train2$Survived)

#Precision and recall the model
t1<-table(pred.train,train2$Survived)
presicion<- t1[1,1]/(sum(t1[1,]))
recall<- t1[1,1]/(sum(t1[,1]))

presicion


recall

#F1 Score
F1<- 2*presicion*recall/(presicion+recall)


# F1 score on the initial test set is 0.871. This pretty good.

# Let's run it on the test set:

test_im <- full[LT+1:1309,c("Pclass","Sex","Age","SibSp","Parch","Fare","Title")]

pred.test <- predict(model,test_im)[1:418]
pred.test <- ifelse(pred.test > 0.5,1,0)
res<- data.frame(test$PassengerId,pred.test)
names(res)<-c("PassengerId","Survived")


#Decision Tree Model
model_dt<- rpart(Survived ~.,data=train1, method="class")
rpart.plot(model_dt)

#predicting the test data
pred.train.dt <- predict(model_dt,train2,type = "class")
mean(pred.train.dt==train2$Survived)


t2<-table(pred.train.dt,train2$Survived)

presicion_dt<- t2[1,1]/(sum(t2[1,]))
recall_dt<- t2[1,1]/(sum(t2[,1]))
presicion_dt

recall_dt


F1_dt<- 2*presicion_dt*recall_dt/(presicion_dt+recall_dt)
F1_dt

pred.test.dt <- predict(model_dt,test_im,type="class")[1:418]
res_dt<- data.frame(test$PassengerId,pred.test.dt)
names(res_dt)<-c("PassengerId","Survived")


write.csv(res_dt,file="gender_submission.csv",row.names = F)

model_rf<-randomForest(Survived~.,data=train1)
plot(model_rf)

#prediction based on random forest
pred.train.rf <- predict(model_rf,train2)
mean(pred.train.rf==train2$Survived)


t1<-table(pred.train.rf,train2$Survived)
presicion<- t1[1,1]/(sum(t1[1,]))
recall<- t1[1,1]/(sum(t1[,1]))
presicion

recall

F1<- 2*presicion*recall/(presicion+recall)
F1

pred.test.rf <- predict(model_rf,test_im)[1:418]
res_rf<- data.frame(test$PassengerId,pred.test.rf)
names(res_rf)<-c("PassengerId","Survived")

