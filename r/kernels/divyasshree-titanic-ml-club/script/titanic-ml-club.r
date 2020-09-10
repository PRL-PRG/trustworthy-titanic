
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats


## Running code

# In a notebook, you can run a single code cell by clicking in the cell and then hitting 
# the blue arrow to the left, or by clicking in the cell and pressing Shift+Enter. In a script, 
# you can run code by highlighting the code you want to run and then clicking the blue arrow
# at the bottom of this window.

## Reading in files

# You can access files from datasets you've added to this kernel in the "../input/" directory.
# You can see the files added to this kernel by running the code below. 


titanic_train <- read.csv("../input/train.csv", stringsAsFactors=FALSE)
titanic_test<-read.csv("../input/test.csv", stringsAsFactors=FALSE)

#library(titanic)

library('ggplot2')
library('scales')
library('dplyr') 
library('randomForest') 
library('readr') 

titanic_train<-bind_rows(titanic_train,titanic_test)


titanic_train$title<-gsub('(.*, )|(\\..*)', '', titanic_train$Name)
#change less frequent titles
table(titanic_train$Sex, titanic_train$title)

title_edit<-c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
              'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
titanic_train$title[titanic_train$title=='Mlle']<-'Miss'
titanic_train$title[titanic_train$title=='Ms']<-'Miss'
titanic_train$title[titanic_train$title=='Mme']<-'Mrs'
titanic_train$title[titanic_train$title %in% title_edit]<-'renamed Title'
table(titanic_train$Sex, titanic_train$title)

#know size of each family

titanic_train$famsize <- titanic_train$SibSp + titanic_train$Parch + 1

ggplot(titanic_train[1:891,], aes(x = famsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') 

#famsize>4 and survived=true is very less
#look for missing values

titanic_train$Embarked[titanic_train$Embarked == ""] <- NA
sum(is.na(titanic_train$Age))
#replace NA with median-method 1
titanic_train1<-titanic_train

med<-median(titanic_train$Age,na.rm = TRUE)
#titanic_train1$Age[titanic_train$Age=="NA"]=med

library(Hmisc)

#impute missing age value using known information-method 2

titanic_train1$Age<-with(titanic_train,impute(Age,mean))

titanic_train1$Fare<-with(titanic_train,impute(Fare,median))
titanic_train1$Survived<-with(titanic_train,impute(Survived,0))
titanic_train1$Embarked<-with(titanic_train,impute(Embarked,'C'))

#group family size into classes

titanic_train1$fsizeD[titanic_train1$famsize == 1] <- 1
titanic_train1$fsizeD[titanic_train1$famsize < 5 & titanic_train1$famsize> 1] <- 2
titanic_train1$fsizeD[titanic_train1$famsize> 4] <- 3


train <- titanic_train1[1:891,]
test <- titanic_train1[892:1309,]


# using randomForest on the training set.

# Set a random seed

set.seed(755)

# Build the model 

titanic_model <- randomForest(Survived ~   Age+
                                Fare +  fsizeD+Pclass ,
                              data = train,na.action = na.exclude)


# Show model error

plot(titanic_model, ylim=c(0,0.36))
ope<-predict(titanic_model,test)

importance    <- importance(titanic_model)
Output<- data.frame(PassengerID = test$PassengerId, Survived = ope)
Output$Survived[Output$Survived>0.5]=1
Output$Survived[Output$Survived!=1]=0


## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.
write.csv(Output,file="pred.csv")
