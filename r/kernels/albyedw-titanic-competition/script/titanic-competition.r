
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


library(readr)
titdata=read_csv("../input/train.csv")
nrow(titdata)

target=factor(titdata$Survived,labels=c("died","survived"))
titdata$Survived=target

titdata$Age[is.na(titdata$Age)]=median(titdata$Age,na.rm=TRUE)

cab=as.character(titdata$Cabin)
empty=c(1:length(cab))

for (i in 1:length(cab)){
  if (is.na(cab[i])=="TRUE") {
  empty[i]="Cabin Unknown"
  }
  else {
  empty[i]=substr(cab[i],1,1)
  }
}

empty[1:10]
titdata$cabinfloor=as.factor(empty)
class(titdata$cabinfloor)
levels(titdata$cabinfloor)

titdata$Sex=as.factor(titdata$Sex)
titdata$Embarked=as.factor(titdata$Embarked)
titdata=titdata[-c(62,830),]
nrow(titdata)

library(ggplot2)
library(lattice)

barchart(titdata$Sex,titdata$Survived,xlab="Number Of Survivers")

barchart(titdata$cabinfloor,titdata$Survived,xlab="Number Of Survivers")

ggplot(aes(Survived,Parch), data=titdata)+
geom_boxplot()

ggplot(aes(Survived,Age), data=titdata)+
geom_boxplot()

ggplot(aes(Survived,Fare), data=titdata)+
  geom_boxplot()


ggplot(aes(Survived,SibSp), data=titdata)+
  geom_boxplot()

set.seed(333)
sampsize=floor(0.75*nrow(titdata))
sample=sample(1:nrow(titdata),size=sampsize)
traintit=titdata[sample,]
testtit=titdata[-sample,]

library(randomForest)


mod = randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+cabinfloor+Embarked,data = traintit, ntree =1000,mtry=3,importance = TRUE)
mod
mod$importance

library(caret)

#tuning mtry parameter with grid seacrh method and cross-validation                  
control=trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(222)
tunegrid=expand.grid(.mtry=c(1:9))
rf_gridsearch=train(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+cabinfloor,data=traintit,method="rf",metric="Accuracy",tuneGrid=tunegrid,trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

newmod=randomForest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+cabinfloor,data = traintit, ntree =1000,mtry=4,importance = TRUE)

test=predict(newmod,testtit)
#making predictions on new data/test data with model
actual=testtit$Survived
res=test==actual
x=res[res=="TRUE"]
accuracy=length(x)/length(res)
accuracy

testdata=read_csv("../input/test.csv")
head(testdata)

testdata$Age[is.na(testdata$Age)]=median(testdata$Age,na.rm=TRUE)
testdata$Sex=as.factor(testdata$Sex)
testdata$Embarked=as.factor(testdata$Embarked)
testdata$Fare[is.na(testdata$Fare)]=median(testdata$Fare,na.rm=TRUE)

cab2=as.character(testdata$Cabin)
empty2=c(1:length(cab2))

for (i in 1:length(cab2)){
  if (is.na(cab2[i])=="TRUE") {
  empty2[i]="Cabin Unknown"
  }
  else {
  empty2[i]=substr(cab2[i],1,1)
  }
}

empty2[1:10]
testdata$cabinfloor=as.factor(empty2)

levels(testdata$Embarked)=levels(titdata$Embarked)
levels(testdata$Sex)=levels(titdata$Sex)
levels(testdata$cabinfloor)=levels(titdata$cabinfloor)

prediction=predict(newmod,testdata)
submission=data.frame(PassengerId=testdata$PassengerId,Survived=prediction)
submission
submission$Survived=as.integer(submission$Survived)
submission

for (i in 1:length(submission$Survived)) {
    if (submission$Survived[i]==1){
        submission$Survived[i]=0
        }
    else {
        submission$Survived[i]=1
    }
    }
    
submission

titfile=write.csv(submission,"titanicsubmission.csv")
