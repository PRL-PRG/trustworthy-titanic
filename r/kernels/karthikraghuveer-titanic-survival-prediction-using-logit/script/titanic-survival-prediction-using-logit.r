
## Importing packages

# This R environment comes with all of CRAN and many other helpful packages preinstalled.
# You can see which packages are installed by checking out the kaggle/rstats docker image: 
# https://github.com/kaggle/docker-rstats

library(tidyverse) # metapackage with lots of helpful functions
library(VIM)
library(data.table)
library(tidyr)
library(readr)
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






#####Read data from the files#######

Training_data<-read.csv('../input/train.csv',na.strings =c("","na") )
Test_data<-read.csv('../input/test.csv',na.strings=c("",'na'))
gender<-read.csv('../input/gender_submission.csv')

###Getting insight into the datatype####
str(Training_data)


######Informing the model about factors######
Training_data$Pclass<-factor(Training_data$Pclass)
Training_data$Survived<-factor(Training_data$Survived)
Training_data$Embarked<-factor(Training_data$Embarked)



Test_data$Pclass<-factor(Test_data$Pclass)




#######Summary of Training data#####
summary(Training_data)


######This line of code helps us identify the count NA values in differnet columns##
NA_value_Train<-sapply(Training_data,function(x) sum(is.na(x)))
NA_value_Test<-sapply(Test_data,function(x) sum(is.na(x)))

#### We observe that Cabin has highest NA values ######
Training_data$Cabin<-NULL  ####Removing the cabin column 
Test_data$Cain<-NULL

#####Handling the Missing data using KNN Imputation technique#####
imputed_Training_data<-kNN(Training_data,variable ='Age')
imputed_test_data<-kNN(Test_data,variable=c('Age','Fare'))

#####Removing the logical variable added due to imputation####
imputed_Training_data$Age_imp<-NULL
imputed_test_data$Age_imp<-NULL
imputed_test_data$Fare_imp<-NULL
####Applying the logistic regression model on the train data######

survive_model<-glm(Survived ~Age+Sex+Pclass+SibSp+Fare+Embarked+Parch, data =imputed_Training_data,family ='binomial')

#####Prediciting the test data######

survival_test<-predict(survive_model,newdata=imputed_test_data,type = 'response')


######Checking accuracy through confusion matrix ######
pred=rep(0,nrow(imputed_test_data))
pred[survival_test >0.56]=1

table(pred,gender$Survived,dnn =c('Predicted','Actual'))




