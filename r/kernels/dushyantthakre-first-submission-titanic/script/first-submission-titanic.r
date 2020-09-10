
# First Kaggle submission with Logistic Regression

# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(Hmisc) # to impute missing values
library(dplyr) # basic Manipulation
library(ROCR) # for performance and accuracy measurment
library(rpart) 
library(caret)
library(randomForest)
library(gbm)
# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input/")

# Any results you write to the current directory are saved as output.

# Read data 
Train <- read.csv("../input/train.csv")
Test <- read.csv("../input/test.csv")

# understanding Traing dataset
head(Train) 
summary(Train)
str(Train)

sum(Train$Survived)
table(Train$Survived)

ggplot(Train, aes(x=Survived)+geom_bar()

# total 342 survived from the disaster (in Training dataset)
# Lets check corelation with other variables
table(Train$Sex)
table(Train$Survived, Train$Sex)

ggplot(Train, aes(x=Sex,fill= factor(Survived))) +geom_bar()

# Total: 891 entries present in training dataset, 314 female and 577 Male passenger were traveling
# out of 314 female passenger 283 survived and out of 577 male passenger 109 survived
# seems female passengers have beeter chance of survival.
table(Train$Age<10, Train$Survived, Train$Sex)
# What about kids below age 10
Train %>%group_by(Age<10, Sex ,Survived=1) %>% summarise(n())


# Lets Check Passanger class and survival 
table(Train$Survived,Train$Pclass )
ggplot(Train, aes(x=Pclass, fill=factor(Survived))) + geom_bar()

ggplot(Train, aes(x=Pclass, fill=factor(Survived))) + geom_bar()+facet_grid(Sex~.)
table(Train$Survived,Train$Pclass ,Train$Sex)

# female passanger travelling in pClass 1 or 2 has higested survival rate
# Male passanger travelling in pClass 1 has slightly good chances of survival compare to other pClass

#Lets Check Embark
table(Train$Survived, Train$Embark)
ggplot(Train, aes(x=Embarked, fill=factor(Survived))) + geom_bar()

### Seems we have missing values for Embark and all missing pasangers survived.
### lets check missing were Male or female
table(Train$Survived, Train$Embark, Train$Sex)
ggplot(Train, aes(x=Embarked, fill=factor(Survived))) + geom_bar()+facet_grid(Sex~.)

# From Above output again female passangers has better the survival
# Lets start with Model Bulding

## pasanger travelling with parents and childerns 
table(Train$Survived, Train$Parch)
ggplot(Train, aes(x=Parch, fill=factor(Survived))) + geom_bar() + facet_grid(Sex~.)


## seems most of the males are travelling alone and alone pasangers are lower in suvival
#SibSp
table(Train$SibSp, Train$Survived)
ggplot(Train, aes(x=SibSp, fill=factor(Survived))) + geom_bar() + facet_grid(Sex~.)


ggplot(Train, aes(x=Fare, fill= factor(Survived))) + geom_density()

#remove multiple categorical variable to single category (Binary) variable
# from Training dataset 
Train$Male <-as.factor(as.character(ifelse(Train$Sex=="male",1,0)))
Train$pc1 <- as.factor(as.character(ifelse(Train$Pclass==1,1,0)))
Train$pc2 <- as.factor(as.character(ifelse(Train$Pclass==2,1,0)))
Train$pc3 <- as.factor(as.character(ifelse(Train$Pclass==3,1,0)))
Train$Survived <- as.factor(as.character(Train$Survived))



## Playing with Name
Train$Title <- gsub('(.*, )|(\\..*)', '', Train$Name )
str(Train$Title)
levels(as.factor( Train$Title))
## Survival rate 
table(as.factor(Train$Title),Train$Survived)
rare_title <- c("Capt","Col", "Don", "Dr", "Jonkheer", "Lady","Major","Rev","Sir","the Countess" )
index <- which(Train$Title == "Mlle")
Train$Title[index] <- "Miss"
index <- which(Train$Title == "Ms")
Train$Title[index] <- "Mrs"
index <- which(Train$Title == "Mme")
Train$Title[index] <- "Mrs"
index <- which(Train$Title == "Dona")
Train$Title[index] <- "rare_title"
Train$Title[Train$Title %in% rare_title] <- "rare_title"
Train$Title <- as.factor(Train$Title)
#### Final factor for title
table(as.factor(Train$Title),Train$Survived)

### Family Variable 
Train$Fmaily <- Train$SibSp + Train$Parch +1
table( Train$Survived,Train$Fmaily)
ggplot(Train, aes(x=Fmaily, fill= factor(Survived))) + geom_bar()


# from test data set
Test$Male <-as.factor(as.character(ifelse(Test$Sex=="male",1,0)))
Test$pc1 <- as.factor(as.character(ifelse(Test$Pclass==1,1,0)))
Test$pc2 <- as.factor(as.character(ifelse(Test$Pclass==2,1,0)))
Test$pc3 <- as.factor(as.character(ifelse(Test$Pclass==3,1,0)))
Test$Survived <- as.factor(as.character(Test$Survived))

# check summary 
summary(Train)

##  Missing value treatment for age variable
Train$Age<- impute(Train$Age,mean)
Test$Age <- impute(Test$Age,mean)

str(Train)

# Check for missing values and 
Train <-  subset(Train, select = -c(Ticket, Fare, Cabin,PassengerId,Name,Sex,Pclass))
Test <- subset(Test, select = -c(Ticket, Fare, Cabin,PassengerId,Name,Sex,Pclass))


# GBM

gbm_Mod <- gbm(Survived~.,data = Train, distribution = "adaboost",n.trees = 500)
gbm_Mod
summary(gbm_Mod)

############### Validation  ###########
pred_gbm_MOD <- predict.gbm(gbm_Mod, newdata = Test, n.trees = 300)
pred_gbm_MOD


summary(pred_gbm_MOD)
predict_gbm <- ifelse(pred_gbm_MOD>0.75,1,0)
table(predict_gbm,Test$Survived)

### ROC ##########################
ROC_Pred_GBM <- prediction(predict_gbm, Test$Survived)
ROC_Pred_GBM@tp


ROC_Pref_GBM <- performance(ROC_Pred_GBM,"tpr","fpr")
ROC_Pref_GBM
plot(ROC_Pref_GBM)
abline(0,1,lty=8,col="grey")



auc_gbm <- performance(ROC_Pred_GBM, "auc")
auc_gbm@y.values

# Write Submission to csv


