## ----message=FALSE, warning=TRUE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(naivebayes)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caTools)
library(corrplot)
library(tidyr)
library(mice)
library(caret)
library(pROC)
library(ROCR)
library(klaR)
library(neuralnet)
library(dummies)



## ----pressure, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train = read.csv("../input/train.csv")
test = read.csv("../input/test.csv")
example = read.csv("../input/gender_submission.csv")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#look at the training data
head(train)
summary(train)
str(train)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#look at testing data
head(test)
str(test)
summary(test)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#remove Survived column from train and combined with test to make a full data set
train2 = train[,-2]
train2$train_or_test = "train"
test = test %>%
  mutate(train_or_test = "test")
full = rbind(train2, test)
head(full)




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#look at missing values
missing_values = colSums(is.na(full))
missing_values
nrow(full)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#delete record with missing Fare value
which(is.na(full$Fare))
full[1044,]$Fare = 14.45


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
which(full$Embarked == "")
#rows 62 and 830 are missing
full[62,]$Embarked = "S"
full[830,]$Embarked = "S"
full$Embarked = droplevels(full$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Break up Name column in First, Last, and Title
full$Name = as.character(full$Name)
full2 = separate(full, Name, into=c("LastName", "Rest"), sep=", ")
full2 = separate(full2, Rest, into=c("title", "First"), sep="\\. ")
as.list(table(full2$title))
barplot(table(full2$title))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
royal_mask = full2$title %in% c("Don", "Dona", "Lady", "Jonkheer", "the Countess", "Sir")
officer_mask = full2$title %in%  c("Col", "Major", "Capt")
miss_mask =  full2$title %in% c("Ms", "Mlle")
dr_mask = full2$title == "Dr"
rev_mask = full2$title == "Rev"
mme_mask = full2$title == "Mme"

#change Don, Dona, Lady, Jonkheer, Countess and Sir to Royal
full2[royal_mask,]$title = "Royal"

#change Col, Maj, and Capt to Officer
full2[officer_mask,]$title = "Officer"

#change Ms, Mlle to Miss
full2[miss_mask,]$title= "Miss"

#change Mme to Mr
full2[mme_mask,]$title = "Mr"



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full2[rev_mask,]
full2[dr_mask,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#change Rev and Dr to Mr
full2[rev_mask,]$title = "Mr"
full2[dr_mask,]$title = "Mr"

#change female doctor back to Mrs
full2[797,]$title = "Mrs"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#look at titles again after all changes
as.list(table(full2$title))
barplot(table(full2$title))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#remove variables that are not useful
no_use = c("Ticket", "Cabin")
full3=full2
full3[,no_use] = NULL
head(full3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#create a dummy variable called MissingAge: 0 if Age is NA, Else 1
full3$MissingAge = 1
missing_age_mask = is.na(full3$Age)
full3[missing_age_mask,]$MissingAge = 0
full3$Pclass = as.factor(full3$Pclass)
head(full3, 25)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#split full data frame in missing age and not missing
split_mask = full3$MissingAge == 1
full3_has_age = full3[split_mask,]
full3_no_age = full3[!split_mask,]
summary(full3_has_age)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(full3_no_age)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full3_has_age, aes(x=Age, fill=Pclass))+
  geom_density(alpha=.3)+
  facet_wrap(Sex~.)



## ----message=FALSE, results= 'hide'----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#perfom imputation on the full3 data set
set.seed(99)
full_imputed = complete(mice(full3, m=10, method = "cart"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#check for na values
sum(is.na(full_imputed))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#check the density plots of imputed DF
ggplot(full_imputed, aes(x=Age, fill=Pclass))+
  geom_density(alpha=.3)+
  facet_wrap(Sex~.)+
  ggtitle("imputed")

ggplot(full3_has_age, aes(x=Age, fill=Pclass))+
  geom_density(alpha=.3)+
  facet_wrap(Sex~.)+
  ggtitle("missing Age values ommited")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#split back into train and test
train_clean = full_imputed[full_imputed$train_or_test == "train",]
test_clean = full_imputed[full_imputed$train_or_test == "test",]
train_clean$train_or_test = NULL
test_clean$train_or_test = NULL
train_clean$MissingAge = NULL
test_clean$MissingAge = NULL

#add survivability back into train
train_clean$Survived = as.factor(train$Survived)
#check for NAs 
sum(is.na(train_clean))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#survivability vs Passenger Class
ggplot(train_clean, aes(x = Pclass, fill = Survived))+
  geom_bar(position = "dodge")+
  labs(title = "Passenger Class Survivability", x = "Passenger Class")




## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#survivability vs Title
ggplot(train_clean, aes(x=as.factor(title), fill=Survived))+
  geom_bar(position = "dodge")+
  labs(title="Title Survivability", x="Title")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#survivability vs Age
p1 = ggplot(train_clean, aes(Age, fill=Survived))+
  geom_histogram()+
  facet_wrap(~Sex)
p2 = ggplot(train_clean, aes(Age, fill=Survived))+
  geom_histogram()
library(gridExtra)
library(grid)
grid.arrange(p2,p1, ncol=1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#survivability vs Sibling or Spouse
ggplot(train_clean, aes(fill=Survived, x=as.factor(SibSp)))+
  geom_bar(position="dodge")+
  labs(title = "Having a sibling or spouse aboard", x="# of siblings or spouse")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#survivability vs Embarked City
ggplot(train_clean, aes(fill=Survived, x=Embarked))+
  geom_bar(position="dodge")+
  labs(title = "Embarked City vs Survivability")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(train_clean, aes(fill=Survived, x=Embarked))+
  geom_bar(position="dodge")+
  facet_wrap(Sex~Pclass)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#survivability vs Parch
ggplot(train_clean, aes(fill=Survived, x=Parch))+
  geom_bar(position = "dodge")+
  labs(title="Parent/Children size group vs Survivability")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# split the training set 80/20
set.seed(100)
train_clean$Survived  = as.factor(train_clean$Survived)
splitter = sample.split(train_clean$Survived, SplitRatio = .8)
train80 = train_clean[splitter,]
train20 = train_clean[!splitter,]


## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#set control parameters for training function
rf_control = trainControl(method = "repeatedcv", number = 10, repeats=3, verboseIter = FALSE)
rf_tune = data.frame(mtry=c(2,4,6,8), min.node.size=c(1,2,3,4), splitrule = c("gini", "extratrees"))

#build model
set.seed(100)
RF_mod = train(Survived ~ Pclass + title + Age + SibSp + Parch+ Embarked + Sex, 
               data = train80,
               method = "ranger",
               trControl = rf_control,
               tuneGrid = rf_tune)

#model results
RF_mod
RF_mod$finalModel


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#model predictions on withheld training data train20
RF_pred = predict(RF_mod, newdata = train20)
confusionMatrix(RF_pred, train20$Survived)
colAUC(as.numeric(RF_pred), as.numeric(train20$Survived), plotROC = TRUE)
RF_ROC = roc(response = as.numeric(train20$Survived), predictor = as.numeric(RF_pred))
pROC::auc(RF_ROC)


## ----error=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(555)
nb_control = trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = FALSE)
nb_mod = train(Survived ~ Pclass + title + Age + SibSp + Parch + Sex + Embarked,
               method = "nb",
               data=train80,
               trControl = nb_control)

summary(nb_mod)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#out of sample predication and performance with Naive Bayes model
nb_pred = predict(nb_mod, newdata = train20, type = "raw")
confusionMatrix((nb_pred), (train20$Survived))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#copy training data sets before modifying
train80nn = train80
train20nn = train20

#set Passenger class to integer
train80nn$Pclass = as.integer(train80nn$Pclass)
train20nn$Pclass = as.integer(train20nn$Pclass)

library(dummies)
dummy_title80 = as.data.frame(dummy(train80nn$title))
colnames(dummy_title80) = c("Master","Miss","Mr","Mrs","Officer","Royal")
dummy_embarked80 = as.data.frame(dummy(train80nn$Embarked))
colnames(dummy_embarked80) = c("C","Q","S")
train80nn_dum = cbind(train80nn, dummy_title80, dummy_embarked80)

dummy_title20 = as.data.frame(dummy(train20nn$title))
colnames(dummy_title20) = c("Master","Miss","Mr","Mrs","Officer")
dummy_embarked20 = as.data.frame(dummy(train20nn$Embarked))
colnames(dummy_embarked20) = c("C","Q","S")
train20nn_dum = cbind(train20nn, dummy_title20, dummy_embarked20)
train20nn_dum <- train20nn_dum %>%
  mutate(Royal = 0)


## ----message=FALSE, echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(nnet)
set.seed(55)
nn_control = trainControl(method = "repeatedcv", number = 3, repeats = 1, verboseIter = FALSE)
nn_mod = train(Survived ~ Pclass + Age + SibSp + Parch+ Master + Miss + Mr + Mrs + Officer + Royal+ C+ Q+ S,
               data = train80nn_dum,
               method = "nnet",
               trControl = nn_control,
               preProcess = "range")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#model
nn_mod$modelInfo



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#neural net accuracy
nn_pred = predict(nn_mod, newdata = train20nn_dum)
confusionMatrix(nn_pred, train20nn_dum$Survived)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#predictions on test data
test_pred = predict(RF_mod, newdata=test_clean)
submission = data.frame(test_clean$PassengerId, test_pred)
colnames(submission) = c("PassengerId", "Survived")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#write CSV file
#write.csv(submission, file = "titanic_submission.csv", row.names = FALSE)

