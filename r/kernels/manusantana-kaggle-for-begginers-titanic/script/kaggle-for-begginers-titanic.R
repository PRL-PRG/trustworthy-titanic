## ----dependencies, message = FALSE, warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library('e1071')
library('ggplot2') 
library('ggthemes') 
library('scales') 
library('dplyr') 
library('mice') 
library('randomForest') 
library('corrplot')
library ('glmnet')
library('tree')
library('caret')
library('car')
library('rpart')
library('rpart.plot')



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Load raw data in the orginal form (setting stringsAsFactors = F)
train <-read.csv('../input/train.csv', stringsAsFactors = F)
test <-read.csv('../input/test.csv', stringsAsFactors = F)
test$Survived <- NA

# Combine both test and train
full_titanic <-rbind(train, test)

# Check data
str(full_titanic)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Missing observation 
colSums(is.na(full_titanic))

# Empty data 
colSums(full_titanic=='')


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full_titanic[1:891,],aes(x = Survived,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("How many people survived on the Titanic?")+
  xlab("Survived") +
  labs(fill = "Survived") 



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full_titanic[1:891,],aes(x = Pclass,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Pclass X Survival rate")+
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full_titanic[1:891,],aes(x = Sex,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Sex X Survival rate")+
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived") 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full_titanic[1:891,], aes(x = Pclass, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~Sex) + 
  ggtitle("Sex X Pclass X Survival rate") +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Title
head(full_titanic$Name)
# Grabing title from passenger names
Names <- full_titanic$Name
Title <-  gsub("^.*, (.*?)\\..*$", "\\1", Names)

full_titanic$Title <- Title

# Show title counts by sex
table(full_titanic$Sex,full_titanic$Title)


# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full_titanic$Title[full_titanic$Title == 'Mlle']        <- 'Miss' 
full_titanic$Title[full_titanic$Title == 'Ms']          <- 'Miss'
full_titanic$Title[full_titanic$Title == 'Mme']         <- 'Mrs' 
full_titanic$Title[full_titanic$Title %in% rare_title]  <- 'Rare Title'
# Show title counts by sex again
table(full_titanic$Sex, full_titanic$Title)

# Title X Survival rate

ggplot(full_titanic[1:891,],aes(x = Title,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Title X Survival rate")+
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived") 


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full_titanic[1:891,], aes(x = Title, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Title X Pclass X Survival rate") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create a family size variable including the passenger themselves
full_titanic$FamilySize <-full_titanic$SibSp + full_titanic$Parch + 1
# Histogram
ggplot(full_titanic, aes(x=FamilySize)) + 
  geom_histogram(binwidth = 1, fill='black') + theme_grey() + 
  scale_x_continuous(breaks= seq(0, 150, by=1))

full_titanic$FamilySized[full_titanic$FamilySize == 1]   <- 'Single'
full_titanic$FamilySized[full_titanic$FamilySize < 5 & full_titanic$FamilySize >= 2]   <- 'Small (2-4)'
full_titanic$FamilySized[full_titanic$FamilySize >= 5]   <- 'Big (>=5)'

ggplot(full_titanic[1:891,],aes(x = FamilySized,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Family Size X Survival rate") +
  xlab("FamilySize") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full_titanic[1:891,], aes(x = FamilySized, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Family Size X Pclass X Survival rate") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

ggplot(full_titanic[1:891,], aes(x = FamilySized, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~Sex) + 
  ggtitle("Family Size X Sex X Survival rate") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Engineer features based on all the passengers with the same ticket
ticket.unique <- rep(0, nrow(full_titanic))
tickets <- unique(full_titanic$Ticket)
for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(full_titanic$Ticket == current.ticket)
  for (k in 1:length(party.indexes)) {
    ticket.unique[party.indexes[k]] <- length(party.indexes)
  }
}
full_titanic$ticket.unique <- ticket.unique
full_titanic$TicketSize[full_titanic$ticket.unique == 1]   <- 'Single'
full_titanic$TicketSize[full_titanic$ticket.unique < 5 & full_titanic$ticket.unique>= 2]   <- 'Small (2-4)'
full_titanic$TicketSize[full_titanic$ticket.unique >= 5]   <- 'Big (>=5)'

# Lets check the Ticket size through graph
ggplot(full_titanic[1:891,],aes(x = TicketSize,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("TicketSize X Survival rate")+
  xlab("TicketSize") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_titanic$IsSolo[full_titanic$SibSp==0] <- 'Yes'
full_titanic$IsSolo[full_titanic$SibSp!=0] <- 'No'
full_titanic$IsSolo <- as.factor(full_titanic$IsSolo)

ggplot(full_titanic[1:891,],aes(x = IsSolo,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("IsSolo X Survival Rate")+
  xlab("IsSolo ") +
  ylab("Total Count") +
  labs(fill = "Survived") 

ggplot(full_titanic[1:891,], aes(x = Sex, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~IsSolo) + 
  ggtitle("IsSolo X Sex X Survival rate") +
  xlab("IsSolo") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
colSums(full_titanic=='')
# As seen, Embarked has just 2 missing value

# Lets replace Embarked by most frequest observation 
table(full_titanic$Embarked)
full_titanic$Embarked[full_titanic$Embarked==""]="S"
table(full_titanic$Embarked)

## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Embarked
ggplot(full_titanic[1:891,],aes(x = Embarked,fill=factor(Survived))) +
  geom_bar() +
  ggtitle("Embarked vs Survival") +
  xlab("Embarked") +
  ylab("Total Count") +
  labs(fill = "Survived") 

# Pclass X Embarked X Survival rate
ggplot(full_titanic[1:891,], aes(x = Embarked, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass X Embarked X Survival rate") +
  xlab("Embarked") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full_titanic[1:891,], aes(x = Embarked, fill = factor(Survived))) +
  geom_bar() +
  facet_wrap(~Sex) +
  ggtitle("Sex X Embarked X Survival rate") +
  xlab("Embarked") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# As seen, missing observation in age
colSums(is.na(full_titanic))

#Using mice to predict missing ages
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title','Surname', 'FamilySized', 'TicketSize' , 'IsSolo')
# Set a random seed
set.seed(123)
# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full_titanic[, !names(full_titanic) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf')
# Save the complete output 
mice_output <- complete(mice_mod)

# Letâs compare the results we get with the original distribution of passenger ages
# Plot age distributions
par(mfrow=c(1,2))
hist(full_titanic$Age, freq=F, main='Age: Original Data', 
     col='darkgray', ylim=c(0,0.04)) 
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgray', ylim=c(0,0.04)) 

## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# It looks good, so letâs replace our age vector in the original data with the output from the mice model.
# Replace Age variable from the mice model
full_titanic$Age <- mice_output$Age
# Show new number of missing Age values
sum(is.na(full_titanic$Age))
# Boxplox Age and Title
ggplot(full_titanic[!is.na(full_titanic$Age),], aes(x = Title, y = Age, fill=Pclass )) + geom_boxplot() + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_grey()

## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Density: Survival density x Age
ggplot(full_titanic[(!is.na(full_titanic$Survived) & !is.na(full_titanic$Age)),], aes(x = Age, fill = Survived)) +
  geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density x Age") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_grey()


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(full_titanic[1:891,], aes(x = Age, fill = factor(Survived))) +
  geom_histogram() +
  facet_wrap(~Sex) +
  ggtitle("Sex X Age X Survival rate") +
  xlab("Age") +
  ylab("Total Count") +
  labs(fill = "Survived")


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create the column child, and indicate whether child or adult
full_titanic$Child[full_titanic$Age < 18] <- 'Child (<18)'
full_titanic$Child[full_titanic$Age >= 18] <- 'Adult (>=18)'
# Show counts
table(full_titanic$Child, full_titanic$Survived)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Adding Mother variable
full_titanic$Mother <- 'Not Mother'
full_titanic$Mother[full_titanic$Sex == 'female' & full_titanic$Parch > 0 & full_titanic$Age > 18 & full_titanic$Title != 'Miss'] <- 'Mother'
# Show counts
table(full_titanic$Mother, full_titanic$Survived)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Finish by factorizing our two new factor variables
full_titanic$Child  <- factor(full_titanic$Child)
full_titanic$Mother <- factor(full_titanic$Mother)
str(full_titanic)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
full_titanic$Survived<- as.factor(full_titanic$Survived)
full_titanic$Sex<- as.factor(full_titanic$Sex)
full_titanic$Embarked<- as.factor(full_titanic$Embarked)
full_titanic$Title <- as.factor(full_titanic$Title)
full_titanic$FamilySized <- as.factor(full_titanic$FamilySized)
full_titanic$TicketSize <- as.factor(full_titanic$TicketSize)
full_titanic$IsSolo<- as.factor(full_titanic$IsSolo)
full_titanic$Child<- as.factor(full_titanic$Child)
full_titanic$Mother<- as.factor(full_titanic$Mother)
full_titanic$Pclass<- as.factor(full_titanic$Pclass)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Divide data into train and set for internal validation
# Lets prepare and keep data in the proper format

train<-full_titanic[1:891, c('Survived','Sex','Embarked','Title','FamilySized','IsSolo', 'Child', 'Mother', 'Pclass')]
#str(train)

# For Cross validation purpose will keep 20% of data aside from my orginal train set
# This is just to check how well my data works for unseen data
set.seed(500)
ind<-createDataPartition(train$Survived,times=1,p=0.8,list=FALSE)

train_80=train[ind,]
test_20=train[-ind,]

# Check the proprtion of Survival rate in orginal training data, current traing and testing data
round(prop.table(table(train$Survived)*100),digits = 1)
round(prop.table(table(train_80$Survived)*100),digits = 1)
round(prop.table(table(test_20$Survived)*100),digits = 1)
str(train)
#str(train_80)
#str(test_20)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Model type: Logistic binomial
x<-model.matrix(Survived~.,train_80)[,-1]
y<- train_80$Survived 

str(x)

#cv.glmnet - cross validation
model_lasso<- cv.glmnet(x, y, 
                      family = "binomial", 
                      alpha = 1,
                      type.measure = "class")

# Lambda.min
model_lasso$lambda.min
lambda.calc<-model_lasso$lambda.min

# Selected variables
coef(model_lasso, s = "lambda.min")



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(x)
flag_lasso<-predict(model_lasso,s=lambda.calc,newx = x, type="response")
str(flag_lasso)

train_80$flag_lasso = flag_lasso
#head(train_80)

pred.lasso<-ifelse(flag_lasso>0.5,"1","0")

train_80$pred.lasso = pred.lasso
#head(train_80)

# Confusion Matrix
mat_conf<-table(pred.lasso,train_80$Survived)
mat_conf
# Accuracy
(mat_conf[1,1]+mat_conf[2,2])/sum(mat_conf)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Validating with test data 
x_20<-model.matrix(Survived~.,test_20)[,-1]
str(x_20)
flag_lasso_20<-predict(model_lasso,s=lambda.calc,newx = x_20, type="response")
pred.lasso_20<-ifelse(flag_lasso_20>0.5,"1","0")
#str(pred.lasso_20)

# Confusion Matrix
mat_conf_20<-table(pred.lasso_20,test_20$Survived)
mat_conf_20
# Accuracy
(mat_conf_20[1,1]+mat_conf_20[2,2])/sum(mat_conf_20)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Sexmale              
#EmbarkedS             
#TitleMiss              
#TitleMr                
#TitleMrs               
#TitleRare Title        
#FamilySizedSingle       
#FamilySizedSmall (2-4)  
#ChildChild (<18)        
#MotherNot Mother        
#Pclass2                
#Pclass3                


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

train$Sexmale[train$Sex=="male"] <- 1
train$Sexmale[train$Sex!="male"] <- 0

train$EmbarkedS[train$Embarked=="S"] <- 1
train$EmbarkedS[train$Embarked=="C"] <- 0
train$EmbarkedS[train$Embarked=="Q"] <- 0

train$TitleMr[train$Title=="Mr"] <- 1
train$TitleMr[train$Title!="Mr"] <- 0

train$TitleMrs[train$Title=="Mrs"] <- 1
train$TitleMrs[train$Title!="Mrs"] <- 0

train$TitleMiss[train$Title=="Miss"] <- 1
train$TitleMiss[train$Title!="Miss"] <- 0

train$TitleRare[train$Title=="Rare Title"] <- 1
train$TitleRare[train$Title!="Rare Title"] <- 0

train$FamilySizedSmall[train$FamilySized=="Small (2-4)"] <- 1
train$FamilySizedSmall[train$FamilySized!="Small (2-4)"] <- 0

train$FamilySizedSingle[train$FamilySized=="Single"] <- 1
train$FamilySizedSingle[train$FamilySized!="Single"] <- 0

train$Child_[train$Child=="Child (<18)"] <- 1
train$Child_[train$Child!="Child (<18)"] <- 0

train$Not_Mother[train$Mother=="Not Mother"] <- 1
train$Not_Mother[train$Mother!="Not Mother"] <- 0

train$Pclass2[train$Pclass=="2"] <- 1
train$Pclass2[train$Pclass!="2"] <- 0

train$Pclass3[train$Pclass=="3"] <- 1
train$Pclass3[train$Pclass!="3"] <- 0

#str(train)
#summary(train)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(500)
ind<-createDataPartition(train$Survived,times=1,p=0.8,list=FALSE)

#str(train)

train_80=train[ind,]
test_20=train[-ind,]

train_80<-train_80[1:714,  c('Survived','Sexmale','EmbarkedS', 'TitleMiss', 'TitleMrs','TitleMr','TitleRare','FamilySizedSingle','FamilySizedSmall','Pclass2','Pclass3','Not_Mother','Child_' )]
                
test_20<-test_20[1:177, c('Survived','Sexmale','EmbarkedS', 'TitleMiss', 'TitleMrs','TitleMr','TitleRare','FamilySizedSingle','FamilySizedSmall','Pclass2','Pclass3','Not_Mother','Child_' )]

str(train_80)
str(test_20)
#summary(train_80)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1234)
model_tree=rpart(Survived~.,data=train_80,method="class")
#summary(model_tree)
rpart.plot(model_tree,extra = 3,fallen.leaves = T)

# Lets Predict train data and check the accuracy of single tree

pred_tree <- predict(model_tree, newdata = train_80, type = 'class')

confusionMatrix(pred_tree,train_80$Survived)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1234)

model_rf<- randomForest(x = train_80[,-1],y=train_80[,1], importance = TRUE, ntree = 1000)
model_rf

varImpPlot(model_rf)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Lets Predict the test data 

pred_rf2=predict(model_rf,newdata = test_20)

confusionMatrix(pred_rf2,test_20$Survived)

# Accuracy : 0.8192



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1274)
model_svm=tune.svm(Survived~.,data=train_80,kernel="linear",cost=c(0.01,0.1,0.2,0.5,0.7,1,2,3,5,10,15,20,50,100))

model_svm



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Lets get a best.linear model  
best_linear=model_svm$best.model
best_linear

# Predict Survival rate using test data
pred_test=predict(best_linear,newdata=test_20,type="class")
summary(pred_test)

confusionMatrix(pred_test,test_20$Survived)
# Accuracy :  0.8136 in data validation


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1274)

model_rd=tune.svm(Survived~.,data=train_80,kernel="radial",gamma=seq(0.1,5))
summary(model_rd)
best_rd=model_rd$best.model


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Lets Predict test data
pred_rd=predict(best_rd,newdata = test_20)

confusionMatrix(pred_rd,test_20$Survived)
# 0.8192 

# Accuracy of test data using Non Liner model is 0.81


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model_log<- glm(Survived ~ ., family = binomial(link=logit), data = train_80)
# Check the summary
summary(model_log)

# Predict train data
flag_log <- predict(model_log, data=train_80,type =  "response")

pred_log<-ifelse(flag_log>0.5,"1","0")

# Confusion matrix
mat_conf<-table(pred_log,train_80$Survived)
mat_conf
#Accuracy
(mat_conf[1,1]+mat_conf[2,2])/sum(mat_conf)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Lets Predict test data
flag_log_test <- predict(model_log, newdata=test_20,type =  "response")
pred_log_test<-ifelse(flag_log_test>0.5,"1","0")
# Confusion matrix 
mat_conf<-table(pred_log_test,test_20$Survived)

# Accuracy
(mat_conf[1,1]+mat_conf[2,2])/sum(mat_conf)

# Accuracy in test data: 0.8135593

