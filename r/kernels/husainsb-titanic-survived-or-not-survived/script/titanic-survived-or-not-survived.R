## ----setup, include=FALSE,echo=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,results = TRUE,warning=FALSE)


## ----setwd,echo=FALSE,results=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#setwd('D://R Files//titanic set')

## ----packages,results=FALSE,warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#install.packages("lattice")  #for plots
library(lattice)
#install.packages("data.table")  #for data table
library(data.table)
#install.packages("ggplot2") #for beautiful plots
library(ggplot2)
#install.packages("dplyr")  #for manipulations
library(dplyr)
#install.packages("caret")  
library(caret)              #for training and building different models
#install.packages("e1071")
library(e1071)              #for Naive Bayes and SVM
#install.packages("randomForest") 
library(randomForest)       #for Random Forest
#install.packages("fastAdaboost")  
library(fastAdaboost)       # for AdaBoost
#install.packages("ROCR")  
library(ROCR)               #For ROC curve


## ----load_train------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_t <- read.csv('../input/train.csv',header = T,quote = '"')
names(rms_t)


## ----survival_sex----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(rms_t$Survived)

survival_rate <- as.matrix(table(rms_t$Survived,rms_t$Sex))

survival_df <- as.data.frame(table(rms_t$Survived,rms_t$Sex))
names(survival_df)<-c('Survived','Sex','Freq')
barchart(survival_df$Freq ~ survival_df$Sex | survival_df$Survived)


## ----survival_class--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
survival_class_df <- as.data.frame(table(rms_t$Survived,rms_t$Pclass))
names(survival_class_df)<-c('Survived','PClass','Freq')
barchart(survival_class_df$Freq ~ survival_class_df$PClass | survival_class_df$Survived)

survival_class_dt <- as.data.table(survival_class_df)
survival_class_dt[,100*Freq/sum(Freq),PClass]


## ----extract_title---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_t$title<-gsub(' ','',gsub('(.*,)|(\\..*)','',rms_t$Name,fixed=F,perl = T))
t(table(rms_t$title,rms_t$Sex))


## ----convert_title,warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_t %>% group_by(title) %>%summarise(mean(Age,na.rm = T))

rms_t <- within(rms_t,title[title %in% c('Capt','Col','Don','Dr','Major','Sir','Rev','Jonkheer') & Sex=='male'] <- 'Mr')
rms_t <- within(rms_t,title[Age<30 & Sex=='female'] <- 'Miss')
rms_t <- within(rms_t,title[Age>=30 & Sex=='female'] <- 'Mrs')

table(rms_t$title)

ggplot(data = rms_t,aes(title,Age,fill=title))+geom_violin()


## ----mean_age--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(subset(rms_t,title=='Master',Age))  #avg 5
summary(subset(rms_t,title=='Mr',Age)) # avg 33
summary(subset(rms_t,title=='Miss',Age)) #avg 18
summary(subset(rms_t,title=='Mrs',Age)) #avg 40

rms_t <- within(rms_t,Age[title=='Mrs' & is.na(Age)] <- 40)
rms_t <- within(rms_t,Age[title=='Miss' & is.na(Age)] <- 18)
rms_t <- within(rms_t,Age[title=='Mr' & is.na(Age)] <- 33)
rms_t <- within(rms_t,Age[title=='Master' & is.na(Age)] <- 5)

table(rms_t$title,rms_t$Survived)

survival_title_df <- as.data.frame(table(rms_t$Survived,rms_t$title))

barchart(survival_title_df$Freq ~ survival_title_df$Var2 | survival_title_df$Var1)

survival_title_dt <- as.data.table(survival_title_df)
names(survival_title_dt)<-c("Survived","title","Freq")
survival_title_dt[,.(Survived,100*Freq/sum(Freq)),title]


## ----survival_embark-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(rms_t$Embarked)
str(rms_t$Embarked)
table(rms_t$Survived,rms_t$Embarked)


## ----view_2missing_embark--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
subset(rms_t,Embarked=="",select = c("Pclass","Fare"))


## ----plot_embarked---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(rms_t,aes(Embarked,Fare))+facet_grid(.~Pclass) + geom_boxplot() + geom_hline(aes(yintercept=80,col="red"))


## ----impute_embarked-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_t<-within(rms_t,Embarked[Embarked==""]<-'C')

ggplot(rms_t,aes(Embarked,fill=factor(Survived)))+ geom_histogram(stat='count')+  facet_grid(.~Sex)


## ----fmly_size-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_t$Surname <- gsub(',.*','',rms_t$Name,fixed=F,perl = T)

rms_fmly <- as.data.frame(list('surname'=rms_t$Surname,'fsize'=rms_t$SibSp + rms_t$Parch + 1,'survived'=rms_t$Survived))


## ----fmly_size_sruvival----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_fmly<-transform(rms_fmly,Dead=0L,Living=0L)

rms_fmly <- within(rms_fmly,{Dead[survived==0]<-1L
                            Living[survived==1]<-1L})

rms_fmly_sum <- rms_fmly %>% group_by(surname,fsize) %>% summarise(Dead=sum(Dead),Living=sum(Living))

rms_fmly_melt<-melt(rms_fmly_sum[,c('surname','fsize','Dead','Living')],c('surname','fsize'),c('Dead','Living'))

rms_fmly_melt <- rms_fmly_melt %>% group_by(fsize,variable) %>% summarise(avg=mean(value))

ggplot(rms_fmly_melt,aes(fsize,y=avg,fill=variable)) + geom_bar(position="dodge",stat="identity")+scale_x_continuous(breaks=c(1:11)) 


## ----fsize_category--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_fmly$size.desc[rms_fmly$fsize==1] <- 'single' 
rms_fmly$size.desc[rms_fmly$fsize>1 & rms_fmly$fsize<5] <- 'small' 
rms_fmly$size.desc[rms_fmly$fsize>=5] <- 'large' 
mosaicplot( table(rms_fmly$size.desc,rms_fmly$survived),color = T)


## ----merge_fsize-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_full<-merge(rms_t,rms_fmly,by.x ="Surname",by.y = "surname" )
rms_t<-subset(rms_full,select=-c(fsize:Living))

rms_t <- rms_t %>% group_by(PassengerId) %>% summarise_all(.funs = function(x) {min(as.character(x))})


## ----chk_fares-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_t$Fare <- as.double(rms_t$Fare)
summary(rms_t$Fare)
count(subset(rms_t,Fare==0))


## ----chk_fares_by_class_embark---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(rms_t,aes(x=Embarked,y=Fare,fill=factor(Pclass)))+geom_boxplot()


## ----zero_fare_impute------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_t$Fare[rms_t$Embarked=='S' & rms_t$Pclass==1 & rms_t$Fare==0] <- median(rms_t$Fare[rms_t$Embarked=='S' & rms_t$Pclass==1])
rms_t$Fare[rms_t$Embarked=='S' & rms_t$Pclass==2 & rms_t$Fare==0] <- median(rms_t$Fare[rms_t$Embarked=='S' & rms_t$Pclass==2])
rms_t$Fare[rms_t$Embarked=='S' & rms_t$Pclass==3 & rms_t$Fare==0] <- median(rms_t$Fare[rms_t$Embarked=='S' & rms_t$Pclass==3])


## ----chk_NA----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sapply(names(rms_t),FUN = function(x) {sum(is.na(x))})


## ----final_groom-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rms_t<- rms_t %>% mutate_if(is.character,as.factor)
rms_t$Parch <- as.integer(rms_t$Parch)
rms_t$SibSp <- as.integer(rms_t$SibSp)
rms_t$Age <- as.double(rms_t$Age)
t(lapply(rms_t,class))


## ----partition-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
trainnew<-createDataPartition(y=rms_t$Survived,p = .8,list = F)
rms_train <- rms_t[trainnew,] #80% training set
rms_test <- rms_t[-trainnew,] #20% test set

dim(rms_train); dim(rms_test)


## ----logit_model,warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
rms_fit <- glm(formula =Survived~Sex+Pclass+Age+size.desc+title,data=rms_train,family = "binomial")
summary(rms_fit)
p<-predict(rms_fit,rms_train,type = "response")


## ----roc-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred<-prediction(p,rms_train$Survived)
perf<-performance(pred,'tpr','fpr')
plot(perf,col="blue",print.cutoffs.at=c(.4,.5,.7))


## ----precitions_logit------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
c<-confusionMatrix(table(ifelse(p>0.4,1,0),rms_train$Survived))
c
acc<-round(c$overall[[1]]*100,2)


## ----CV_10fold-rep3,warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
model.10x3fold<-train(Survived~Sex+Pclass+Age+size.desc+title,data=rms_train,
                      method = "glm",
                      trControl = trainControl(method = "repeatedcv",number = 10,repeats = 3,verboseIter = F))
model.10x3fold

acc<-round(model.10x3fold$results[[2]]*100,2)


## ----rf--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
model.rf <- randomForest(y=rms_train$Survived,x=rms_train[,c("Sex","Pclass","Age","size.desc","Embarked")],importance=T,keep.forest = T)
plot(model.rf)
legend("topright",colnames(model.rf$err.rate),col=1:3,fill=1:3)
varImpPlot(model.rf,scale = F)
pred_cart<-predict(model.rf,rms_train)
c<-confusionMatrix(table(pred_cart,rms_train$Survived))
c
acc<-round(c$overall[[1]]*100,2)


## ----svm,warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
model.svm<-train(Survived~Sex+Pclass+Age+size.desc+title,data=rms_train,
                 method="svmRadial",
                 trControl = trainControl (method="cv" ,number=10, verboseIter = F))

p<-predict(model.svm,rms_train)
c<-confusionMatrix(table(p,rms_train$Survived))
c
acc<-round(c$overall[[1]]*100,2)


## ----adaboost,warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(123)
model.ada<-train(as.factor(Survived)~Sex+Pclass+Age+size.desc+title,data=rms_train,method="adaboost", trControl = trainControl(method = "cv",number = 3,verboseIter = F))
p<-predict(model.ada,rms_train)
c<-confusionMatrix(table(p,rms_train$Survived))
c
acc<-round(c$overall[[1]]*100,2)


## ----pred_rms_test,echo=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

p<-predict(rms_fit,rms_test,type='response')
c<-confusionMatrix(table(ifelse(p>0.4,1,0),rms_test$Survived))
acc<-round(c$overall[[1]]*100,2)
print(c('Accuracy(%) from Logistic Model:',acc),quote = F)

p<-predict(model.rf,rms_test)
c<-confusionMatrix(table(p,rms_test$Survived))
acc<-round(c$overall[[1]]*100,2)
print(c('Accuracy(%) from Random Forest:',acc),quote = F)

p<-predict(model.svm,rms_test)
c<-confusionMatrix(table(p,rms_test$Survived))
acc<-round(c$overall[[1]]*100,2)
print(c('Accuracy(%) from SVM-RBF:',acc),quote = F)

p<-predict(model.ada,rms_test)
c<-confusionMatrix(table(p,rms_test$Survived))
acc<-round(c$overall[[1]]*100,2)
print(c('Accuracy(%) from AdaBoost:',acc),quote = F)


## ----test_set--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_set <- read.csv('../input/test.csv',header = T,quote = '"')
names(rms_test)


## ----title_var-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_set$title<-gsub(' ','',gsub('(.*,)|(\\..*)','',test_set$Name,fixed=F,perl = T))

test_set <- within(test_set,title[title %in% c('Capt','Col','Don','Dr','Major','Sir','Rev','Jonkheer') & Sex=='male'] <- 'Mr')
test_set <- within(test_set,title[Age<30 & Sex=='female'] <- 'Miss')
test_set <- within(test_set,title[Age>=30 & Sex=='female'] <- 'Mrs')

t(table(test_set$title,test_set$Sex))


## ----ms_miss---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_set[test_set$title=='Ms',]
test_set <- within(test_set,title[title=='Ms' & Sex=='female'] <- 'Miss')

t(table(test_set$title,test_set$Sex))


## ----na_age----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(test_set$Age))


## ----impute_age------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_set <- within(test_set,Age[title=='Mrs' & is.na(Age)] <- 40)
test_set <- within(test_set,Age[title=='Miss' & is.na(Age)] <- 18)
test_set <- within(test_set,Age[title=='Mr' & is.na(Age)] <- 33)
test_set <- within(test_set,Age[title=='Master' & is.na(Age)] <- 5)

ggplot(data = test_set,aes(title,Age,fill=title))+geom_violin()


## ----size.desc-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_set$Surname <- gsub(',.*','',test_set$Name,fixed=F,perl = T) #Extract Surname of each passenger

test_fmly <- as.data.frame(list('surname'=test_set$Surname,'fsize'=test_set$SibSp + test_set$Parch + 1))  #calculate Family size


rms_fmly_subset<-subset(rms_fmly,select = 1:2)
all_fmly <-rbind(test_fmly,rms_fmly_subset) #Merge family size of training and test set

all_fmly$size.desc[all_fmly$fsize==1] <- 'single' 
all_fmly$size.desc[all_fmly$fsize>1 & all_fmly$fsize<5] <- 'small' 
all_fmly$size.desc[all_fmly$fsize>=5] <- 'large' 

#Merge family size on original test set using Surname variable
test_full<-merge(test_set,all_fmly,by.x ="Surname",by.y = "surname" )
test_set<-subset(test_full,select=-c(fsize))

test_set<- test_set %>% group_by(PassengerId) %>% summarise_all(.funs = function(x) {min(as.character(x))})

names(rms_test)


## ----cast_datatypes--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_set<- test_set %>% mutate_if(is.character,as.factor)
test_set$Parch <- as.integer(test_set$Parch)
test_set$SibSp <- as.integer(test_set$SibSp)
test_set$Age <- as.double(test_set$Age)
t(lapply(test_set,class))


## ----pred_test-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_set$Survived<-predict(model.svm,test_set)
#write.csv(test_set[,c("PassengerId","Survived")],"test_file.csv",quote = F,row.names = F)

