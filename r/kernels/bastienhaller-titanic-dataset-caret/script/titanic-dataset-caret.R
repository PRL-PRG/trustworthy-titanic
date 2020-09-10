## ---- message=FALSE,warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(ISLR)
library(tree)
library(MASS)
library(randomForest)
library(leaps)
library(glmnet)
library(ranger)
library(xgboost)
library(caret)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train<-read.csv("../input/train.csv",stringsAsFactors = F)
test<-read.csv("../input/test.csv",stringsAsFactors=F)
test$Survived <- NA
train$dset<-"train"
test$dset<-"test"
combined<-bind_rows(train,test)
str(train)
summary(train)
str(test)
summary(test)


combined<-combined%>%
  mutate(familyname=substr(Name,1,str_locate(Name,",")-1),
         title=substr(Name,str_locate(Name,",")+2,str_locate(Name,fixed("."))-1)
  )



combined%>%filter(dset=="train")%>%
  ggplot(aes(x=as.factor(Survived),fill=Sex))+geom_bar()+ggtitle("Survival by Gender")+xlab("Survived")+
   geom_text(stat="count",aes(label=..count..),position = "stack",vjust=1)

combined%>%filter(dset=="train")%>%
  dplyr::count(Survived,Sex)%>%
  mutate(Share=n/sum(n))%>%
  arrange(Survived,-Share)


combined%>%filter(dset=="train")%>%
  ggplot(aes(x=as.factor(Survived),fill=as.factor(title)))+geom_bar()+facet_wrap(~Sex)+
  ggtitle("Survived by Title")+xlab("Survived")+labs(fill="Title")+ylab("# of pax")



combined%>%filter(dset=="train")%>%
  dplyr::count(title,Sex,Survived)%>%
  ggplot(aes(x=as.factor(Survived),y=n,col=as.factor(title)))+geom_jitter()+facet_wrap(~Sex)+
  ggtitle("Survived by Title")+xlab("Survived")+labs(fill="Title")+ylab("# of pax")+
  guides(col=guide_legend(title="Title"))


combined%>%
  filter(dset=="train")%>%
  group_by(Sex,title)%>%
summarise(SurvivalRate=mean(Survived),count=n())%>%
  ggplot(aes(y=SurvivalRate,x=title,size=count,col=as.factor(Sex)))+geom_point()+
  ggtitle("Survival Rate by Title")+labs(col="sex",size="# of pax")

##cluster titles

combined<-combined%>%mutate(title=ifelse(title %in% c("Mlle","Ms","Dona"),"Miss",title))
combined<-combined%>%mutate(title=ifelse(title %in% c("Mme"),"Mrs",title))
combined<-combined%>%mutate(title=ifelse(title %in% c("Jonkheer","Rev","Don","Capt"),"Mr",title))
combined<-combined%>%mutate(title=ifelse(title 
  %in% c("Dr","Lady","the Countess","Col","Dr","Major","Sir"),"raretitle",title))


combined%>%filter(dset=="train")%>%
  ggplot(aes(x=as.factor(title),fill=as.factor(Survived)))+geom_bar()+facet_wrap(~Sex)+
  ggtitle("Survived by Title")+xlab("Survived")+labs(fill="Survived")+ylab("# of pax")


combined%>%
  filter(dset=="train")%>%
  group_by(Sex,title)%>%
summarise(SurvivalRate=mean(Survived),count=n())%>%
  ggplot(aes(y=SurvivalRate,x=title,size=count,col=as.factor(Sex)))+geom_point()+
  ggtitle("Survival Rate by Title")+labs(col="sex",size="# of pax")


groupsize=combined%>%
  group_by(Ticket)%>%
  summarise(Groupsize=n())
  


combined<-combined%>%left_join(groupsize,by=c("Ticket"="Ticket"))

combined<-combined%>%mutate(familysize=SibSp+Parch+1,
                            familyID=paste(familyname,as.character(familysize)))





Traveltype<-combined%>%
  distinct(Ticket,familyname)%>%
  count(Ticket,familyname)%>%
  group_by(Ticket)%>%summarise(names=sum(n))%>%
  mutate(traveltype=ifelse(names>1,"group","family"))%>%
  dplyr::select(Ticket,traveltype)

combined<-combined%>%left_join(Traveltype,by=c("Ticket"="Ticket"))%>%
  mutate(traveltype=ifelse( Groupsize==1,"single",traveltype))

rm(groupsize,Traveltype)




combined%>%filter(dset=="train")%>%ggplot(aes(x=as.factor(Pclass),fill=as.factor(Survived)))+geom_bar()+facet_wrap(~Sex)+
  ggtitle("Survival by class")+
  labs(x="Pclass",fill="Survived")


combined%>%filter(dset=="train")%>%
  group_by(Sex,Pclass)%>%
  summarise(SurvivalRate=mean(Survived))


combined%>%filter(dset=="train")%>%ggplot(aes(x=Age,y=Pclass,color=as.factor(Survived)))+
  geom_jitter()+
  scale_color_manual(values=c("red", "blue"))+
  facet_grid(Sex~.)+ggtitle("Survival by Class and Age")+labs(col="Survived")

combined%>%filter(dset=="train")%>%ggplot(aes(x=Fare,y=Pclass,color=as.factor(Survived)))+
  geom_jitter()+
  scale_color_manual(values=c("red", "blue"))+
  facet_grid(Sex~.)+ggtitle("Survival by Class and Fare")+labs(col="Survived")



combined%>%filter(dset=="train")%>%
  ggplot(aes(x=Groupsize,fill=factor(Survived)))+
  geom_bar(stat="count",position = "dodge")+
  scale_x_continuous(breaks=c(1:11))+ggtitle("Survival by Group Size")+labs(fill="Survived")


combined%>%filter(dset=="train")%>%
  group_by(Groupsize)%>%
  summarise(SurvivalRate=mean(Survived))%>%
  arrange(desc(SurvivalRate))

combined%>%filter(dset=="train")%>%
  ggplot(aes(x=familysize,fill=factor(Survived)))+
  geom_bar(stat="count",position = "dodge")+
  scale_x_continuous(breaks=c(1:11))+ggtitle("Survival by Family Size")+labs(fill="Survived")

combined%>%filter(dset=="train")%>%
  group_by(familysize)%>%
  summarise(SurvivalRate=mean(Survived))%>%
  arrange(desc(SurvivalRate))

combined<-combined%>%mutate(familysizeD=ifelse(familysize==1,"singleton",
                                               ifelse(familysize>1 &familysize<5,"small","large"))
                              )


combined%>%filter(dset=="train")%>%
  ggplot(aes(x=familysizeD,fill=factor(Survived)))+
  geom_bar(stat="count",position = "dodge")+
 ggtitle("Survival by Family Size")+labs(fill="Survived")

combined%>%filter(dset=="train")%>%
  group_by(familysizeD)%>%
  summarise(SurvivalRate=mean(Survived))%>%
  arrange(desc(SurvivalRate))


combined%>%filter(dset=="train")%>%
  ggplot(aes(x=traveltype,fill=factor(Survived)))+
  geom_bar(stat="count",position = "dodge")+
  ggtitle("Survival by Travel type")+labs(fill="Survived")

combined%>%filter(dset=="train")%>%
  group_by(traveltype)%>%
  summarise(SurvivalRate=mean(Survived))




combined$Deck<-substr(combined$Cabin,1,1)




combined%>%filter(dset=="train")%>%
  ggplot(aes(x=Deck,fill=factor(Survived)))+
  geom_bar(stat="count",position="dodge")+
  ggtitle("Survival by Deck")+labs(fill="Survived")


combined%>%filter(dset=="train")%>%
  group_by(Deck)%>%
  summarise(SurvivalRate=mean(Survived))%>%
  arrange(desc(SurvivalRate))



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combined$Fare[is.na(combined$Fare)]=median(combined$Fare,na.rm=T)

combined$Embarked[combined$Embarked == ""]="S"




factor_vars<-c("PassengerId","Pclass","Sex","Ticket","Cabin","Embarked","title","traveltype","Deck")


combined[factor_vars]<-lapply(combined[factor_vars],function(x) as.factor(x))


dmy<-dummyVars(Age~Pclass+Sex+SibSp+Parch+Fare+Embarked+title+familysize,data=combined,fullRank = TRUE)
age_transformed<-data.frame(predict(dmy, newdata = combined))


 trControl = trainControl(method = "cv", number = 10, verboseIter = FALSE)
 

 capture.output(model<-train(x=age_transformed[!is.na(combined$Age),],
                  y=combined$Age[!is.na(combined$Age)],
                            method = "ranger",
                          num.trees = 1000,
                           trControl=trControl
                            ))
 
 

 print(model)
 plot(model)
 
 combinedrf<-combined


combinedrf$Age[is.na(combinedrf$Age)]<-round(predict(model,age_transformed[is.na(combinedrf$Age),]) )

combined%>%ggplot(aes(Age))+geom_histogram()+ggtitle("Original Age Distribution")
combinedrf%>%ggplot(aes(Age))+geom_histogram()+ggtitle("Estimated Age Distribution")


combinedrf<-combinedrf%>%
    mutate(Child=ifelse(Age<18,"Child","Adult"),
           Mother=ifelse(Sex=="female" & Parch>0 & Age>18 &title!="Miss","Mother","NotMother" )
           )

combinedrf$Child<-as.factor(combinedrf$Child)
combinedrf$Mother<-as.factor(combinedrf$Mother)

combinedrf%>%filter(dset=="train")%>%
  ggplot(aes(x=Child,fill=factor(Survived)))+
  geom_bar(stat="count",position="stack")+facet_wrap(~Sex)+
  ggtitle("Survival Child")+labs(fill="Survived")


combinedrf%>%filter(dset=="train")%>%
  group_by(Child,Sex)%>%
  summarise(SurvivalRate=mean(Survived))%>%
  ungroup()%>%
  arrange(desc(SurvivalRate))

combinedrf%>%filter(dset=="train")%>%
  ggplot(aes(x=Mother,fill=factor(Survived)))+facet_wrap(~Sex)+
  geom_bar(stat="count",position="stack")+
  ggtitle("Survival Mother")+labs(fill="Survived")


combinedrf%>%filter(dset=="train")%>%
  group_by(Mother,Sex)%>%
  summarise(SurvivalRate=mean(Survived))%>%
  ungroup()%>%
  arrange(desc(SurvivalRate))
combinedrf$Survived<-as.factor(combinedrf$Survived)




## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


train=combinedrf%>%filter(dset=="train")
test=combinedrf%>%filter(dset=="test")%>%mutate(Survived=1)

trainX<-model.matrix(Survived~Sex+Age,
             data=train)[,-1]
trainY<-train$Survived
levels(trainY)=c("N","Y")
myFolds<-createFolds(trainY)

myControl <- trainControl(
 summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  savePredictions = TRUE,
  verboseIter = FALSE,
  index = myFolds
)


###Logistic Regression

 capture.output(model_logistic<-train(x=trainX,y=trainY,
                           method="glm",
                      trControl=myControl))

model_logistic


trainX<-model.matrix(Survived~Sex+Age+SibSp+Parch+Pclass+Fare+Deck+familysizeD+title,
             data=train)[,-1]


testX<-model.matrix(Survived~Sex+Age+SibSp+Parch+Pclass+Fare+Deck+familysizeD+title,
             data=test)[,-1]


garbage <-capture.output(model_logistic2<-train(x=trainX,y=trainY,
                           method="glm",
                      trControl=myControl))


p_prob<-predict(model_logistic2,testX,type="prob")
p_class_logistic2<-predict(model_logistic2,testX)

### GLMNET

trainX<-model.matrix(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Deck+familysizeD+title+Embarked+Child+Mother,
             data=train)[,-1]

testX<-model.matrix(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Deck+familysizeD+title+Embarked+Child+Mother,
             data=test)[,-1]

garbage <-capture.output(model_glmnet <- train(
  x = trainX, y = trainY,
  metric = "ROC",
  method = "glmnet",
  preProcess=c("center","scale"),
  trControl = myControl,
  tuneGrid = expand.grid(
alpha = 0:1,
lambda = seq(0.0001, 1, length = 250))
))

plot(model_glmnet)
max(model_glmnet[["results"]][["ROC"]])

coef(model_glmnet$finalModel, model_glmnet$bestTune$lambda)


p_prob<-predict(model_glmnet,testX,type="prob")
p_class_glm_net<-predict(model_glmnet,testX)


garbage <-capture.output(model_rf<-train(y=trainY,x=trainX,
                method="ranger",
                metric="ROC",
                preProcess=c("center","scale"),
                trControl=myControl,
                tuneLength=ncol(trainX ),
               # num.tree=1000,
                importance="impurity"
  
))

plot(model_rf)
plot(varImp(model_rf),main="Random Forest")

p_prob<-predict(model_rf,testX,type="prob")
p_class_rf<-predict(model_rf,testX)



grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),
                    shrinkage=c(0.01,0.05,0.1,0.5),
                    n.minobsinnode = c(3,5,10),
                    interaction.depth=c(1,5,10))

garbage <-capture.output(model_gbm <- train(
  x = trainX, y = trainY,
  metric = "ROC",
  method = "gbm",
  preProcess=c("center","scale"),
  tuneGrid = grid,
  trControl = myControl
))

plot(model_gbm)

p_prob<-predict(model_gbm,testX,type="prob")
p_class_gbm<-predict(model_gbm,testX)
plot(varImp(object=model_gbm),main="GBM - Variable Importance")


model_list <- list(logistic = model_logistic2, glmnet = model_glmnet,rf=model_rf,gbm=model_gbm)

resamples<-resamples(model_list)
summary(resamples)
bwplot(resamples,metric="ROC")


rf_submit<-data.frame(PassengerId=test$PassengerId,Survived=ifelse(p_class_rf=="Y",1,0))
gbm_submit<-data.frame(PassengerId=test$PassengerId,Survived=ifelse(p_class_gbm=="Y",1,0))
glm_net_submit<-data.frame(PassengerId=test$PassengerId,Survived=ifelse(p_class_glm_net=="Y",1,0))


