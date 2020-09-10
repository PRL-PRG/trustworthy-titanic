
 options(warn= -1)
  nomsg<-suppressMessages
   nomsg(library(tidyverse))
   nomsg(library(caret))
    nomsg(library(mice))
      nomsg(library(RANN))
train<-read.csv("../input/train.csv")
train<-as.tibble(train)  

str(train)

#Remove cabin,name,Ticket for now as these may not predict survival 
train<-train %>% 
  mutate(PassengerId=as.factor(PassengerId),Pclass=as.factor(Pclass),
         Survived=as.factor(Survived),AgeGroup=as.factor(findInterval(Age,c(0,18,35,100)))) %>% 
 select(-PassengerId,-Name,-Ticket,-Cabin,-Embarked)

#View and deal with nas
anyNA(train$AgeGroup) 

nomsg(require(Amelia))
missmap(train,col=c("snow","indianred4"),main="Who's missing aboard the Titanic?!")

#Change levels
levels(train$AgeGroup)<-c("Young","Mid Age","Aged")
levels(train$Sex)<-c("F","M")
#Impute median
train_1<-mice(train,m=3,method="cart",printFlag = F)
train_imp<-complete(train_1)

#checkNAs
anyNA(train_imp) 

#redo levels
train_imp<-train_imp %>% 
  mutate(AgeGroup=as.factor(findInterval(Age,c(0,18,35,100))))
levels(train_imp$AgeGroup)<-c("Young","Mid Age","Aged")

train_imp %>% 
group_by(Survived) %>% 
summarise(n=n()) %>% 
mutate(prop=n/sum(n)) %>% 
ggplot(aes(Survived,prop,fill=Survived))+geom_col()+
geom_text(aes(label=round(prop*100,1)))+
scale_fill_manual(values=c("whitesmoke","indianred2"))+
ggpubr::theme_pubr()+
ggtitle("Proportion(s) of Survival and Deaths")+
labs(caption=("There were more chances of survival"))+
theme(plot.caption=element_text(colour="steelblue",size=13),
     plot.title=element_text(colour="steelblue",size=13,hjust=0.5))

Fares<-train_imp %>% 
group_by(Fare) %>% 
summarise(n=n()) %>% 
mutate(FareGroup=as.factor(findInterval(Fare,c(0,100,200,500)))) 
levels(Fares$FareGroup)<-c("Cheap","Medium","Expensive","VIP")
 Fares %>% 
 group_by(FareGroup) %>% 
summarise(n=n()) %>% 
ggplot(aes(FareGroup,n,fill=FareGroup))+geom_col()+
ggthemes::theme_hc()+
geom_label(aes(label=n))+
labs(caption="Majority had cheap tickets")+
ggtitle("Who bought which ticket?")+
theme(plot.caption=element_text(colour="black",face="bold",size=14),
     plot.title=element_text(colour="black",face="bold",size=14,hjust=0.5))+
scale_fill_manual(values=c("indianred2","dodgerblue","orange3","yellow2"))

train_imp_1<-train_imp %>% 
mutate(FareGroup=as.factor(findInterval(Fare,c(0,100,200,500)))) 
levels(train_imp_1$FareGroup)<-c("Cheap","Medium","Expensive","VIP") 
train_imp_1 %>% 
ggplot(aes(Survived,Sex,fill=Sex))+geom_col()+facet_grid(~FareGroup)+
ggpubr::theme_pubr()+
ggtitle("The Cheaper The Ticket, The Lower The Survival Chances")+
scale_fill_manual(values=c("orange4","violet"))+
labs(caption="The VIP Survived")+
theme(plot.caption=element_text(colour="black",face="bold",size=14),
     plot.title=element_text(colour="black",face="bold",size=14,hjust=0.5),
     axis.text.y=element_blank())


train_imp_1 %>% 
group_by(AgeGroup) %>% 
summarise(n=n()) %>% 
mutate(Prop=(n/sum(n))*100) %>% 
arrange(desc(Prop)) %>% 
mutate(AgeGroup=fct_reorder(AgeGroup,Prop)) %>% 
ggplot(aes(AgeGroup,Prop,fill=AgeGroup))+geom_col()+
ggthemes::theme_economist()+
geom_label(aes(label=Prop))+
theme(panel.background=element_rect(fill="white"),
     plot.background=element_rect(fill="white"))+
guides(fill=F)+
ggtitle("There were more Mid-Aged individuals aboard the Titanic")

#Let's visualise survival by Age Group
train_imp %>% 
   ggplot(aes(Survived,fill=Sex))+geom_histogram(stat="count")+facet_wrap(AgeGroup~Pclass)+
  ggtitle("Survival by class,Agegroup and Gender")+theme(plot.title=element_text(hjust=0.5))+
 scale_fill_manual(values=c("steelblue3","orange"))+
ggthemes::theme_gdocs()+
theme(panel.background=element_rect(fill="snow"),
     panel.grid=element_blank(),
     axis.text=element_text(colour="black",face="bold"),
     axis.title=element_text(colour="dodgerblue",face="bold"),
     plot.title=element_text(hjust=0.5,colour="dodgerblue",face="italic"))

train_imp_1 %>% 
filter(Sex=="F"|AgeGroup=="Young") %>% 
group_by(Survived) %>% 
summarise(n=n()) %>% 
mutate(Survival=n/sum(n)) %>% 
ggplot(aes(Survived,as.factor(Survival),fill=as.factor(Survival)))+geom_col()+
ggthemes::theme_hc()+
geom_label(aes(label=Survival))+
theme(panel.background=element_rect(fill="white"),
     plot.background=element_rect(fill="white"))+
guides(fill=F)+
scale_fill_manual(values=c("dodgerblue","tan3"))+
ggtitle("At First Glance It Appears To have Worked")+
labs(x="Survival",y="Proportional of total number")


#Create partition
train1<-createDataPartition(train_imp$Survived,p=0.8,list=F)
validate<-train_imp[-train1,]
train1<-train_imp[train1,]
#Set metric and control
control<-trainControl(method="repeatedcv",number = 10,repeats = 3)
metric<-"Accuracy"

#Set up models
set.seed(233)
fit.knn<-train(Survived~.,data=train1,method="knn",trControl=control,metric=metric)
set.seed(233)
fit.svm<-train(Survived~.,data=train1,method="svmRadial",trControl=control,metric=metric)
set.seed(233)
fit.cart<-train(Survived~.,data=train1,method="rpart",trControl=control,metric=metric)
set.seed(233)
fit.rf<-train(Survived~.,data=train1,method="rf",trControl=control,metric=metric)
set.seed(233)
fit.nb<-train(Survived~.,data=train1,method="nb",trControl=control,metric=metric,verbose=F)
#Try Gradiet Boosting
set.seed(233)
fit.gbm<-train(Survived~.,data=train1,method="gbm",trControl=control,metric=metric,verbose=F)
result<-resamples(list(knn=fit.knn,svm=fit.svm,cart=fit.cart,rf=fit.rf,nb=fit.nb,gbm=fit.gbm))

dotplot(result) 

getTrainPerf(fit.rf)

#Validate 
predicted<-predict(fit.rf,validate)
cmat<-confusionMatrix(predicted,validate$Survived)
cmat$overall

plot(varImp(fit.rf,main="Who's running our forest?"))

test<-read.csv("../input/test.csv")
#............
test<-test %>% 
  mutate(PassengerId=as.factor(PassengerId),Pclass=as.factor(Pclass),
        AgeGroup=as.factor(findInterval(Age,c(0,18,35,100)))) %>% 
  select(-Ticket,-Cabin,-Name,-Embarked)
levels(test$AgeGroup)<-c("Young","Mid Age","Aged")
levels(test$Sex)<-c("F","M")
#Make as train data
#Preprocess and remove NAs from age and Fare
   test2<-preProcess(test,method="medianImpute")
   test2_imp<-predict(test2,test)
   #map nas
   test2_imp<-test2_imp %>% 
     mutate(AgeGroup=as.factor(findInterval(Age,c(0,18,35,100))))
   #.....
   levels(test2_imp$AgeGroup)=c("Young","Mid Age","Aged")

#Try on test data
predictedtest<-predict(fit.rf,test2_imp,na.action=na.pass)
#Set column
Survival<-test2_imp%>% 
  mutate(Survived=predictedtest) %>% 
  select(PassengerId,Survived)
#find the confusion matrix
cm<-confusionMatrix(predictedtest,Survival$Survived)
cm$overall

control12<-trainControl(method="repeatedcv",number=10,repeats=3,search="grid")
tunegrid<-expand.grid(.mtry=c(1:15))
set.seed(233)
rf_tuned<-train(Survived~.,data=train1,method="rf",trControl=control12,metric=metric,
               tuneGrid=tunegrid)
getTrainPerf(fit.rf)
getTrainPerf(rf_tuned)

tunedprediction<-predict(rf_tuned,test2_imp,na.action=na.pass)
SurvivalTuned<-test2_imp %>% 
              mutate(Survived=tunedprediction)
cm2<-confusionMatrix(tunedprediction,SurvivalTuned$Survived)
cm2$overall


 set.seed(233)
gbgrid<-expand.grid(n.trees=950,
                    interaction.depth=25,
                    shrinkage=0.01,
                    n.minobsinnode=15)
fit.gbm_modi<-train(Survived~.,data=train1,method="gbm",tuneGrid=gbgrid,
                    trControl=control,metric=metric,
               verbose=F)
print(fit.gbm_modi)#Accuracy=83%
#
predictedtest_mod<-predict(fit.gbm_modi,newtest_imp,na.action=na.pass)
Survival<-newtest_imp%>% 
  mutate(Survived=predictedtest_mod) %>% 
  select(PassengerId,Survived)
#find the confusion matrix
cm1<-confusionMatrix(predictedtest_mod,Survival$Survived)
cm1$overall
write.csv(Survival,"submitme46.csv",row.names = F)
#Tuned GBM 2
set.seed(233)
gbgrid2<-expand.grid(n.trees=350,
                     interaction.depth=15,
                     shrinkage=0.01,
                     n.minobsinnode=15)
fit.gbm_modi_1<-train(Survived~.,data=train1,method="gbm",tuneGrid=gbgrid,
                    trControl=trainControl(method="repeatedcv",number = 10,repeats=3),
                    metric=metric,
                    verbose=F)
print(fit.gbm_modi_1)
