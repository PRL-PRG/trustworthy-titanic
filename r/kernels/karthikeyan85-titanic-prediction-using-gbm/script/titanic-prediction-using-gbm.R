library(caret)
#..... PREPROCESS............#

preprocess<-function(x)
{
  tt<-x
  names(tt)<-tolower(names(tt))
  suppressWarnings(library(VIM))
  set.seed(3)
  tt_imp<-kNN(tt,variable = c("age","fare"),k=4)
  
  tt_imp[,c(2,3)]<-lapply(tt_imp[,c(2,3)],factor)
  levels(tt_imp$survived)<-c("Dead","Alive")
  levels(tt_imp$embarked)<-c("S","C","Q","S")
  levels(tt_imp$pclass)<-c("1_class","2_class","3_class")
  tt_imp$cabin_c<-tt_imp$cabin
  levels(tt_imp$cabin_c)[1]<-"without_cabin"
  levels(tt_imp$cabin_c)[-1]<-"with_cabin"
  tt_imp$cabin_name<-as.character(tt_imp$cabin)
  tt_imp$cabin_name[tt_imp$cabin==""]<-"no_cabin"
  tt_imp$cabin_name[grep("A",tt_imp$cabin)]<-"A"
  tt_imp$cabin_name[grep("B",tt_imp$cabin)]<-"B"
  tt_imp$cabin_name[grep("C",tt_imp$cabin)]<-"C"
  tt_imp$cabin_name[grep("D",tt_imp$cabin)]<-"D"
  tt_imp$cabin_name[grep("E",tt_imp$cabin)]<-"E"
  tt_imp$cabin_name[grep("F",tt_imp$cabin)]<-"F"
  tt_imp$cabin_name[grep("G",tt_imp$cabin)]<-"G"
  tt_imp$cabin_name[grep("T",tt_imp$cabin)]<-"T"
  
  
  
  tt_imp$family_size<-(tt_imp$sibsp+tt_imp$parch+1)
  tt_imp$kid[tt_imp$age>18]<-0
  tt_imp$kid[tt_imp$age<=18]<-1
  
  ticket_fre<-as.data.frame(table(tt_imp$ticket))
  tt_imp$ticket_size<-tt_imp$ticket
  levels(tt_imp$ticket_size)<-ticket_fre$Freq
  tt_imp$ticket_size<-as.numeric(as.character(tt_imp$ticket_size))
  
  tt_imp$family_name<-sapply(strsplit(as.character(tt_imp$name),"[,.]"),'[',1)
  tt_imp$family_salute<-sapply(strsplit(as.character(tt_imp$name),"[,.]"),'[',2)
  tt_imp$family_salute<-gsub(" ","",tt_imp$family_salute)
  tt_imp$mother<-0
  tt_imp$mother[tt_imp$family_salute=="Mrs" & tt_imp$kid==0 &
                  tt_imp$parch>0]<-1
  
  tt_imp$class_cab<-paste(tt_imp$pclass,tt_imp$cabin_c,sep = "_")
  tt_imp$class_cabname<-paste(tt_imp$pclass,tt_imp$cabin_name,sep = "_")
  
  tt_imp$family_ticket <- ave(tt_imp$family_name, tt_imp$ticket,
                              FUN=function(x) length(unique(x)))
  tt_imp$group_ticket[tt_imp$family_ticket==1]<-"same_family"
  tt_imp$group_ticket[tt_imp$family_ticket>1]<-"different_family"
  tt_imp$group_ticket<-as.factor(tt_imp$group_ticket)
  tt_imp$regroup<-paste("on this ticket",tt_imp$ticket_size,
                        "people from",tt_imp$family_ticket,
                        tt_imp$group_ticket)
  
  tt_imp
  
}


suppressWarnings(library(ggplot2))
to_model<-read.csv("../input/train.csv")
to_validate<-read.csv("../input/test.csv")
to_validate$Survived<-0
to_validate<-to_validate[,c(1,12,2:11)]

full_data<-preprocess(rbind(to_model,to_validate))

#feature_t<-full_data[,-c(1,2,5,6,7,8,10,11,12,14,19,22)]
feature_t<-full_data[,c(3,5,6,10,12,16,17,19,21,22,24,25,27)]
dummy_feature_t<-dummyVars(~.,data = feature_t,levelsOnly = FALSE,
                           sep = "_")
dummy_feature<-as.data.frame(predict(dummy_feature_t,feature_t))

final_data<-cbind.data.frame(full_data[,c(1,2)],dummy_feature)

#.....selection important variables............

control <- trainControl(method="repeatedcv", number=10, repeats=5)
set.seed(123)
model_r<-train(survived~.-passengerid,data=final_data,method="gbm",
               trControl=control,verbose=FALSE)

imp_Var<-as.data.frame(varImp(model_r)$importance)
imp_Var<-as.data.frame(cbind(row.names(imp_Var),imp_Var$Overall))
names(imp_Var)<-c("variable","values")
imp_Var$values<-as.numeric(as.character(imp_Var$values))
imp_Var$variable<-gsub("`","",imp_Var$variable)
imp_Var_fin<-imp_Var[ imp_Var$values>0.9,]
#imp_Var_fin<-imp_Var_fin[order(imp_Var_fin$values,decreasing = TRUE),]

final_data_cor<-as.data.frame(cbind(full_data[,c(1,2)],
                                    final_data[,as.vector(imp_Var_fin$variable)]))

final_data_model_cor<-final_data_cor[c(1:891),]
final_data_validate_cor<-final_data_cor[-c(1:891),]

# actual model starts...................................................

final_data_cor_tr<-final_data_model_cor[c(1:800),]
final_data_cor_tt<-final_data_model_cor[-c(1:800),]

library(rpart)

control <- trainControl(method="repeatedcv", number=10, repeats=5,
                        summaryFunction=defaultSummary)
set.seed(123)
#model<-train(survived~.-passengerid,data=final_data_cor_tr,method="gbm",
#             verbose=FALSE,trControl=control)
model<-rpart(survived~.-passengerid,data=final_data_cor_tr,method="class")

pred<-predict(model,final_data_cor_tt,type = "class")

#pred<-predict(model,final_data_cor_tt)

confusionMatrix(pred,final_data_cor_tt$survived)

#old modeling & upload...................................................

final_data_model_cor<-final_data_cor[c(1:891),]
final_data_validate_cor<-final_data_cor[-c(1:891),]

control <- trainControl(method="repeatedcv", number=10, repeats=5)
gbmgrid <- expand.grid(interaction.depth=1, n.trees = 100,
                       shrinkage=0.1,n.minobsinnode=10)
set.seed(123)
#model<-train(survived~.-passengerid,data=final_data_model_cor,
#             method="gbm",verbose=FALSE,trControl=control, 
#             tuneGrid = gbmgrid)

#pred<-predict(model,final_data_validate_cor)

model<-rpart(survived~.-passengerid,data=final_data_model_cor,method="class")

pred<-predict(model,final_data_validate_cor,type = "class")
output<-as.data.frame(cbind(final_data_validate_cor$passengerid,pred))

names(output)<-c("PassengerId", "Survived")
output$Survived<-as.factor(output$Survived)
levels(output$Survived)<-c("0","1")
write.csv(output,"output1.csv",row.names = FALSE)
