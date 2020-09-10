
# This Python 3 environment comes with many helpful analytics libraries installed
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
# For example, here's several helpful packages to load in 

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory


# Any results you write to the current directory are saved as output.

library(titanic)
attach(titanic_train)
library(pROC)
library(e1071)
library(kernlab)
library(ROCR)

library(rpart) 
library(partykit)
library(randomForest) 

set.seed(123)
#Explorando los datos
RegresionU<-glm(Survived~Age+Fare,family=binomial())
summary(RegresionU)

RegresionRoc<-roc(Survived[is.na(Age) == FALSE & is.na(Fare) == FALSE],as.vector(RegresionU$fitted.values))
plot(RegresionRoc)

auc(RegresionRoc)
#AUC de regresion original
table(Survived[is.na(Age) == FALSE & is.na(Fare) == FALSE],RegresionU$fitted.values>.5)


RegresionSVM<-ksvm(Survived~Fare+Age,data=titanic_train,kernel="rbfdot",kpar=list(sigma=1),type="C-svc",C=714,scaled=c()) 
RegresionSVM
plot(RegresionSVM,data=titanic_train[is.na(Age)==FALSE & is.na(Fare)==FALSE,])


titanic_train2 <- cbind(Fare,Age)
titanic_train2 <- titanic_train2[is.na(Fare)==FALSE & is.na(Age)==FALSE,]
RegresionPredictSVM<-predict(RegresionSVM,titanic_train2,type="decision") 


library(ROCR)
Survived2 <- Survived[is.na(Fare)==FALSE & is.na(Age)==FALSE]
RegresionPredictSVMPred <- prediction(RegresionPredictSVM,Survived2)
RegresionPredictSVMPredPerf <- performance(RegresionPredictSVMPred,measure = "tpr", x.measure = "fpr") 
# Paso de performance
plot(RegresionPredictSVMPredPerf) # Curva ROC
abline(0,1) # Identidad

RegresionPredictSVMPredAUC <- performance(RegresionPredictSVMPred,measure = "auc")  
RegresionPredictSVMPredAUC@y.values


sigest(Survived~Fare+Age,data=titanic_train) 
RegresionPredictSVMb<-ksvm(Survived~Fare+Age,data=titanic_train,kernel="rbfdot",type="C-svc",C=714,scaled=c()) 
RegresionPredictSVMb
plot(RegresionPredictSVMb,data=titanic_train[is.na(Age)==FALSE & is.na(Fare)==FALSE,])

RegresionPredictSVMbdec<-predict(RegresionPredictSVMb,titanic_train2,type="decision") 
RegresionPredictSVMbdecpred <- prediction(RegresionPredictSVMbdec,Survived2) 
RegresionPredictSVMbdecpref <- performance(RegresionPredictSVMbdecpred,measure = "tpr", x.measure = "fpr") 
plot(RegresionPredictSVMbdecpref) # Curva ROC
abline(0,1) # Recta a 45 grados

RegresionPredictSVMbdecpredAUC <- performance(RegresionPredictSVMbdecpred,measure = "auc")  
RegresionPredictSVMbdecpredAUC@y.values
RegresionPredictSVMbC<-ksvm(x=cbind(Fare,Age),y=Survived,data=titanic_train,kernel="rbfdot",type="C-svc",C=714,scaled=c(),prob.model=TRUE) 
RegresionPredictSVMbCProb<-predict(RegresionPredictSVMbC,titanic_train2,type="probabilities") 

table(Survived2,RegresionPredictSVMbCProb[,2]>0.396)

RegSVMRBFC<-ksvm(Survived~Fare+Age,data=titanic_train,kernel="rbfdot",type="C-svc",C=1,scaled=c()) 
RegSVMRBFC
plot(RegSVMRBFC,data=titanic_train[is.na(Age)==FALSE & is.na(Fare)==FALSE,])
RegSVMRBFCdec<-predict(RegSVMRBFC,titanic_train2,type="decision") 
RegSVMRBFCpred <- prediction(RegSVMRBFCdec,Survived2) 
RegSVMRBFCpref <- performance(RegSVMRBFCpred,measure = "tpr", x.measure = "fpr") 
plot(RegSVMRBFCpref) 
abline(0,1)

RegSVMRBFCpredAUC <- performance(RegSVMRBFCpred,measure = "auc")  
RegSVMRBFCpredAUC@y.values
RegSVMRBFCC<-ksvm(x=cbind(Fare,Age),y=Survived,data=titanic_train,kernel="rbfdot",type="C-svc",C=714,scaled=c(),prob.model=TRUE) 
RegSVMRBFCCProb<-predict(RegSVMRBFCC,titanic_train2,type="probabilities") 

table(Survived2,RegSVMRBFCCProb[,2]>0.396)

attach(titanic_train)
variablesCam <- as.data.frame(model.matrix(~Survived+Pclass+Sex+Age+SibSp+Parch+Fare+Sex*Age+Sex*SibSp+Sex*Parch+Sex*Fare+Pclass*Age+Pclass*SibSp+Pclass*Parch+Pclass*Fare))

# primer arbol de clasificacion con Sexmale+Age+SibSp+Parch+Fare
Survived2<-factor(variablesCam$Survived,levels=0:1,labels=c("No","Si"))

TrainD<-rpart(Survived2~Sexmale+Age+SibSp+Parch+Fare,method="class",data=variablesCam,control=rpart.control(cp=0))
printcp(TrainD)
summary(TrainD)
plot.new()
plotcp(TrainD)
# podando el arbol en el minimo valor cp por validacion cruzada
cpxerrormin<-TrainD$cptable[which.min(TrainD$cptable[,4]),1]
TrainDPrune<-prune(TrainD,cp=cpxerrormin)
TrainDPruneParty<-as.party(TrainDPrune)
plot(TrainDPruneParty)
# podando el arbol en el minimo valor cp con dev standard
TrainDPruneD1<-prune(TrainD,cp=0.024137931)
TrainDPruneD1Party<-as.party(TrainDPruneD1)
plot(TrainDPruneD1Party)
# utilizando el arbol en test
data(titanic_test)
detach(titanic_train)
attach(titanic_test)
VarSpread <- as.data.frame(model.matrix(~Pclass+Sex+Age+SibSp+Parch+Fare))

VarSpread<-VarSpread[,-1]
PredTrainDPruneD1<-predict(TrainDPruneD1,newdata=VarSpread)

