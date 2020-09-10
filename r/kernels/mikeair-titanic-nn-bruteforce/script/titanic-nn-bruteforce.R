#######################################################################
# The intention of this kernel is only to demonstrate a simple way of
# grid searching optimal network structure/hyperparams, evaluated by AUC
########################################################################

library(neuralnet);
library(pROC)
library(caret)

data <- read.csv(file="../input/train.csv",header=T,na.strings=c(""))
str(data)

data$male <- as.integer(data$Sex == "male")
data$embarkedC <- as.integer(data$Embarked == "C")
data$embarkedQ <- as.integer(data$Embarked == "Q")

#mean imputation of everything that have missing values:
for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
}

#removing some of the features for the sake of simplicity:
data$Name = NULL;data$Ticket = NULL;data$Cabin=NULL;
data$PassengerId=NULL;data$Sex=NULL;data$Embarked=NULL
#str(data)

#train <- data

######## subsetting for train and test set:

index <- sample(1:nrow(data),round(0.75*nrow(data)))

train <- data[index,]
test <- data[-index,]


#Train data: making model matrix
m <- model.matrix(  ~ Survived + Pclass + male + Age + SibSp+ Fare+ embarkedC+ embarkedQ, data=train)
head(m)

#Test data: making model matrix
m2 <- model.matrix(  ~  Pclass + male + Age + SibSp+ Fare+ embarkedC+ embarkedQ, data=test)
head(m2)

###############################################################################

Test_metric = NULL
Test_metric1 = NULL
Test_metric2 = NULL
Test_metric3 = NULL
Test_AUC = NULL
Train_AUC = NULL
RES_DF = NULL
overall = NULL

#Set the hyper parameter alternatives in here:
#more layers are defined by: c(25,25), 
#see: ?neuralnet for more info

HiddenNodes1 <- list(c(25),c(50))#,  c(25,25,25), c(10,20,20), c(15,15,15)
Thresholds=c(0.5,0.8)
LearningRate=c(0.15,0.2)
Algorithms=c("rprop+")
Epochs=1


for (k in HiddenNodes1){
  for (j in LearningRate){
    for (i in Thresholds){
      for (l in Algorithms){
        
        r2 <- neuralnet( Survived ~ Pclass + male + Age + SibSp+ Fare+ embarkedC+ embarkedQ, 
                         data=train, 
                         hidden=k,               #        <- c(nodesLayer1,nodesLayer2,nodesLayer3)
                         threshold=i,
                         rep=Epochs,        
                         learningrate=j, 
                         algorithm =l, #"rprop+"
                         lifesign = "full",
                         err.fct = "sse"
        )
        
        ######### Storing values from the model ########################################
        
        Test_metric = rbind(Test_metric, j) #learningrate
        Test_metric1 = rbind(Test_metric1, i) #threshold
        Test_metric2 = rbind(Test_metric2, k) #hidden nodes in layer1
        Test_metric3 = rbind(Test_metric3, l) #algorithm
        
        ################################################################################

        #predictions
        res <- compute(r2, m2[,c("Pclass", "male", "Age", "SibSp","Fare","embarkedC","embarkedQ")])     #test
        res2 <- compute(r2, m[,c("Pclass", "male", "Age", "SibSp","Fare","embarkedC","embarkedQ")])     #train
        
        ################################################################################
        
        #Predictions DF for train set:
        pred_train.df = as.data.frame(res2$net.result)
        names(pred_train.df)[names(pred_train.df)=="V1"] <- "prediction"
        pred_train.df$prediction <- as.numeric(pred_train.df$prediction < 0.5)
        
        
        #Predictions DF for test set:
        pred_test.df = as.data.frame(res$net.result)
        names(pred_test.df)[names(pred_test.df)=="V1"] <- "prediction"
        pred_test.df$prediction <- as.numeric(pred_test.df$prediction < 0.5)
        
        #============ TRAIN AUC ==============#
        
        #Make a roc object: First target, then prediction
        rocobj_train <- roc(train$Survived, pred_train.df$prediction)
        
        #get the area under the curve:
        Train_AUC <- rbind(Train_AUC, rocobj_train$auc)
        
        #============ TEST AUC ==============#
        
        #Make a roc object: First target, then prediction
        rocobj_test <- roc(test$Survived, pred_test.df$prediction)
        
        #get the area under the curve:
        Test_AUC <- rbind(Test_AUC, rocobj_test$auc)
        
        #########################################################
        
        print("The test-AUC for this model was:")
        print(rocobj_test$auc)
        
        print("The train-AUC for this model was:")
        print(rocobj_train$auc)
        
      }}}}


################################################################################

#Wrapping up the results:
Difference = as.data.frame(as.numeric(Test_AUC-Train_AUC));colnames(Difference)="Difference"
Difference

resultsettings.df <- as.data.frame(cbind(Test_metric[1:length(Test_AUC),], Test_metric1[1:length(Test_AUC),], Test_metric2[1:length(Test_AUC),], Test_metric3[1:length(Test_AUC),], Train_AUC, Test_AUC, Difference))
rownames(resultsettings.df) <- NULL
colnames(resultsettings.df) <- c("Learningrate","Thresholds","Hiddenlayers","Algo","AUC_Train","AUC_Test","Difference")

#Showing the performance of the models, for further evaluation:
resultsettings.df[order(resultsettings.df$AUC_Test, decreasing = TRUE),]



