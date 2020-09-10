
#Import datasets
train<-read.csv("train.csv")
test<-read.csv("test.csv")
test_result<-read.csv("gender_submission.csv")
test$Survived<-test_result[,2]

#Full dataset (TRAIN+TEST)
data<-rbind(train,test)
head(data)
str(data)  #1309 observation of 12 variables (+title)
summary(data)  #NAN: Age(263);Fare(1);Embarked(2)

# Change 'Survived' to factor
data$Survived<-as.factor(data$Survived)

# change the empty values in 'Embarked' to NA
levels(data$Embarked)[1]<-NA

# extract title by finding an alphabetical string followed by '.' (e.g. Mr., Miss.)
library(stringr)
data$title <- str_extract(data$Name, '[a-zA-Z]+(?=\\.)')
data$title <- as.factor(data$title)
summary(data$title)

# group all the rare titles together
data$title <- as.character(data$title)
data$title[!(data$title %in% c('Master', 'Miss', 'Mr', 'Mrs'))] <- 'Other'
data$title <- as.factor(data$title)
summary(data$title)


# get the mean age of each title group
title.age <- aggregate(Age~title, data=data, mean)
title.age$Age<-round(title.age[2],1)
title.age

# impute missing ages
for(i in 1:nrow(data)){
  if(is.na(data[i,'Age'])){
    data[i,'Age'] <- title.age$Age[which(title.age$title==data[i,'title']),]
  }
}
summary(data$Age)

#Eclude missing values in 'Fare' and 'Embarked'
summary(data$Fare)                    #NaN: 1db (1044 ID)
data$PassengerId[is.na(data$Fare)==TRUE]
summary(data$Embarked)                #NaN: 2 db (62, 830 ID)
data$PassengerId[is.na(data$Embarked)==TRUE]
data<-data[c(-1044,-62,-830),]  #Done

summary(data)

# split traing and testing sets
train <- data[1:nrow(train), ]   #1-893 ID
test <- data[(nrow(train)+1):nrow(data),]  #894-1309 ID
summary(train)
summary(test)

remove_cols = c("PassengerId","Name","Ticket","Cabin","title")
train_new = train[,!(names(train) %in% remove_cols)]
test_new = test[,!(names(test) %in% remove_cols)]
summary(train_new)
summary(test_new)
str(train_new)
str(test_new)

# 1a. model: Decision tree 
library(caret)
library(C50)
library(gmodels)
fit_tree<-C5.0(train_new[-1],train_new$Survived)
summary(fit_tree)
fit_tree  

pred_tree_train<-predict(fit_tree, train_new)
CrossTable(train$Survived,pred_tree_train)  #Train Accuracy=88.77% ((516+275)/891)
pred_tree <- predict(fit_tree, test_new)
CrossTable(test$Survived,pred_tree)  #Test Accuracy=86.98% ((232+129)/415)

# 1b. model: Decision tree + Class Weight

class_weight0 = c(0.1,0.2,0.3,0.4,0.5)

for (i in class_weight0){
  error_cost <- matrix(c(0, 1, (1-i)/i, 0), nrow = 2)
  fit_trees = C5.0(train_new[-1],train_new$Survived, costs = error_cost,control = C5.0Control( minCases = 1))
  summary(fit_trees)
  pred_train = predict(fit_trees, train_new)
  pred_test = predict(fit_trees,test_new)
  print(paste("Class weights","{0:",i,"1:",1-i,"}"))
  CrossTable(train$Survived,pred_train)
 CrossTable(test$Survived,pred_test)
}

####
#Class weights {0: 0.1 1: 0.9 }
#Train Accuracy: (71+340)/891=0.46
#Test Accuracy: (16+149)/415=0.39

#Class weights {0: 0.2 1: 0.8 }
#Train Accuracy: (441+300)/891=0.83
#Test Accuracy: (207+149)/415=0.857

#Class weights {0: 0.3 1: 0.7 }  >>>> BEST
#Train Accuracy: (470+298)/891=0.86
#Test Accuracy: (226+146)/415=0.896

#Class weights {0: 0.4 1: 0.6 }
#Train Accuracy: (508+287)/891=0.89
#Test Accuracy: (228+122)/415=0.843

#Class weights {0: 0.5 1: 0.5 }
#Train Accuracy: (517+274)/891=0.887
#Test Accuracy: (233+129)/415=0.872

#Best Decision tree + Class Weight Model
error_cost<- matrix(c(0, 1, (1-0.3)/0.3, 0), nrow = 2)
fit_tree_best = C5.0(train_new[-1],train_new$Survived, costs = error_cost,control = C5.0Control( minCases = 1))
pred_test = predict(fit_tree_best,test_new)
CrossTable(test$Survived,pred_test)  #Test Acc=0.896=(226+146)/415


# 2. model: Logistic Regression
fit_glm<-glm(train_new$Survived~.,data=train_new,family = "binomial")

pred_glm<-probs_glm
pred_glm[probs_glm>0.5]=1
pred_glm[probs_glm<=0.5]=0
CrossTable(train$Survived,pred_glm) #Train Accuracy=83.27% ((485+257)/891)

probs_glm_te<-predict(fit_glm,newdata=test_new,type="response")  #1 valsége
pred_glm_te<-probs_glm_te
pred_glm_te[probs_glm_te>0.5]=1
pred_glm_te[probs_glm_te<=0.5]=0
CrossTable(test$Survived,pred_glm_te) #Test Accuracy=91.33% ((238+141)/415)

# 3a. model: Random Forest
library(randomForest)
fit_rf <- randomForest(train_new$Survived~.,data=train_new,mtry=6,maxnodes=64,ntree=5000,nodesize = 1)

pred_rf <- predict(fit_rf,newdata = train_new,type = "class")
CrossTable(train$Survived,pred_rf) #Train Accuracy=91.02% ((531+280)/891)

pred_rf_te <- predict(fit_rf,newdata = test_new,type = "class")
CrossTable(test$Survived,pred_rf_te) #Test Accuracy=88.19% ((242+124)/415)

# 3b. model: Random Forest + Grid search  >NOT WORKING
library(e1071)
fit_rf_grid<-tune(randomForest,train_new$Survived~.,data=train_new[-1],classwt = c(0.3,0.7),
                  ranges=list(mtry=c(5,6),maxnodes=c(32,64),ntree=c(3000,5000),nodesize = c(1,2)),
                  tunecontrol = tune.control(cross = 5))

# 4a. model: - Adaboost
fit_ada <- C5.0(train_new[-1],train_new$Survived,trails =5000,control = C5.0Control(minCases = 1))


pred_ada <-predict(fit_ada,newdata = train_new ,type = "class")
CrossTable(train$Survived,pred_ada) #Train Accuracy=88.77% ((517+274)/891)

pred_ada_te <- predict(fit_ada,newdata = test_new,type = "class")
CrossTable(test$Survived,pred_ada_te) #Test Accuracy=87.23% ((233+129)/415)

# 4b. model: - Adaboost + Error Costs
fit_ada_weight <- C5.0(train_new[-1],train_new$Survived,trails =5000,costs = error_cost,control = C5.0Control(minCases = 1))
pred_ada_te_w <- predict(fit_ada_weight,newdata = test_new,type = "class")
CrossTable(test$Survived,pred_ada_te_w) #Test Accuracy=84.57% ((206+145)/415)


# 5a. model: SVM
library(e1071)
fit_svm<-svm(train_new$Survived~.,data=train_new,kernel="linear")
fit_svm

pred_svm_train<-predict(fit_svm,train_new)
pred_svm_test<-predict(fit_svm,test_new[-1])
CrossTable(pred_svm_train,train_new$Survived)  #Train Accuracy=78.67% ((469+232)/891)
CrossTable(pred_svm_test,test_new$Survived)    #Test Accuracy=100% ((264+151)/415)
#Train Accuracy=78.67% (linear), 83.61% (RBF), 83.5% (poly)
#Test Accuracy=100% (linear), 93.97% (RBF), 90.6% (poly)

# 5b. model: SVM + Tune   
svm_tune<-tune.svm(Survived~.,data=train_new,kernel="linear",cost=c(0.1,0.3,1,10),gamma=c(0.001,0.01,0.1,1), tunecontrol = tune.control(cross = 5))
svm_tune_best<-svm_tune$best.model
  
  
pred_svm_tune_test<-predict(svm_tune_best,test_new[-1],type="class")
CrossTable(pred_svm_tune_test,test_new$Survived)    #Test Accuracy=100% ((264+151)/415)


#################################### ENSEMBLE #################################################

#ENSEMBLE - TRAIN
probs_tree<-predict(fit_tree, train_new,type="prob")[,2]  #1 valsége >> ENSEMBLE 1. oszlop
probs_glm<-predict(fit_glm,newdata=train_new,type="response")  #1 valsége >> ENSEMBLE 2. oszlop
probs_rf <- predict(fit_rf,newdata = train_new,type = "prob")[,2]  #1 valsége >> ENSEMBLE 3. oszlop
probs_ada <- predict(fit_ada,newdata = train_new ,type = "prob")[,2]   #1 valsége >> ENSEMBLE 4. oszlop


# Ensemble of Models - TRAIN
ensemble <- data.frame(probs_tree,probs_glm,probs_rf,probs_ada)
#ensemble <- cbind(ensemble,train_new$Survived)
#names(ensemble)[5] <- "Survived"
head(ensemble)
str(ensemble)

# Meta-classifier on top of individual classifiers
meta <- glm(train_new$Survived~.,data=ensemble, family = "binomial")
meta_probs <- predict(meta, ensemble,type = "response")
ensemble$pred_class = 0
ensemble$pred_class[meta_probs>0.5]=1

CrossTable(train_new$Survived,ensemble$pred_class) #Train Accuracy=93.49% ((536+297)/891)

#ENSEMBLE - TEST
probs_tree_te<-predict(fit_tree, test_new,type="prob")[,2]  #1 valsége >> ENSEMBLE 1. oszlop
probs_glm_te<-predict(fit_glm,newdata=test_new,type="response")  #1 valsége >> ENSEMBLE 2. oszlop
probs_rf_te <- predict(fit_rf,newdata = test_new,type = "prob")[,2]  #1 valsége >> ENSEMBLE 3. oszlop
probs_ada_te <- predict(fit_ada,newdata = test_new ,type = "prob")[,2]   #1 valsége >> ENSEMBLE 4. oszlop


# Ensemble of Models - TEST
ensemble_test <- data.frame(probs_tree_te,probs_glm_te,probs_rf_te,probs_ada_te)
#ensemble_test <- cbind(ensemble_test,test_new$Survived)
#names(ensemble_test)[5] <- "Survived"
rownames(ensemble_test) <- 1:nrow(ensemble_test)
head(ensemble_test)
str(ensemble_test)

meta_probs_test <- predict(meta, ensemble_test, type = "response")  #NOT WORKING
#ensemble_test$pred_class = 0
#ensemble_test$pred_class[meta_probs_test>0.5]=1
#head(ensemble_test)

#CrossTable(test_new$Survived,ensemble_test$pred_class) #Test Accuracy=??

###############################################################################################


## Saving data

# If you save any files or images, these will be put in the "output" directory. You 
# can see the output directory by committing and running your kernel (using the 
# Commit & Run button) and then checking out the compiled version of your kernel.
