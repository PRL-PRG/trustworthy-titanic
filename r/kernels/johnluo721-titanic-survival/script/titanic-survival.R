


# read csv data 
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
library(lattice)
library(ggplot2)
library(caret)
library(plyr)

# Gain insight of data structure
head(train,10)
names(train)
summary(train)
str(train)


#============ Tide up data set
# 1. convert all category variable into factor
# 2. Missing value imputation: fill missing values
# 3. feature engineering: Introduce new variables

train$Survived<-as.factor(train$Survived)
train$Pclass<-as.factor(train$Pclass)
str(train)
train_new<-train

# 2. fill missing values
na_index <- which(is.na(train_new$Age))
na_index
train_new$Age[na_index] <- median(train_new$Age,na.rm=TRUE)
train_new$Age
na_index <- which(is.na(train_new$Fare))
train_new$Fare[na_index] <- median(train_new$Fare,na.rm=TRUE)
train_new$Embarked[train_new$Embarked==""] <- "S"

# 3. Introduce new variables
  # age group
train_new$Age_grp[train_new$Age<=2]<-"infant"
train_new$Age_grp[train_new$Age>2 & train_new$Age<=18]<-"child"
train_new$Age_grp[train_new$Age>18]<-"adult"
train_new$Age_grp<-as.factor(train_new$Age_grp)
  #family size                
train_new$family<-train_new$SibSp+train_new$Parch+1
  #Cabin class
train_new$Cabin_class<-substr(train_new$Cabin,1,1)
train_new$Cabin_class[train_new$Cabin_class==""]<-"unknow"
train_new$Cabin_class<-as.factor(train_new$Cabin_class)


#=====test dataset

test$Pclass<-as.factor(test$Pclass)
test_new<-test
test_new$family<-test_new$SibSp+test_new$Parch+1

na_index <- which(is.na(test_new$Age))
test_new$Age[na_index] <- median(train_new$Age,na.rm=TRUE)
na_index <- which(is.na(test_new$Embarked))
test_new$Embarked[na_index] <- "S"
na_index <- which(is.na(test_new$Fare))
test_new$Fare[na_index] <- median(train_new$Fare,na.rm=TRUE)

test_new$Age_grp[test_new$Age<=2]<-"infant"
test_new$Age_grp[test_new$Age>2 & test_new$Age<=18]<-"child"
test_new$Age_grp[test_new$Age>18]<-"adult"
test_new$Age_grp<-as.factor(test_new$Age_grp)
#family size                
test_new$family<-test_new$SibSp+test_new$Parch+1
#Cabin class
test_new$Cabin_class<-substr(test_new$Cabin,1,1)
test_new$Cabin_class[test_new$Cabin_class==""]<-"unknow"
test_new$Cabin_class<-as.factor(test_new$Cabin_class)


set.seed(1234)
# =====================Tree Model
library(rpart)

library(rpart.plot)
library(RColorBrewer)

tree_model<-rpart(Survived~ Age+Pclass+Sex+Age+family+Embarked+Fare+SibSp+Parch+Age_grp+Cabin_class
                  ,data=train_new, 
                  method= "class",  
                  parms= list(split="gini"
                              # ,loss= matrix(c(0,1,10,0), byrow=TRUE, nrow=2)  #Define Loss Matrix for unbalanced data
                                           
                              #,prior= c(0.7, 0.3)  #prior probabilities setting for unbalanced data
                  ),
                  ## rpart algorithm options (These are defaults, thus the whole control argument can be omitted)
                  control = rpart.control(
                    minsplit       = 20,   # minimum number of observations required before split
                    minbucket      = 10, # minimum number of observations in any terminal node. deault = minsplit/3
                    cp             = 0.0001, # complexity parameter used as the stopping rule
                    #stump = F,
                    maxcompete     = 4,    # number of competitor splits retained in the output
                    maxsurrogate   = 5,    # number of surrogate splits retained in the output
                    #usesurrogate   = 2,    # how to use surrogates in the splitting process
                    xval           = 10,   # number of cross-validations
                    #surrogatestyle = 0,    # controls the selection of a best surrogate
                    maxdepth       = 5)   # maximum depth of any node of the final tree
                  ##,
                  ## cost = c() # a vector of cost for each variable
                  )
                  
  plot(tree_model)  
 
  tree_model
plotcp(tree_model)
summary(tree_model)$cptable

# ===============================  Diagnostics ===============================

#check the accuracy on prediction
prediction_tree<-predict(tree_model, train_new,type="class")
table(train_new$Survived,prediction_tree)

tree_sumry<-summary(tree_model)


# =========Prune Tree
# Principal of prune the tree is up to point have minimum cross validation error, 
# and using cp when minimum cross validation error achieved to decide size of tree. 


plotcp(tree_model)
printcp(tree_model) 
min_cp<-tree_sumry$cptable[which.min(tree_sumry$cptable[,"xerror"]), "CP"]

prn_tree_model<-prune( tree_model,  cp=min_cp)
# alternatively can set cp parameter directly
# prn_tree_model<-rpart( as.factor(Survived)~ Pclass+Sex+Age+family,train_new, 
#                   method= "class",  cp=min_cp)

#fancyRpartPlot(prn_tree_model)
# ==========Prediction 
prediction_tree<-predict(prn_tree_model, test_new,type="class")

# ==========Submission file
#submission_tree<-data.frame(PassengerId=test_new$PassengerId, Survived=prediction_tree)

#write.csv(submission_tree,file="../output/submission_tree.csv",row.names = FALSE)


#=========================  Random Forest ===============================

library(randomForest)

#===========Point need to note
# 1.Random Forest can not deal with missing/NA value, need fill the missing before run the model
# 2.For the factor variable, it should have same level in both training and testing dataset
levels(test_new$Embarked) <- levels(train_new$Embarked)
levels(test_new$Age_grp) <- levels(train_new$Age_grp)
levels(test_new$Cabin_class) <- levels(train_new$Cabin_class)

#===========Random Forest Model

forest<-randomForest( as.factor(Survived)~ Pclass+Sex+Age+family+Fare+SibSp+Parch+Embarked+Age_grp+Cabin_class,
                      data = train_new,  
                      ntree= 100, #Number of trees to grow
                      # proximity=T,
                      #prior=c(0.6,0.4),
                      importance= TRUE,
                      #na.action = na.omit,# Raindom forest can not deal with missing value
                      do.trace= 100,
                      keep.forest=TRUE,
                      maxnodes=50, #Maximum number of terminal nodes trees in the forest
                      nodesize=100 #Minimum size of terminal nodes.
                      #strata=
                    )


print(forest)
str(forest)
summary(forest)
importance(forest)

# ==========Submission file

prediction_rf<-predict(forest,test_new)


# ==========Submission file
#submission_rf<-data.frame(PassengerId=test_new$PassengerId, Survived=prediction_rf)

#write.csv(submission_rf,file="../output/submission_rf.csv",row.names = FALSE)

# ========================  GBM CARET===========================================
library(caret)
set.seed(1234)
# note: need define dependent variable as factor to let model know binary outcome instead regression
# train$Survived<-as.factor(train$Survived)
str(train_new)


# classProbs =TURE need fix some issue
# levels(train$Survived) <- make.names(levels(factor(train$Survived)))
# class(train$Survived)
# 
# levels <- unique(train$Survived) 
# train$Survived <- factor(train$Survived, labels=make.names(levels))

#====Feature Selection
# subsets = c(2,3,4,5,6,7,8) # Number of variable in the model
# ctrl= rfeControl(functions = rfFuncs, method = "cv",verbose = TRUE, returnResamp = "final")
# Profile = rfe(train_new[,-c(Name,Ticket)], train$Survived, sizes = subsets, rfeControl = ctrl)
# plot(Profile)

str(train)

#====
gbm_model<- train(Survived~ Pclass+Sex+Age+family+Embarked+Fare+SibSp+Parch+Age_grp#+Cabin_class
                  ,data=train_new, 
                  method="gbm",

                  trControl=trainControl(   
                                        method = "cv", #Cross validation
                                        number = 10,   #number of fold for corss validation
                                        repeats = 5, 
                                        classProbs = FALSE,  #need set up as true to allow output ROC and AUC
                                                             # Note setting this parm to true may cause issue
                                                             #one way to fix is factor predictor properly
                                        summaryFunction = defaultSummary,
                                        verboseIter = TRUE #print log
                                        #metric=
                                        ),
                  tuneGrid=expand.grid(n.trees=c(150, 200, 250, 300),  #Boosting Iterations or number of trees
                                      interaction.depth=c(3,4,5), #Max Tree Depth
                                      shrinkage=c(0.01,0.1), #Shrinkage or learning rate
                                      n.minobsinnode=c(20,30) #minimum number of training set samples in a node
                                      )
                  
                  #tuneLength=5 #Alternative way to tune the parameter

                  )
str(train_new)
#===========
print(gbm_model)
plot(gbm_model)

prediction_gbm<-predict(gbm_model, test_new)
# ==========Submission file



#submission_gbm<-data.frame(PassengerId=test_new$PassengerId, Survived=prediction_gbm)

#write.csv(submission_gbm,file="../output/submission_gbm.csv",row.names = FALSE)

