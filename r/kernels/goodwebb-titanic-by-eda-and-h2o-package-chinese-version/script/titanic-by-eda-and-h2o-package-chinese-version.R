## ----setup, include=FALSE, echo=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo=TRUE, error=FALSE)


## ----message = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(plotly)
library(ggplot2)
library(h2o)  #h2o建模使用
library(rpart)
library(rpart.plot)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData<-read.csv("../input//train.csv", stringsAsFactors = F,na.strings = c("NA", ""))
testData<-read.csv("../input//test.csv", stringsAsFactors = F,na.strings = c("NA", ""))
#train :891位,test:418位 
dim(trainData)
dim(testData)
trainData$Survived<-as.factor(trainData$Survived)
FullData<-bind_rows(trainData,testData)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(FullData)
sapply(FullData, function(x) {sum(is.na(x))})


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gg <- ggplot(trainData, aes(Sex,fill=Survived)) +geom_bar(position = "dodge")+geom_label(stat='count', aes(label=..count..))+labs(x = 'Training data only')
#ggplotly(gg,tooltip = c("x", "fill","count"))
gg
#gga <- ggplot(FullData, aes(Sex,fill=Survived)) +geom_bar(position = "dodge")+labs(x = 'All data')
#ggplotly(gga,tooltip = c("x", "fill","count"))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gg2 <- ggplot(trainData, aes(Pclass,fill=Survived)) +geom_bar(position = "dodge")+geom_label(stat='count', aes(label=..count..))+labs(x = 'Training data only')
gg2



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gg3 <- ggplot(trainData, aes(Age,col=Survived)) +geom_freqpoly(binwidth=2)
ggplotly(gg3)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

gg4 <- ggplot(trainData, aes(Fare,col=Survived)) +geom_freqpoly(binwidth=10)
ggplotly(gg4)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TotalSur<-sum(trainData$Survived==1)
TotalunSur<-sum(trainData$Survived==0)
sum(trainData[trainData$Survived==1,]$Fare>=80)/TotalSur
sum(trainData[trainData$Survived==0,]$Fare>=80)/TotalunSur


## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gg5 <- ggplot(trainData, aes(SibSp,fill=Survived)) +geom_bar(position = "dodge")
ggplotly(gg5)
gg6 <- ggplot(trainData, aes(Parch,fill=Survived)) +geom_bar(position = "dodge")
ggplotly(gg6)
#Fsize
#Family size
FullData$Fsize<-FullData$SibSp+FullData$Parch+1
gg8 <- ggplot(FullData[1:891,],aes(Fsize,fill=Survived)) +geom_bar(position = "dodge")+geom_label(stat='count', aes(label=..count..))
gg8



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[trainData$SibSp=="8",]
testData[testData$Ticket=="CA. 2343",]


## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gg7 <- ggplot(trainData, aes(Embarked,fill=Survived)) +geom_bar(position = "dodge")+geom_label(stat='count', aes(label=..count..))+labs(x = 'Training data only')
gg7


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#資料處理
#新增一欄位Title紀錄乘客稱位
FullData$Title <- gsub('(.*, )|(\\..*)', '', FullData$Name)
#以下為Sex和title次數表
table(FullData$Sex, FullData$Title)
another_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
FullData$Title[FullData$Title == 'Mlle']        <- 'Miss' 
FullData$Title[FullData$Title == 'Ms']          <- 'Miss'
FullData$Title[FullData$Title == 'Mme']         <- 'Mrs' 
FullData$Title[FullData$Title=='Mr' & FullData$Pclass=='1'] <- 'MrP1'
FullData$Title[FullData$Title=='Mr' & FullData$Pclass!='1'] <- 'MrP23'
FullData$Title[FullData$Title %in% another_title]  <- 'Another'
table(FullData$Sex, FullData$Title)



## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gg7 <- ggplot(FullData[1:891,], aes(Title,fill=Survived)) +geom_bar(position = "dodge")
ggplotly(gg7)


## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
h2o.init()



## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

FullData$Survived<-as.factor(FullData$Survived)
FullData$Sex<-as.factor(FullData$Sex)
FullData$Title<-as.factor(FullData$Title)
FullData$Embarked<-as.factor(FullData$Embarked)
data1<-as.h2o(FullData,destination_frame = "FullData")
tempmodel<-h2o.randomForest(x=colnames(FullData)[-(1:2)],y="Age",training_frame = data1)                        
predictdata<-as.data.frame(h2o.predict(tempmodel,data1))
head(predictdata,5)
missAgePassengerId<-FullData[is.na(FullData$Age),]$PassengerId
FullData[missAgePassengerId,]$Age=predictdata[missAgePassengerId,]



## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

tempmodel2<-h2o.randomForest(x=colnames(FullData)[-(1:2)],y="Fare",training_frame = data1)                        
predictdata2<-as.data.frame(h2o.predict(tempmodel2,data1))
missFarePassengerId<-FullData[is.na(FullData$Fare),]$PassengerId
FullData[missFarePassengerId,]$Fare=predictdata2[missFarePassengerId,]



## ----waring=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
FullData[which(is.na(FullData$Embarked)),c( 'Title', 'Survived', 'Pclass', 'Age', 'SibSp', 'Parch', 'Ticket', 'Fare', 'Cabin', 'Embarked') ]




## ----waring=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
FullData[(!is.na(FullData$Embarked) & !is.na(FullData$Fare)),] %>%
  group_by(Embarked, Pclass) %>%
  summarise(Fare=median(Fare))



## ----waring=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

FullData$Embarked[c(62, 830)] <- 'C'



## ---- warning =FALSE,message = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
baseData<-trainData[,c("Survived","Sex","Age","Pclass")]
# 先把資料區分成 train=0.8, test=0.2 
train.index <- sample(x=1:nrow(baseData), size=ceiling(0.8*nrow(baseData) ))
train <- baseData[train.index, ]
test <- baseData[-train.index, ]
cart.model<- rpart(Survived ~. , 
                    data=train)
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)



## ---- warning =FALSE,message = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred <- predict(cart.model, newdata=test, type="class")
# 用table看預測的情況
table(real=test$Survived, predict=pred)
# 計算預測準確率 = 對角線的數量/總數量
confus.matrix <- table(real=test$Survived, predict=pred)



## ---- warning =FALSE,message = FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(diag(confus.matrix))/sum(confus.matrix) # 對角線的數量/總數量



## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
baseData<-FullData[1:891,]
traindatah2o<-as.h2o(baseData,destination_frame = "aa")
#testdatah2o<-as.h2o(data2,destination_frame = "bb")
h20rfmodel1<-h2o.randomForest(x=colnames(traindatah2o)[-1],y="Survived",training_frame =traindatah2o,nfolds=10,keep_cross_validation_predictions=TRUE) 
h2o.varimp_plot(h20rfmodel1)
h20rfmodel1
cat(paste('<b>精準度為',h20rfmodel1@model$cross_validation_metrics_summary[1,1],'</b>'))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
h20rfmodel1@model$cross_validation_metrics_summary[1,1]


## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
h20rfmodel2<-h2o.randomForest(x=colnames(traindatah2o)[-c(1,4,7,8,9,11)],y="Survived",training_frame =traindatah2o,nfolds=10,keep_cross_validation_predictions=TRUE) 
h20rfmodel2@model$cross_validation_metrics_summary[1,1]


## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
xgb<-h2o.xgboost(x =colnames(traindatah2o)[-c(1,4,7,8,9,11)],
                       y ="Survived",
                       training_frame = traindatah2o,
                       model_id = "light_xgb_model_synAI",
                       stopping_rounds = 20,
                       stopping_metric = "logloss",
                       distribution = "AUTO",
                       score_tree_interval = 2,
                       learn_rate = 0.1,
                       ntrees = 50,
                       max_depth = 5,
                       subsample = 0.75,
                       # colsample_bytree = 0.75,
                       tree_method = "auto",
                       grow_policy = "depthwise",
                       booster = "gbtree")
                       
h2o.varimp_plot(xgb)    
xgb@model


## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model1<-h2o.deeplearning(x=colnames(traindatah2o)[-1],y="Survived",training_frame = traindatah2o,
                         activation = "RectifierWithDropout",hidden=c(100),
                         epochs=10,variable_importances = TRUE,nfolds=7,keep_cross_validation_predictions=TRUE)
model1


## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model1@model$cross_validation_metrics_summary[1,1]


## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model2<-h2o.deeplearning(x=colnames(traindatah2o)[-1],y="Survived",training_frame = traindatah2o,
                         activation = "RectifierWithDropout",hidden=c(200,200,200),
                         epochs=10,variable_importances = TRUE,nfolds=7,keep_cross_validation_predictions=TRUE)
model3<-h2o.deeplearning(x=colnames(traindatah2o)[-1],y="Survived",training_frame = traindatah2o,
                         activation = "RectifierWithDropout",hidden=c(200),
                         epochs=50,variable_importances = TRUE,nfolds=7,keep_cross_validation_predictions=TRUE) 
model4<-h2o.deeplearning(x=colnames(traindatah2o)[-1],y="Survived",training_frame = traindatah2o,
                         activation = "RectifierWithDropout",hidden=c(100),
                         epochs=10,l1=0.05,variable_importances = TRUE,nfolds=7,keep_cross_validation_predictions=TRUE)    
model5<-h2o.deeplearning(x=colnames(traindatah2o)[-c(1,4,7,8,9,11)],y="Survived",training_frame = traindatah2o,
                         activation = "RectifierWithDropout",hidden=c(200),
                         epochs=50,variable_importances = TRUE,nfolds=7,keep_cross_validation_predictions=TRUE)                           


## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model2@model$cross_validation_metrics_summary[1,1]


## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model3@model$cross_validation_metrics_summary[1,1]


## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model4@model$cross_validation_metrics_summary[1,1]


## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
model5@model$cross_validation_metrics_summary[1,1]


## ---- warning = FALSE,message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finaltestData<-FullData[892:1309,]
finaltestData<-as.h2o(finaltestData,destination_frame = "finaltestData")
finally<-as.data.frame(predict(model5,finaltestData))
solution <- data.frame(PassengerID = FullData[892:1309,"PassengerId"], Survived = finally$predict)
write.csv(solution, file = 'h2o_mod_Solution.csv', row.names = F)
finally2<-as.data.frame(predict(h20rfmodel2,finaltestData))
solution2 <- data.frame(PassengerID = FullData[892:1309,"PassengerId"], Survived = finally2$predict)
write.csv(solution2, file = 'rf_mod_Solution.csv', row.names = F)
finally3<-as.data.frame(predict(xgb,finaltestData))
solution3 <- data.frame(PassengerID = FullData[892:1309,"PassengerId"], Survived = finally3$predict)
write.csv(solution3, file = 'h2o_mod_xgboost_Solution.csv', row.names = F)

