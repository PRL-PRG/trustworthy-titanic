## load the data and check the structure of data
titanic<-read.csv("../input/train.csv")
str(titanic)
test_data<-read.csv("../input/test.csv")

## check the missing values and replace missing value in Age with median of Age column
sum(is.na(titanic))
titanic$Age=ifelse(is.na(titanic$Age),median(titanic$Age,na.rm=T),titanic$Age)

## apply glm
log_titanic<-glm(Survived~Pclass+Sex+Age+SibSp,data = titanic,family = binomial)
summary(log_titanic)

## predict probability from model
predict<-predict(log_titanic,type = 'response')
summary(predict)

##creation of confusion matrix
table(titanic$Survived,predict>0.5)
table(titanic$Survived,predict>0.7)
table(titanic$Survived,predict>0.3)

## draw ROC curve with threshold values  
library(ROCR)
pred<-prediction(predict,titanic$Survived)
pref<-performance(pred,"tpr","fpr")
plot(pref,colorize = TRUE,print.cutoffs.at = seq(0,1,0.1),text.adj= c(-0.2,1.7))

## prediction with test data
sum(is.na(test_data$Age))
test_data$Age=ifelse(is.na(test_data$Age),median(test_data$Age,na.rm=T),test_data$Age)
pred_test = predict(log_titanic, type = 'response',newdata = test_data)
summary(pred_test)

## converting probability more than 0.5 in 1(survived) else 0
pred_num_test <- ifelse(pred_test > 0.5, 1, 0)
pred_fact <- factor(pred_num_test, levels=c(0, 1))
test_data$survived<-pred_fact







