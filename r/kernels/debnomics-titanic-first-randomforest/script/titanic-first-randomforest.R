project<-"titanic"
train<-read.csv("../input/train.csv", stringsAsFactors = FALSE)
test<-read.csv("../input/test.csv", stringsAsFactors = FALSE)

library(ggplot2)
library(randomForest)

test$Survived<-99
comb<-rbind(train,test)

#Feature Engineering - do the analysis in a different place and copy the final result here
fea<-function(data) {
  comb_1<-data
  comb_1$Age[which(comb_1$Age<1)]<-comb_1$Age[which(comb_1$Age<1)]*100
  comb_1$Age[is.na(comb_1$Age)] <- median(comb_1$Age, na.rm=TRUE)
  comb_1$Fare[is.na(comb_1$Fare)] <- median(comb_1$Fare, na.rm=TRUE)
  comb_1$title<-"Unknown"
  comb_1$title[which(regexpr("Mr.",comb_1$Name)>0)]<-"Mr"
  comb_1$title[which(regexpr("Mrs.",comb_1$Name)>0)]<-"Mrs"
  comb_1$title[which(regexpr("Miss.",comb_1$Name)>0)]<-"Miss"
  comb_1$title[which(regexpr("Ms.",comb_1$Name)>0)]<-"Miss"
  comb_1$title[which(regexpr("Master.",comb_1$Name)>0)]<-"Master"
  comb_1$title[which(regexpr("Dr.",comb_1$Name)>0)]<-"Dr"
  comb_1$title[which(regexpr("Rev.",comb_1$Name)>0)]<-"Rev"
  comb_1$title[which(regexpr("Col.",comb_1$Name)>0)]<-"Col"
  comb_1$title[which(regexpr("Major.",comb_1$Name)>0)]<-"Col"
  comb_1$Name<-NULL
  comb_1$Cabin_Code<-substr(comb_1$Cabin,1,1)
  comb_1$Cabin_Code[which(comb_1$Cabin_Code=="T")]<-"A"
  comb_1$Cabin_Code[which(comb_1$Cabin_Code=="")]<-"A"
  comb_1$Cabin<-NULL
  comb_1$Ticket<-NULL
  comb_1$PassengerId<-NULL
  comb_1$Embarked[which(comb_1$Embarked=="")]<-"S"
  comb_1$Parch[which(comb_1$Parch==9)]<-6

  comb_1$Embarked<-as.factor(comb_1$Embarked)
  comb_1$Cabin_Code<-as.factor(comb_1$Cabin_Code)
  comb_1$title<-as.factor(comb_1$title)
  comb_1$Sex<-as.factor(comb_1$Sex)
  
  comb_1$Survived<-NULL
  
  comb_1$Embarked<-NULL
  comb_1$Parch<-NULL
  return(comb_1)
}

#lapply (test,function(x) unique(x[order(x)]))

#cross validation data setup

#cross validation train model

#GLM Model

#Regression Tree Model

#Random Forest Model
rf <- randomForest(fea(train), as.factor(train$Survived), ntree=100, importance=TRUE)

imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)
#GBM Model

#cross validation test model

#GLM Model Fit

#Regression Tree Model Fit

#Random Forest Model Fit
rf_fit <- predict(rf, fea(test))

#GBM Model Fit


#Cross validation Performance measure

#correlation between output




#submission entry
#submission_id<-"randomForest"
#submission <- data.frame(PassengerId = test$PassengerId)
#submission$Survived <- rf_fit
#time<-sub(" ","",as.character(Sys.time(), "%b%Y%d"))
#file_name<-paste(project,time,submission_id,".csv")
#write.csv(submission, file = file_name, row.names=FALSE)

