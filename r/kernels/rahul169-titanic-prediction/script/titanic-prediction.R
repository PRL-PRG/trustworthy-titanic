 titanic<-read.csv("../input/train.csv")
 test<-read.csv("../input/test.csv")
is.na(titanic)
colSums(is.na(titanic))
mean(titanic$Age,na.rm=T)
titanic$Age[is.na(titanic$Age)]<-29.70
is.na(test)
colSums(is.na(test))
test$Age[is.na(test$Age)]<-29.70
test$Fare[is.na(test$Fare)]<-32.20
titanic$Survived<-as.factor(titanic$Survived)
library(ggplot2)

ggplot(titanic,aes(x=(Age),fill=factor(Survived)))+geom_bar(position = 'dodge')
ggplot(titanic,aes(x=(Age),fill=(Pclass)))+geom_bar()+facet_grid(".~Survived")
ggplot(titanic,aes(x=(Pclass),fill=(Sex)))+geom_bar()+facet_grid(".~Survived")
ggplot(titanic,aes(x=(Embarked),fill=(Sex)))+geom_bar()+facet_grid(".~Survived")
ggplot(titanic,aes(x=Age,y=Pclass,col=Sex))+geom_jitter(size=3)+facet_grid(".~Survived")

library(randomForest)
tip<-randomForest(Survived~Age+Sex+Pclass+Parch+Fare,data = titanic,ntree=100,importance=TRUE)
tip 

varImpPlot(tip)
new_pd1=predict(tip,test)
new_pd1

PassengerId<-test$PassengerId
Survived<-predict(tip,test)
Survived
prediction<-cbind(PassengerId,Survived)
write.csv(prediction,"survivallist.csv")
