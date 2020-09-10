rm(list=ls())

training<-read.csv("../input/train.csv",stringsAsFactors = FALSE)
test<-read.csv("../input/test.csv",stringsAsFactors = FALSE)
test$Survived<-rep(NA,nrow(test))
data<-rbind(training,test)




str(data)
summary(data)


data$Embarked <-as.factor(data$Embarked)
data$Pclass <-as.factor(data$Pclass)
data$Sex <-as.factor(data$Sex)
data$Survived <-as.factor(data$Survived)
data$N_Family<-data$SibSp+data$Parch+1
data$individual_price<-data$Fare/data$N_Family

data$individual_price[1044]<-mean(data$individual_price[which(data$Pclass ==3 & is.na(data$Fare)==FALSE)])
data$Embarked[which(data$Embarked=="")]<-"S"

data$Title <- gsub('(.*, )|(\\..*)', '', data$Name)
data$Title[data$Title == 'Ms']          <- 'Miss'

data$Age[data$Title == 'Dr' & is.na(data$Age)==TRUE]<- mean(data$Age[data$Title == 'Dr' & is.na(data$Age)==FALSE])
data$Age[data$Title == 'Master'& is.na(data$Age)==TRUE]<- mean(data$Age[data$Title == 'Master' & is.na(data$Age)==FALSE])
data$Age[data$Title == 'Miss'& is.na(data$Age)==TRUE]<- mean(data$Age[data$Title == 'Miss' & is.na(data$Age)==FALSE])
data$Age[data$Title == 'Mr'& is.na(data$Age)==TRUE]<- mean(data$Age[data$Title == 'Mr' & is.na(data$Age)==FALSE])
data$Age[data$Title == 'Mrs'& is.na(data$Age)==TRUE]<- mean(data$Age[data$Title == 'Mrs' & is.na(data$Age)==FALSE])



tr_data<-data[1:891,]
te_data<-data[892:1309,]


model_logistic=glm(Survived~Pclass+Sex+Age+N_Family,family=binomial(link="logit" ),data = tr_data)
summary(model_logistic)
mean(tr_data$Survived==round(predict(model_logistic,tr_data,type="response")))

te_data$Survived<-round(predict(model_logistic, te_data, type="response"))

write.csv(te_data[,1:2], file = 'Titanic_sol.csv', row.names = F)
