rm(list=ls())

library(fastDummies)
library(scales)
library(tidyverse)
library(keras)

#setwd("D:/R/titanic")
#titanic<-read.csv("train.csv")
#test<-read.csv("test.csv")


titanic<-read.csv("../input/train.csv")
test<-read.csv("../input/test.csv")
colnames(titanic)
colnames(test)


trn <- titanic[,-c(2)]
str(trn)
tst <- test

colnames(trn)
colnames(tst)

all<-rbind(trn,tst)
str(all)


tmp<-all %>% 
  separate(Name,
           sep="([\\,\\.])",
           into = c("Last","Title"),
           remove = TRUE) 
summary(as.factor(tmp$Last))
summary(as.factor(tmp$Title))
tmp$Pclass<-as.factor(tmp$Pclass)
tmp$Last<-as.factor(tmp$Last)
tmp$Title<-as.factor(tmp$Title)
str(tmp)

str(tmp)
summary(tmp$Last)[1]
tmp<-tmp[,-3]
summary(tmp$Title)

levels(tmp$Title)[c(1,2,3,6,8,17)]<-" Mr"
summary(tmp$Title)
levels(tmp$Title)[c(2,4,7,8,10,12)]<-" Miss"
summary(tmp$Title)
str(tmp)

summary(tmp$Ticket)
tmp.t<-tmp
summary(tmp.t)

tmp.t.top<-tmp.t[which(tmp.t$Ticket=="CA. 2343" | tmp.t$Ticket=="1601" | tmp.t$Ticket=="CA 2144" | tmp.t$Ticket=="3101295" | tmp.t$Ticket=="347077" | tmp.t$Ticket=="347082" | tmp.t$Ticket=="PC 17608" | tmp.t$Ticket=="S.O.C. 14879"),]
tmp.t.rest<-tmp.t[which(tmp.t$Ticket!="CA. 2343" & tmp.t$Ticket!="1601" & tmp.t$Ticket!="CA 2144" & tmp.t$Ticket!="3101295" & tmp.t$Ticket!="347077" & tmp.t$Ticket!="347082" & tmp.t$Ticket!="PC 17608" & tmp.t$Ticket!="S.O.C. 14879"),]
summary(tmp.t.rest$Ticket)

tmp.t.rest$Ticket<-as.character(tmp.t.rest$Ticket)
t<-sub("^([[:alpha:]]*).*", "\\1", tmp.t.rest$Ticket)
summary(as.factor(t))
t<-as.factor(t)
levels(t)[1]<-"Numeric"
levels(t)[c(3,5,7,8,9,10,12,15,16,19,21)]<-"Others"

tmp.t.rest$Ticket<-t
str(tmp.t.top)
tmp.t<-rbind(tmp.t.top,tmp.t.rest)
tmp.t$Ticket<-droplevels(tmp.t$Ticket)
str(tmp.t)
tmp<-tmp.t
u<-sub("^([[:alpha:]]*).*", "\\1", tmp$Cabin)
summary(as.factor(u))
u<-as.factor(u)
levels(u)[1]<-"Num"
tmp$Cabin<-u
str(tmp)


tmp.fare<-tmp[which(!is.na(tmp$Fare)),]
tmp.na.fare<-tmp[which(is.na(tmp$Fare)),]

tmp.age<-tmp.fare[which(!is.na(tmp.fare$Age)),]
tmp.na.age<-tmp.fare[which(is.na(tmp.fare$Age)),]
colnames(tmp.age)
reg<-lm(Age~., data = tmp.age[,c(2,3,5,6,8,10,11)])
summary(reg)
pred<-predict(reg, newdata = tmp.na.age)
tmp.na.age$Age<-pred
tmp.fare<-rbind(tmp.age,tmp.na.age)

sum(is.na(tmp.fare))
colnames(tmp.fare)
reg<-lm(Fare~., data = tmp.fare[,c(2,6,7,8,9,10)])
summary(reg)
pred<-predict(reg, newdata = tmp.na.fare)
tmp.na.fare$Fare<-pred
colnames(tmp.fare)
colnames(tmp.na.fare)
tmp<-rbind(tmp.na.fare,tmp.fare)

all<-tmp
str(all)
tmp<-dummy_cols(all)
str(tmp)
tmp<-tmp[,-c(2,3,4,8,10,11)]
str(tmp)
all<-tmp

tmp.all<-all


sum(is.na(tmp.all))
tmp<-tmp.all
test.id<-data.frame(PassengerId=test$PassengerId)
str(test.id)
str(tmp)
test.id$PassengerId<-as.character(test.id$PassengerId)
tmp$PassengerId<-as.character(tmp$PassengerId)
titanic$PassengerId<-as.character(titanic$PassengerId)

n.test<-inner_join(tmp,test.id,by="PassengerId")
n.titanic<-anti_join(tmp,test.id,by="PassengerId")
S<-data.frame(PassengerId=titanic$PassengerId,
              Survived=titanic$Survived)
n.titanic<-inner_join(n.titanic,S,by="PassengerId")

mean <-apply(n.titanic[,-c(1,49)], 2, mean) 
std <-apply(n.titanic[,-c(1,49)], 2, sd)
n.tit <-scale(n.titanic[,-c(1,49)], center = mean, scale = std) 
n.tst <-scale(n.test[,-1], center = mean, scale = std)
n.tit<-cbind(n.tit,n.titanic$PassengerId)
n.tit<-cbind(n.tit,n.titanic$Survived)
n.tst<-cbind(n.tst,n.test$PassengerId)

data<-n.titanic
set.seed(18)
train <- sample(nrow(data), .8*nrow(data), replace = FALSE)
d.train <- data[train,]
d.test <- data[-train,]
colnames(d.train)[1]
x.train<-as.matrix(d.train[,-c(1,49)])
y.train<-d.train[,49]

x.test<-as.matrix(d.test[,-c(1,49)])
y.test<-d.test[,49]

y.train<-as.numeric(y.train)
y.test<-as.numeric(y.test)

x.test<-array_reshape(x.test,c(179,47))
x.train<-array_reshape(x.train,c(712,47))


model <-keras_model_sequential() %>%
  layer_dense(units = 32, activation = "relu", input_shape = c(47)) %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 1, activation = "sigmoid")
  
  
model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

history <-model%>% fit(
  x.train,
  y.train,
  epochs = 200,
  batch_size = 16,
  validation_split=0.2
)
plot(history)
pred<-model %>% predict(x.test)
table(y.test, pred>0.5)

nn.test<-as.matrix(n.test[,-c(1)])
pred<-model %>% predict(nn.test)
result<-if_else(pred>0.5,1,0)

sub<-data.frame(PassengerId=n.test$PassengerId, Survived=result, row.names=NULL)


head(sub)
str(sub)
write.csv(sub,"sub.csv",row.names=FALSE)