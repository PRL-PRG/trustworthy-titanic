## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t_test <- read.csv("../input/test.csv", header=T)
t_train <- read.csv("../input/train.csv",header=T)
# Rename test string for easier understanding
t_train$Survived_ <- as.factor(with(t_train, 
                                    ifelse(Survived=="1","Survived", 
                                           ifelse(Survived==0,"Died","N/A"))))
#combine the data and check the structure 
t_test[,c("Survived","Survived_")] <- NA
t_full <- rbind(t_train,t_test)  
str(t_full) #check data structure


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(t_train$Survived_))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(t_train$Survived_[which(t_train$Sex=='female')]))



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sapply(t_full, function(x) sum(is.na(x)))
sapply(t_full,function(x) sum((x)=="")) #count variable with blank value
t_full[is.na(t_full$Fare),]#Which passenger missing Fare ?


## ----warning=FALSE, message=FALSE, result='hide'---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2) #visualization
library(gridExtra) # multi-panel plot
library(dplyr) #data manipulation


## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(t_full, aes(factor(Embarked),Fare,fill=factor(Pclass)))+
  geom_boxplot()+ ggtitle("Fare by Embarked and Pclass")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t_full$Fare[is.na(t_full$Fare)] <- median(t_full$Fare[which(t_full$Pclass=="3" & t_full$Embarked=="S")],na.rm=T)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t_full[t_full[,"Embarked"]=='',] 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t_full$Name <- as.character(t_full$Name)
str <- grep("George Nelson",t_full$Name,perl=T)
select_row <- t_full[str,c("Name","Embarked")]
select_row


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
E_F<- with(t_full[t_full$Fare<500,], 
           aggregate(Fare,list(Embarked=Embarked,Pclass=Pclass),
                                                mean,na.rm=T)) #exclude fares higher than 500 to avoid skewed result
E_F 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t_full$Embarked[c(62,830)]="S"
t_full$Embarked <- factor(t_full$Embarked) #drop useless factor levels


## ----warning=FALSE, message=FALSE, result='hide'---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(mice)
library(lattice) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
imp_ <- mice(t_full[,c('Pclass','Sex','SibSp','Parch','Fare','Embarked','Age')],
             m=5, method='pmm', seed=500)  #Impute the missing values
print(imp_) #print the result

#Diagnostic checking: Imputation should be values that close to observation when they are not missing
p1<- xyplot(imp_, Age ~ Pclass,pch = 20, cex = 1.4)
p2<- densityplot(imp_)
grid.arrange(p1,p2,nrow=1,ncol=2)
stripplot(imp_, pch = 20, cex = 1.2)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
imp_output1 <- complete(imp_,1)
imp_output2 <- complete(imp_,2) 
imp_output3 <- complete(imp_,3) 
imp_output4 <- complete(imp_,4) 
imp_output5 <- complete(imp_,5) 
par(mfrow=c(2,3))
hist(t_full$Age,  main = 'Original Age', xlab = "Age",col='blue')
hist(imp_output1$Age,  main = 'Imputed1 Age', xlab = "Age",col='pink')
hist(imp_output2$Age,  main = 'Imputed2 Age', xlab = "Age",col='pink')
hist(imp_output3$Age,  main = 'Imputed3 Age', xlab = "Age",col='pink')
hist(imp_output4$Age,  main = 'Imputed4 Age', xlab = "Age",col='pink')
hist(imp_output5$Age,  main = 'Imputed5 Age', xlab = "Age",col='pink')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t_full$Age <- imp_output2$Age


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
child <- with(t_full, ifelse(Age < 18, "Y","N"))
t_full$Child <- as.factor(child)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Split full name into title, first name and last name
name <- data.frame(do.call(rbind, strsplit(as.vector(t_full$Name), split = "[,.]")))
head(name)
t_full$Title<- sub(' ','',name$X2)

#join the three columns back to original dataset
H_title <- c('Capt','Col','Don','Dona','Dr','Jonkheer','Lady','Major','Rev','Sir','the Countess')
O_title <- c('Master','Miss','Mlle','Mme','Mr','Mrs','Ms','Mrs','Ms')
Title <- with(t_full,
              ifelse(Title %in% H_title,'Honorific_title',
                     ifelse(Title %in% O_title,'Ordinal_title','other')))

#How survival rate differ on title?
t_full$Title <- as.factor(Title)
prop.table(table(t_full$Survived_, t_full$Title),2)


## ----warning=FALSE, message=FALSE, result='hide'---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(reshape2)
library(scales)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Divided data back into training and testing
t_train <- t_full[!is.na(t_full$Survived),]
t_test <- t_full[is.na(t_full$Survived),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
attach(t_train)

#calcute the proportion
Gender_S <- melt(prop.table(table(Sex,Survived_),1))
Child_S <- melt(prop.table(table(Child,Survived_),1))
Title_S <- with(t_train,  melt(prop.table(table(Title,Survived_),1)))
Pclass_S <- melt(prop.table(table(Pclass,Survived_),1))
Embarked_S <- melt(prop.table(table(Embarked,Survived_),1))
SibSp_S <- with(t_train, melt(prop.table(table(SibSp,Survived_),1)))

#Use plot to show the result
p1 <- ggplot(Gender_S)+geom_bar(aes(Sex, value,fill=factor(Survived_)),stat="identity")+
  ggtitle("Gender vs. Survived")+xlab("")+ylab("%")
p2<- ggplot(Child_S)+geom_bar(aes(Child, value,fill=factor(Survived_)),stat="identity")+
  ggtitle("Child vs. Survived")+xlab("")+ylab("%")
p3<- ggplot(Title_S)+geom_bar(aes(Title, value,fill=factor(Survived_)),stat="identity")+
  ggtitle("Title vs. Survived")+xlab("")+ylab("%")
p4<-ggplot(Pclass_S)+geom_bar(aes(Pclass, value,fill=factor(Survived_)),stat="identity")+
  ggtitle("Pclass vs. Survived")+xlab("")+ylab("%")
p5 <-ggplot(Embarked_S)+geom_bar(aes(Embarked, value,fill=factor(Survived_)),stat="identity")+
  ggtitle("Embarked vs. Survived")+xlab("")+ylab("%")
p6 <-ggplot(SibSp_S)+geom_bar(aes(SibSp, value,fill=factor(Survived_)),stat="identity")+
  ggtitle("SibSp vs. Survived")+xlab("")+ylab("%")
p7<- ggplot(t_train, aes(x=Age, fill=factor(Survived_)))+
  geom_histogram(position="dodge",binwidth=1.5) +
  ggtitle ("Age vs. Survived") +xlab("Age")
p8<- ggplot(t_train[Fare<500,], aes(factor(Survived), Fare, fill = factor(Survived_))) +
  geom_boxplot() + ggtitle ("Fare(less than 500) vs. Survived")+xlab("")

grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)
grid.arrange(p5,p6,p7,p8,nrow=2,ncol=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
a<-prop.table(table(Sex,Survived_,Embarked),c(1,3))
# calculate the proportation of survival across gender and embarked
b<- melt(a)#convert values into a molten data frame
b$Freq <-  paste(round(b$value*100,digits=1),"%",sep="") #change the format to percentage
p9<- ggplot(b, aes(x=Embarked, y=value, fill=factor(Survived_))) +  geom_bar(stat="identity") + 
  facet_wrap(~Sex,ncol=2) + ggtitle("Survival on Gender and Embarked")+
  xlab("Embarked") +  ylab("%")
p9

b2<- melt(prop.table(table(Sex,Survived,Pclass),c(1,3)))
b2$Freq <-  paste(round(b2$value*100,digits=1),"%",sep="")
p10<- ggplot(b2, aes(x=Pclass, y=value, fill=factor(Survived))) + 
  geom_bar(stat="identity") +   facet_wrap(~Sex,ncol=2) +
  ggtitle("Survival on Gender and Class")+xlab("Pclass") + ylab("%")
p10


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(100)
dt <- sample(nrow(t_train),nrow(t_train)*0.8) 
t_train_model <- t_train[dt,]
t_train_test <- t_train[-dt,]


## ----message=FALSE,results='hide'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart) #rpart has good ability to handle missing, since we have a lot of missing values
library(rpart.plot)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(231)
tree.train <- rpart(factor(Survived_) ~ 
                      Pclass + Sex + Age + Child+ SibSp + Parch + Fare + Embarked + Title ,
                    data=t_train_model,method="class")
#illustrate the result
par(mfrow=c(1,1))
rpart.plot(tree.train, 
           box.col=c("#FF6666", "#33CCCC")[tree.train$frame$yval],cex=0.8)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tree.train$cptable 
# Normally, we select a tree size that minimizes the cross-validated error, which is 'Xerror'
# Let's automate the selection
tree.prun<- prune(tree.train, cp=tree.train$cptable[which.min(tree.train$cptable[,"xerror"]),"CP"])
# show the pruned tree using following code
#rpart.plot(tree.prun, box.col=c("#FF6666", "#33CCCC")[tree.prun$frame$yval])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pr.yhat = predict(tree.prun,newdata=t_train_test,type="class") 
sum(t_train_test$Survived_==pr.yhat) / nrow(t_train_test) 



## ----message=FALSE,results='hide'------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(122)

rf.train <- randomForest(factor(Survived_) ~ 
                           Pclass + Sex + Age + Child+ SibSp + Parch + Fare + Embarked + Title,
                         data=t_train_model, mtry=3,importance=TRUE) 
plot(rf.train)
legend('topright', colnames(rf.train$err.rate), col=1:3, fill=1:3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
varImpPlot(rf.train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rf.yhat = predict(rf.train,newdata=t_train_test,type="class") 
sum(t_train_test$Survived_==rf.yhat) / nrow(t_train_test) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(221)
bag.train <- randomForest(factor(Survived) ~ 
                            Pclass + Sex + Age + Child+ SibSp + Parch + Fare + Embarked + Title,
                          data=t_train_model, mtry=9,importance=TRUE) 
plot(bag.train)
legend('topright', colnames(rf.train$err.rate), col=1:3, fill=1:3)

varImpPlot(bag.train)#generate the importance of variables


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bag.yhat = predict(bag.train,newdata=t_train_test,type="class") 
sum(t_train_test$Survived==bag.yhat) / nrow(t_train_test) 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(122)
rf.train <- randomForest(factor(Survived) ~ 
                           Pclass + Sex + Age + Child+ SibSp + Parch + Fare + Embarked + Title,
                         data=t_train, mtry=3,importance=TRUE) 

rf.yhat = predict(rf.train,newdata=t_test,type="class") 

# Save the solution to a dataframe
Prediction <- data.frame(PassengerID = t_test$PassengerId, Survived = rf.yhat)

# Output the file
write.csv(Prediction, file = 'rf_prediction.csv', row.names = F)

