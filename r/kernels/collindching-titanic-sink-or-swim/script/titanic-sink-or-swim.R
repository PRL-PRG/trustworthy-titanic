## ----load,message=FALSE,warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(kableExtra)
library(knitr)
library(rpart)
library(VIM)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)

str(train)
str(test)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test$Survived <- NA
merged <- rbind(train,test)
str(merged)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(merged)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged$Fare[merged$Fare==""] <- NA
merged$Cabin[merged$Cabin==""] <- NA
merged$Embarked[merged$Embarked==""] <- NA


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
aggr(merged,prop=F,numbers=T,col=c("lightblue","indianred3"),sortVars=T,sortCombs=T)


## ----warning=FALSE,message=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
stdtheme <- theme(plot.title=element_text(hjust=.5,size=12,face="bold"),legend.title=element_blank())

p.sex <- ggplot(data=train,mapping=aes(x=Sex,fill=factor(Survived,labels=c("Died","Survived")))) + 
  geom_bar(position="fill") + 
  labs(title="A. Sex and Survival") +
  stdtheme 

meanfare_0 <- mean(train$Fare[train$Survived==0])
meanfare_1 <- mean(train$Fare[train$Survived==1])
p.fare <- ggplot(data=train,mapping=aes(x=Fare,color=factor(Survived))) +
  geom_freqpoly() +
  labs(title="C. Fare and Survival") +
  geom_vline(xintercept=meanfare_0,color="red") +
  geom_vline(xintercept=meanfare_1,color="blue") +
  xlim(0,200) +
  stdtheme +
  guides(color=FALSE)

meanage_0 <- mean(train$Age[train$Survived==0],na.rm=TRUE)
meanage_1 <- mean(train$Age[train$Survived==1],na.rm=TRUE)
p.age <- ggplot(data=train,aes(x=Age,fill=factor(Survived))) +
  geom_histogram(binwidth=10) +
  labs(title="B. Age and Survival, Binwidth 10") +
  geom_vline(xintercept=meanage_0,color="red") +
  geom_vline(xintercept=meanage_1,color="blue") +
  scale_x_continuous(breaks=seq(0,100,10)) +
  stdtheme +
  guides(fill=FALSE)

layout = matrix(c(1,2,3,3),2,2,byrow=TRUE)
grid.arrange(p.sex,p.age,p.fare,layout_matrix=layout)


## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p2.age <- ggplot(train,mapping=aes(x=Age,fill=factor(Survived))) +
  geom_histogram(binwidth=5,position="fill") +
  guides(fill=FALSE) +
  labs(title="Survival rate by age, binwidth=5",y="Proportion") +
  scale_x_continuous(breaks=seq(0,100,10)) +
  stdtheme

p2.fare <- ggplot(train,mapping=aes(x=Fare,fill=factor(Survived))) +
  geom_histogram(binwidth=10,position="fill") +
  labs(title="Survival rate by fare, binwidth=10",y="Proportion") +
  scale_x_continuous(breaks=seq(0,550,50)) +
  stdtheme +
  guides(fill=FALSE)

# separate cabins
merged$CabinGr <- merged$Cabin %>% as.character() %>% substr(1,1)
train <- merged[1:891,]

p2.cabgr1 <- ggplot(data=subset(train,!is.na(CabinGr)),aes(x=CabinGr)) +
  geom_bar() +
  labs(title="Num passengers per cabin group",y="Count") +
  stdtheme

p2.cabgr2 <- ggplot(data=subset(train,!is.na(CabinGr)),aes(x=CabinGr,fill=factor(Survived))) +
  geom_bar(position="fill") +
  labs(title="Survival rate by cabin group",x="CabinGr",y="Proportion") +
  guides(fill=FALSE) +
  stdtheme

p.embarked <- ggplot(subset(train,!is.na(Embarked)),mapping=aes(x=Embarked,fill=factor(Survived,labels=c("Died","Survived")))) +
  geom_bar(position="fill") +
  labs(title="Port and Survival",y="Proportion",x="Port") +
  stdtheme  

grid.arrange(p2.age,p2.fare,p2.cabgr1,p2.cabgr2,p.embarked,layout_matrix=matrix(c(1,5,2,2,3,4),3,2,byrow=TRUE))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged$AgeBucket[!is.na(merged$Age)] <- "0-20"
merged$AgeBucket[merged$Age>20 & merged$Age<=40] <- "20-40" 
merged$AgeBucket[merged$Age>40 & merged$Age<=60] <- "40-60" 
merged$AgeBucket[merged$Age>60] <- "60+"

age.sex_surv <- aggregate(Survived ~ AgeBucket + Sex,
          data=merged,
          FUN=function(x) {sum(x)/length(x)})

kable(age.sex_surv[order(-age.sex_surv$Survived),],"html",row.names=FALSE) %>% 
  kable_styling(full_width=FALSE)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
names <- as.character(merged$Name)
titles <- strsplit(names,"[,.]") %>% sapply(FUN=function(x) x[2])
titles <- gsub("^ | $","",titles)
table(titles)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged$Title = "Mr"
merged$Title[titles %in% c("Dona","Lady","Mme","Mrs","the Countess")] <- "Mrs"
merged$Title[titles %in% c("Miss","Mlle","Ms")] <- "Ms"
merged$Title <- factor(merged$Title)

prop.table(table(merged$Title,merged$Survived),1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged$nFam <- merged$SibSp + merged$Parch

ggplot(data=merged[!is.na(merged$Survived),],aes(x=nFam,fill=factor(Survived))) +
  geom_bar(position="fill") +
  stdtheme +
  labs(title="Survival by num family members",x="Family members onboard",y="Survival rate") +
  scale_y_continuous(breaks=seq(0,1,.1)) +
  scale_x_continuous(breaks=0:10)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged[is.na(merged$Fare),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(merged[!is.na(merged$Fare) & merged$Sex=="male" & merged$Pclass==3,],aes(x=Age,y=Fare)) +
  geom_point() +
  geom_smooth()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
missfare <- merged[merged$Pclass==3 & merged$Age>=50 & merged$Sex=="male",]$Fare %>% median(na.rm=TRUE)

merged$Fare[is.na(merged$Fare)] <- missfare


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged[is.na(merged$Embarked),]
merged$Embarked[62] <- "S"
merged$Embarked[830] <- "S"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title,
                data=merged[!is.na(merged$Age),],
                method="anova")
merged$Age[is.na(merged$Age)] <- predict(agefit,merged[is.na(merged$Age),])


## ----eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## train <- merged[1:891,]
## test <- merged[892:1309,]
## 
## sfit <- rpart(Survived ~ Pclass + Name + Sex + SibSp + Parch + Fare + Embarked + AgeBucket + Title + nFam,
##               train,
##               method="class",
##               control=rpart.control(minsplit=5))
## 
## prediction <- predict(sfit,test,type="class")
## 
## submit <- data.frame(PassengerId=test$PassengerId,Survived=prediction)
## 
## write.csv(submit,file="rpart.csv",row.names=FALSE)

