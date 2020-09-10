library(dplyr)
train <- read.csv('../input/train.csv')
test  <- read.csv('../input/test.csv')
library(datasets)
test.survived <- data.frame(Survived = rep("None",nrow(test)),test[,])
data.combined <- rbind(train,test.survived)                         
str(data.combined)
data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)
table(data.combined$Survived)
table(data.combined$Pclass)
train$Pclass <- as.factor(train$Pclass)
library(ggplot2)
ggplot(train, aes(x=Pclass, fill= factor(Survived)))+geom_bar() + labs(fill="Survived")
head(as.character(train$Name))
length(unique(as.character(data.combined$Name)))
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])
data.combined[which(data.combined$Name %in% dup.names),]

library(stringr)
misses <- data.combined [which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]
mrses <- data.combined [which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]
males <- data.combined [which(train$Sex=="male"),]
males[1:5,]
extractTitle<- function(Name){
  name <- as.character(Name)
  if (length(grep("Miss.",Name))>0){
    return("Miss.")
  } else if (length(grep("Master.",Name))>0){
    return("Master.")
  } else if (length(grep("Mrs.",Name))>0){
    return("Mrs.")
  } else if (length(grep("Mr.",Name))>0){
    return("Mr.")
  } else {
    return("Other")
  }
}
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"Name"]))
}
data.combined$titles <- as.factor(titles)
ggplot(data.combined[1:891,], aes(x=titles, fill=Survived))+ geom_bar()+
  facet_wrap(~Pclass) + ggtitle("Pclass") +
  labs(fill="Survived")
table(data.combined$Sex)
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])
ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived))+ geom_bar()+
  facet_wrap(~Pclass) + ggtitle("Pclass")+
  labs(fill="Survived")
ggplot(data.combined[1:891,], aes(Age, fill=Survived))+
  geom_bar()+facet_wrap(~Sex + Pclass)
boys <-data.combined[which(data.combined$title=="Master."),]
summary(boys$Age)
misses <-data.combined[which(data.combined$title=="Miss."),]
summary(misses$Age)
ggplot(misses[misses$Survived !="None",], aes(x=Age, fill=Survived))+
  geom_bar(width = 5,
                 position = "stack",
                 inherit.aes = TRUE)+facet_wrap(~Pclass) + xlab("Age")+
  ylab("Total Count")+ggtitle("Age for 'Miss.' by Pclass")
misses.alone <- misses [which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <=14.5))
summary(data.combined$SibSp)
length((data.combined$SibSp))
data.combined$SibSp <- as.factor((data.combined$SibSp))

ggplot(data.combined[1:891,], aes(x=SibSp, fill=Survived))+
  geom_bar(width = 1)+facet_wrap(~Pclass+ title) + xlab("SibSp")+
  ylab("Total Count")+ ggtitle("Pclass, Title")+labs(fill="Survived")

data.combined$Parch <- as.factor((data.combined$Parch))
ggplot(data.combined[1:891,], aes(x=Parch, fill=Survived))+
  geom_bar(width = 1, stat="count", na.rm = FALSE,
                 show.legend = NA, inherit.aes = TRUE)+facet_wrap(~Pclass+ title) + xlab("Parch")+
  ylab("Total Count")+ggtitle("Pclass, Title")+labs(fill="Survived")

temp.SibSp <- c(train$SibSp,test$SibSp)
temp.Parch <- c(train$Parch,test$Parch)
data.combined$family.size<- as.factor(temp.SibSp+temp.Parch+1)
ggplot(data.combined[1:891,], aes(x=family.size, fill=Survived))+
  geom_bar(width = 1)+facet_wrap(~Pclass+ title) + xlab("family.size")+
  ylab("Total Count")+ ylim(0,300)+ggtitle("Pclass, Title")+labs(fill="Survived")
str(data.combined$Ticket)
data.combined$Ticket[,] <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]
Ticket.first.char <- ifelse(data.combined$Ticket==""," ",substr
                            (data.combined$Ticket,1,1))
unique(data.combined$Ticket)
data.combined$Ticket.first.char <- as.factor(data.combined$Ticket.first.char)
ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill=Survived))+
  geom_bar()+ggtitle("Survilability by Ticket.first.char") +
  xlab("Ticket.first.char") +
  ylab("Total Count")+ ylim(0,350) +
  labs(fill="Survived")
ggplot(data.combined[1:891,], aes(x=Ticket.first.char, fill=Survived))+
  geom_bar()+ggtitle("Pclass, Title")+facet_wrap(~Pclass+ title)
  + xlab("Ticket.first.char")+
  ylab("Total Count")+ ylim(0,200)+labs(fill="Survived")
summary(data.combined$Fare)
length(unique(data.combined$Fare))

ggplot(data.combined, aes(x=Fare))+
  geom_histogram(binwidth = 5)+ggtitle("Combined Fare Distribution")
  + xlab("Fare")+ylab("Total Count")+ ylim(0,200)

ggplot(data.combined[1:891,], aes(x=Fare, fill=Survived))+
  geom_histogram(binwidth = 5)+ggtitle("Pclass, Title")+facet_wrap(~Pclass+ title)
  + xlab("Fare")+
  ylab("Total Count")+ ylim(0,50)+labs(fill="Survived")
str(data.combined$Cabin)
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:180]

data.combined[which(data.combined$Cabin==""),"Cabin"] <- "U"
data.combined$Cabin[1:100]
Cabin.first.char<- as.factor(substr(data.combined$Cabin,1,1))
str(Cabin.first.char)
levels(Cabin.first.char)
data.combined$Cabin.first.char<-Cabin.first.char

ggplot(data.combined[1:891,], aes(x=Cabin.first.char, fill=Survived))+
  geom_bar()+ggtitle("Survilability by Cabin.first.char")
+ xlab("Cabin.first.char")+
  ylab("Total Count")+labs(fill="Survived")

ggplot(data.combined[1:891,], aes(x=Cabin.first.char, fill=Survived))+
  geom_bar()+ggtitle("Survilability by Cabin.first.char")+facet_wrap(~Pclass)
+ xlab("Pclass")+
  ylab("Total Count")+labs(fill="Survived")

ggplot(data.combined[1:891,], aes(x=Cabin.first.char, fill=Survived))
 +geom_bar()+ggtitle("Pclass, Title")
 +facet_wrap(~Pclass+title)
 +xlab("Cabin.first.char")+ylab("Total Count")+labs(fill="Survived")

data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin," "),"Y","N"))

ggplot(data.combined[1:891,], aes(x=Cabin.multiple, fill=Survived))+
  geom_bar()+ggtitle("Pclass, Title")+facet_wrap(~Pclass+title)
+ xlab("Cabin.multiple")+
  ylab("Total Count")+labs(fill="Survived")

str(data.combined$Embarked)
levels(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x=Embarked, fill=Survived))+
  geom_bar()+ggtitle("Pclass, Title")+facet_wrap(~Pclass+title)
+ xlab("Embarked")+
  ylab("Total Count")+ ylim(0,300)+labs(fill="Survived")

library(randomForest)

rf.train.1 <- data.combined[1:891,c("Pclass","title")]
rf.label<- as.factor(train$Survived)
set.seed(1234)
rf.1 <- randomForest(x=rf.train.1,y=rf.label,importance = TRUE)
rf.1
varImpPlot(rf.1)

rf.train.2 <- data.combined[1:891,c("Pclass","title","SibSp")]
set.seed(1234)
rf.2 <- randomForest(x=rf.train.2,y=rf.label,importance = TRUE)
rf.2
varImpPlot(rf.2)

rf.train.3 <- data.combined[1:891,c("Pclass","title","Parch")]
set.seed(1234)
rf.3 <- randomForest(x=rf.train.3,y=rf.label,importance = TRUE)
rf.3
varImpPlot(rf.3)

rf.train.4 <- data.combined[1:891,c("Pclass","title","SibSp","Parch")]
set.seed(1234)
rf.4 <- randomForest(x=rf.train.4,y=rf.label,importance = TRUE)
rf.4
varImpPlot(rf.4)

rf.train.5 <- data.combined[1:891,c("Pclass","title","family.size")]
set.seed(1234)
rf.5 <- randomForest(x=rf.train.5,y=rf.label,importance = TRUE)
rf.5
varImpPlot(rf.5)

rf.train.6 <- data.combined[1:891,c("Pclass","title","SibSp","family.size")]
set.seed(1234)
rf.6 <- randomForest(x=rf.train.6,y=rf.label,importance = TRUE)
rf.6
varImpPlot(rf.6)

rf.train.7 <- data.combined[1:891,c("Pclass","title","Parch","family.size")]
set.seed(1234)
rf.7 <- randomForest(x=rf.train.7,y=rf.label,importance = TRUE)
rf.7
varImpPlot(rf.7)
#my predictive model is rf.5