## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(ggthemes)
library(stringr)
library(knitr)
library(gridExtra)
library(randomForest)
library(caret)
library(Hmisc)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <-read.csv("../input/train.csv")
test <-read.csv("../input/test.csv")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test$Survived=NA
all=rbind(train, test)
all=transform(all, Survived=as.factor(Survived), Pclass=as.factor(Pclass))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sapply(all, function(x) {sum(is.na(x))})


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(all[!is.na(all$Survived),], aes(x=Survived, fill=Survived)) + geom_bar(position="dodge") +
    geom_label(aes(label=scales::percent(..count../sum(..count..))),
              stat='count',position = position_dodge(0.9), fontface = "bold")+theme_stata()+
  scale_fill_brewer(palette="Pastel1")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1=ggplot(all, aes(x=Sex, fill=Sex)) + geom_bar(position="dodge") +
    geom_label(aes(label=scales::percent(..count../sum(..count..))), stat='count') +
    ggtitle("All Data")+
    theme_stata()+scale_fill_brewer(palette="Pastel1")

p2=ggplot(all[!is.na(all$Survived),], aes(x=Sex, fill=Survived)) + geom_bar(position="dodge") +
    geom_label(aes(label=..count..), stat='count') +
    ggtitle("Training Data Only")+
    theme_stata() + scale_fill_brewer(palette="Pastel1")
              
grid.arrange(p1,p2, nrow=1)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(all[!is.na(all$Survived),], aes(x=Pclass, fill=Pclass)) + geom_bar(position="dodge") +
    geom_label(aes(label=scales::percent(..count../sum(..count..))), stat='count') +
  ggtitle("All Data")+
    theme_stata() + scale_fill_brewer(palette="Pastel1")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p4=ggplot(all[!is.na(all$Survived),], aes(x=Pclass, fill=Survived)) + geom_bar(position="dodge") +
  geom_text(aes(label=scales::percent(..count../sum(..count..))), stat='count', position = position_dodge(0.9), vjust = 0) +
    theme_stata() + scale_fill_brewer(palette="Pastel1")

p5=ggplot(all[!is.na(all$Survived),], aes(x=Pclass, fill=Survived)) + geom_bar(position="stack") +
facet_grid(.~Sex)+ geom_text(aes(label=..count..), stat="count", position= position_stack(vjust = 0.5))+
    theme_stata() + scale_fill_brewer(palette="Pastel1")

p6=ggplot(all[!is.na(all$Survived),], aes(x=Pclass, fill=Survived)) + geom_bar(position="dodge") +
facet_grid(.~Sex)+
    theme_stata() + scale_fill_brewer(palette="Pastel1")
 
p7=ggplot(all[!is.na(all$Survived),], aes(x=Pclass, fill=Survived)) + geom_bar(position="fill") +
facet_grid(.~Sex) + 
  theme_stata() + scale_fill_brewer(palette="Pastel1")



grid.arrange( p4, p5, ncol=2) 



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$SexClass <- ifelse(all$Sex=="male" & all$Pclass==1, "P1Male", 
                       ifelse(all$Sex=="male" & all$Pclass==2, "P2Male", 
                              ifelse(all$Sex=="male" & all$Pclass==3, "P3Male", 
                                     ifelse(all$Sex=="female" & all$Pclass==1, "P1Female", 
                                            ifelse(all$Sex=="female" & all$Pclass==2, "P2Female", 
                                                   ifelse(all$Sex=="female" & all$Pclass==3, "P3Female", ""))))))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(all$Name)
all$title=str_extract(all$Name, pattern="[:upper:]\\w+\\.")
kable(table(all$Sex, all$title))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$title=ifelse(all$title %in% c("Mlle.", "Ms."), "Miss.", all$title)
all$title=ifelse(all$title=="Mme.", "Mrs.", all$title)
all$title=ifelse(!(all$title %in% c("Miss.", "Mrs.", "Mr.", "Master.")), "Rare", all$title)
kable(table(all$Sex, all$title))


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p6=ggplot(all[(!is.na(all$Survived)),], aes(x=title, fill=Survived)) + geom_bar(position="stack") + geom_text(aes(label=..count..), stat="count", position= position_stack(vjust = 0.5))+
    theme_stata()+ 
    scale_fill_brewer(palette="Pastel1")

p7=ggplot(all[(!is.na(all$Survived)),], aes(x=title, y=Age, fill=Sex))+geom_boxplot()+
  theme_stata()+ scale_fill_brewer(palette="Pastel1")

grid.arrange(p6, p7, ncol=2)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kids <- all %>% filter(Age<=18) %>% mutate(nanny=ifelse(Parch==0, 1, 0))
ggplot(kids[(!is.na(kids$Survived)),], aes(x=nanny, fill=Survived)) + geom_bar(position="stack") + geom_text(aes(label=..count..), stat="count", position= position_stack(vjust = 0.5))+
    theme_stata()+ 
    scale_fill_brewer(palette="Pastel1")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$famSize=all$SibSp+all$Parch
ggplot(all[(!is.na(all$Survived)),], aes(x=famSize, fill=Survived)) + 
    geom_bar(stat="count")+
    theme_stata()+ 
    scale_fill_brewer(palette="Pastel1")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all[(all$famSize==7), ]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all[(all$Ticket==113503), c("Name", "Age", "Ticket", "Fare", "famSize")]

all[(all$Ticket==110152), c("Name", "Age", "Ticket", "Fare", "famSize")]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all <- all %>% group_by(Ticket, Fare) %>% mutate(n=n()) %>% mutate(TSize=n-1)

ggplot(all[(!is.na(all$Survived)),], aes(x=TSize, fill=Survived)) + 
    geom_bar(stat="count")+
    theme_stata()+ 
    scale_fill_brewer(palette="Pastel1")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hist(all$TSize-all$famSize)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all <- all %>% mutate(groupSize=max(TSize, famSize)) %>%
  mutate(group=ifelse(groupSize==0, "solo", ifelse(groupSize==1 | groupSize==2, "duo", ifelse(groupSize==3 | groupSize==4, "small group", ifelse(groupSize>=5, "big group", " "))))) %>% 
  transform(group=factor(group, levels=c("solo", "duo", "small group", "big group")))

ggplot(all[!is.na(all$Survived),], aes(x=group, fill=Survived)) +geom_bar(stat="count", position="dodge")+
    geom_label(aes(label=..count..), stat='count')+
      theme_stata() + scale_fill_brewer(palette="Pastel1")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$FarePP=ifelse(all$TSize>0, all$Fare/all$TSize, all$Fare)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
FareMissing <- all[!is.na(all$Fare),] %>% group_by(Pclass) %>%summarise(FareMed=median(FarePP))
all <- left_join(all, FareMissing, by=c("Pclass"))

all[is.na(all$Fare),c("Name", "Pclass", "Embarked", "Fare", "FarePP")]

FareMissing[FareMissing$Pclass==3,]



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$FarePP[all$PassengerId==1044] <- 7.8542


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

all$FarePP[which(all$Fare==0)]=all$FareMed[which(all$FarePP==0)]



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(all[!is.na(all$Survived),], aes(x=FarePP, fill="sienna2")) +
  geom_histogram(bins=30) + guides(fill=FALSE) +theme_stata() 


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$FareBin=cut2(all$FarePP, g=5)

ggplot(all[!is.na(all$Survived),], aes(x=FareBin, fill=Survived)) +geom_bar(stat="count") + facet_grid(.~Pclass)+
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_fill_brewer(palette="Pastel1")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(all[!is.na(all$Survived) & !is.na(all$Age),], aes(x = Age)) + geom_density(aes(fill = Survived), alpha=0.7)+theme_stata()+  scale_fill_brewer(palette="Pastel1")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(all[!is.na(all$Age),], aes(x = title, y = Age, fill=Pclass )) +
  geom_boxplot() + scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_stata() + scale_fill_brewer(palette="Pastel1")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
age_lm=lm(Age~Pclass+title+groupSize, data=all[!is.na(all$Age),])

summary(age_lm)

all$AgePred=predict(age_lm, all)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p8=ggplot(all[!is.na(all$Age),], aes(x=Age, fill="sienna2")) +
  geom_histogram(bins=30) + guides(fill=FALSE) +theme_stata()
p9=ggplot(all[!is.na(all$Age),], aes(x=AgePred, fill="sienna2")) +
  geom_histogram(bins=30) + guides(fill=FALSE) +theme_stata()
p10=ggplot(all[!is.na(all$Age),], aes(x=Age-AgePred)) +
  geom_histogram(bins=30) + guides(fill=FALSE) +theme_stata()
grid.arrange(p8, p9, p10, ncol=3)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Using mean
AgeMeanMed=all[!is.na(all$Age),] %>% group_by(Pclass, title, groupSize) %>% mutate(AgePred=mean(Age))

p11=ggplot(AgeMeanMed[!is.na(AgeMeanMed$Age),], aes(x=Age, fill="sienna2")) +
  geom_histogram(bins=30) + guides(fill=FALSE) +theme_stata()
p12=ggplot(AgeMeanMed[!is.na(AgeMeanMed$Age),], aes(x=AgePred, fill="sienna2")) +
  geom_histogram(bins=30) + guides(fill=FALSE) +theme_stata()
p13=ggplot(AgeMeanMed[!is.na(AgeMeanMed$Age),], aes(x=Age-AgePred)) +
  geom_histogram(bins=30) + guides(fill=FALSE) +theme_stata()
grid.arrange(p11, p12, p13, ncol=3)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Using mean
AgeMeanMed=all[!is.na(all$Age),] %>% group_by(Pclass, title, groupSize) %>% mutate(AgePred=median(Age))

p14=ggplot(AgeMeanMed[!is.na(AgeMeanMed$Age),], aes(x=Age, fill="sienna2")) +
  geom_histogram(bins=30) + guides(fill=FALSE) +theme_stata()
p15=ggplot(AgeMeanMed[!is.na(AgeMeanMed$Age),], aes(x=AgePred, fill="sienna2")) +
  geom_histogram(bins=30) + guides(fill=FALSE) +theme_stata()
p16=ggplot(AgeMeanMed[!is.na(AgeMeanMed$Age),], aes(x=Age-AgePred)) +
  geom_histogram(bins=30) + guides(fill=FALSE) +theme_stata()
grid.arrange(p14, p15, p16, ncol=3)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$Age[is.na(all$Age)]=all$AgePred[is.na(all$Age)]



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
all$SexClass=as.factor(all$SexClass)
#Separating back into train and test
trainFix=all[!is.na(all$Survived)& !is.na(all$Age),]
testFix=all[is.na(all$Survived),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
LR_model <- glm(Survived~SexClass+ Age +FareBin+group,family=binomial(link='logit'),data=trainFix)
pred_train <-predict(LR_model, newdata=trainFix, type="response")
pred=ifelse(pred_train<.5, 0, 1)
mean(pred==trainFix$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(1001)
svm_model <- train(Survived~SexClass+ Age +FarePP+group, data=trainFix, 'svmRadial', 
preProcess= c('center', 'scale'), trControl=trainControl(method="cv", number=5))
svm_model

pred_train <-predict(svm_model,trainFix)
mean(pred_train==trainFix$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RF_model=train(x=trainFix[,c('SexClass', 'Age', 'FarePP', 'group')], y=trainFix$Survived, data=trainFix, method='rf', trControl=trainControl(method="cv", number=5))
RF_model

pred_train <-predict(RF_model,trainFix)
mean(pred_train==trainFix$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RF_model1=randomForest(Survived~ SexClass+ Age +FarePP+group, data=trainFix)
varImpPlot(RF_model1)
pred_train <-predict(RF_model1,trainFix)
mean(pred_train==trainFix$Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

test$Survived=predict(RF_model, testFix)
submission=data.frame(PassengerID=test$PassengerId, Survived=test$Survived)

write.csv(submission, "submission.csv", row.names=FALSE)

