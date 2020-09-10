
#requuired libraries
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(DMwR)
library(dummies)
library(ROCR)
library(randomForest)

#Lets read the data 
train = read.csv('../input/train.csv')
test = read.csv('../input/test.csv')

test$Survived <- NA
cmp_titanic = rbind(train, test)
summary(cmp_titanic)

str(cmp_titanic)

cmp_titanic$Survived <- as.factor(cmp_titanic$Survived)
cmp_titanic$Pclass <- as.factor(cmp_titanic$Pclass)
cmp_titanic$Name <- as.character(cmp_titanic$Name)
str(cmp_titanic)

table(cmp_titanic$Survived)

ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),],aes(Survived, fill = Survived)) +
  geom_bar()+
  labs(x= 'Survival Rate')+
  geom_label(stat = 'count', aes(label = ..count..), size = 4)+
  theme_grey(base_size = 18)

g1 <- ggplot(cmp_titanic, aes(Sex, fill = Sex)) +
  geom_bar()+
  labs(x='Gender Distribution in complete data')+
  geom_label(stat = 'count',aes(label=..count..))+
  scale_fill_manual(values = c("female" = "yellow", "male" = "sky blue"))

g2 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),], aes(x=Sex, fill = Survived))+
  geom_bar()+
  labs(x='Train data - Survival distribution across genders')+
  geom_label(stat = 'count',aes(label=..count..),position = "stack")+
  scale_fill_manual('legend',values = c("0" = "red", "1" = "green"))

grid.arrange(g1, g2, ncol=2)

cmp_titanic$LastName = unlist(lapply(cmp_titanic$Name, function(x){unlist(strsplit(x, split='[,.]'))[1]}))

data.frame(table(cmp_titanic$LastName))%>%
  group_by(Freq)%>%
  summarise(groups = n())

cmp_titanic$Title <-unlist(lapply(cmp_titanic$Name,
                                  function(x){unlist(strsplit(x, '[,.]'))[2]}))
cmp_titanic$Title <- trimws(cmp_titanic$Title)
data.frame(table(cmp_titanic$Title))

cmp_titanic$Title <- ifelse(cmp_titanic$Title=='Ms', 'Miss', cmp_titanic$Title)

cmp_titanic$Title <- ifelse(cmp_titanic$Title %in% c('Master','Miss','Mr', 'Mrs'),
                            cmp_titanic$Title, 'Rare')
cmp_titanic$Title <- as.factor(cmp_titanic$Title)

g1 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),], aes(Title, fill = Survived))+
  geom_bar(position = 'fill')+
  labs(x='Survival rate across Titles')

g2 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),], aes(Title, fill=Survived))+
  geom_bar(position = 'stack')+
  facet_grid(.~Pclass)+
  labs(x='Pclass wise survival for each Title')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(g1, g2, ncol = 2)

g1 <- ggplot(cmp_titanic, aes(Pclass, fill = Pclass)) +
  geom_bar() +
  geom_label(stat = 'count', aes(label = ..count..)) +
  labs(x='PClass Distribution in complete data')

g2 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),], aes(x=Pclass, fill = Survived)) +
  geom_bar(stat='count',position = 'stack') +
  labs(x='Train Data - Survived vs Not across Classes')

g3 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),], aes(x=Sex, fill = Survived)) +
  geom_bar(stat='count',position = 'fill') +
  labs(x='Train Data - Survived vs Not as per gender across Classes')+
  facet_grid(.~Pclass)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(g1, g2, g3, ncol=3)

nrow(cmp_titanic)
summary(cmp_titanic$Age)

g1 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Age) & !is.na(cmp_titanic$Survived),],
       aes(Age, fill = Survived))+
  geom_density(alpha=0.5)+
  labs(x='Age density to survival')

g2 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Age) & !is.na(cmp_titanic$Survived),],
       aes(Age, fill = Survived))+
  geom_density(alpha=0.5)+
  scale_fill_manual(values = c('red', 'green'))+
  facet_grid(.~Pclass)+
  labs(x='Age Density across Pclass to survival')

g3 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Age),], aes(x=Title, y=Age))+
  geom_boxplot()+
  labs(x= 'Age distribution across Titles')

g4 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Age),], aes(Title, y=Age, fill = Pclass))+
  geom_boxplot()+
  labs(x= 'Age distribution as per Pclass across Titles')+
  scale_fill_manual('legend', values = c('1' ='yellow', '2' = 'violet', '3' ='navy blue'))

grid.arrange(g1, g3, g2, g4, nrow = 2, top = 'Age distribution Models')


cmp_titanic$Family = cmp_titanic$SibSp + cmp_titanic$Parch + 1

data.frame(table(cmp_titanic$Family))

g1 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),], aes(Family, fill = Survived))+
  geom_bar(position = 'stack')+
  scale_x_continuous(breaks = seq(1,12,1))+
  labs(x='Survival as per family size')

g2 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),], aes(Family, fill = Survived))+
  geom_bar()+
  scale_x_continuous(breaks = seq(1,12,1))+
  facet_wrap(.~Sex+Pclass, ncol = 3)+
  labs(x='PClass - Gender - Family size')

grid.arrange(g1, g2, nrow = 2)

cmp_titanic[cmp_titanic$LastName=='Allison',c('Survived','Pclass','Name',
                                              'Sex','Age','SibSp','Parch',
                                              'Ticket','Title','LastName','Family')]
cmp_titanic[cmp_titanic$LastName=='Boulos',c('Survived','Pclass','Name',
                                             'Sex','Age','SibSp','Parch',
                                             'Ticket','Title','LastName','Family')]
cmp_titanic[cmp_titanic$LastName=='Brown',c('Survived','Pclass','Name',
                                             'Sex','Age','SibSp','Parch',
                                             'Ticket','Title','LastName','Family')]

cmp_titanic$Fml_Tkt = paste(cmp_titanic$Family,cmp_titanic$Ticket, sep = '-')
#now lets see the same records
cmp_titanic[cmp_titanic$LastName=='Brown',c('Survived','Pclass','Name',
                                            'Sex','Age','SibSp','Parch',
                                            'Ticket','Family','Fml_Tkt')]

cmp_titanic[cmp_titanic$Fml_Tkt=='3-11769',]

cmp_titanic[cmp_titanic$Fml_Tkt=='1-110152',]

cmp_titanic[cmp_titanic$LastName=='Andersson',]
cmp_titanic[cmp_titanic$Fml_Tkt=='7-347082',]

Pasngr_Ticket <- data.frame(cmp_titanic%>%
                              group_by(Ticket)%>%
                              summarise(OnBrd_2gtr = n()))

cmp_titanic = left_join(cmp_titanic,Pasngr_Ticket,by="Ticket")
cmp_titanic[cmp_titanic$LastName=='Andersson',]

nrow(cmp_titanic[cmp_titanic$Family!=cmp_titanic$OnBrd_2gtr,])

cmp_titanic[cmp_titanic$Ticket %in% c('PC 17558','PC 17761'),]

g1 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),], aes(Family, fill = Survived))+
  geom_bar(position = 'dodge')+
  scale_x_continuous(breaks = seq(1,12,1))+
  labs(x='Survival as per family size')

g2 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),], aes(OnBrd_2gtr, fill = Survived))+
  geom_bar(position = 'dodge')+
  scale_x_continuous(breaks = seq(1,12,1))+
  labs(x='Survival as per onboard together')

grid.arrange(g1, g2, ncol=1)

head(cmp_titanic[cmp_titanic$Pclass=='1',-c(4,5,6,7,8)])

cmp_titanic[cmp_titanic$Ticket %in% c('19950','PC 17759'),-c(1,2,4,5) ]

cmp_titanic[cmp_titanic$LastName %in% c('Hogeboom', 'Andrews'),]

cmp_titanic[!(cmp_titanic$Fare==''|cmp_titanic$Fare==0|is.na(cmp_titanic$Fare)==TRUE),] %>%
  group_by(Pclass) %>%
  summarise(mean(Fare))

cmp_titanic$Fare = ifelse((cmp_titanic$Fare==''|cmp_titanic$Fare==0|is.na(cmp_titanic$Fare)==TRUE),
       ifelse(cmp_titanic$Pclass==1,89.4,
              ifelse(cmp_titanic$Pclass==2,21.6,13.4)),cmp_titanic$Fare)

cmp_titanic$Fare_Pasngr = cmp_titanic$Fare/cmp_titanic$OnBrd_2gtr

sum(is.na(cmp_titanic$Cabin)) + nrow(cmp_titanic[cmp_titanic$Cabin=='',])

sum(is.na(cmp_titanic$Embarked)) + nrow(cmp_titanic[cmp_titanic$Embarked=='',])

cmp_titanic %>%
  group_by(Embarked) %>%
  summarise(n())

cmp_titanic$Embarked = as.character(cmp_titanic$Embarked)
cmp_titanic$Embarked = ifelse(cmp_titanic$Embarked=='', 'S', cmp_titanic$Embarked)
cmp_titanic$Embarked = as.factor(cmp_titanic$Embarked)

g1 <- ggplot(cmp_titanic, aes(x= Embarked, fill = Embarked))+
  geom_bar()

g2 <- ggplot(cmp_titanic[!is.na(cmp_titanic$Survived),], aes(x=Embarked, fill = Survived))+
  geom_bar()

g3 <- ggplot(cmp_titanic, aes(x=Embarked, fill = Pclass))+
  geom_bar(position = 'dodge')+
  scale_fill_manual('legend', values = c('1' ='yellow', '2' = 'violet', '3' ='navy blue'))

grid.arrange(g1, g3, g2, nrow = 2)

cmp_titanic[is.na(cmp_titanic$Age),]%>%
  group_by(Title)%>%
  summarise(n())

fnScaling = function(x){
  return((x-min(x))/(max(x)-min(x)))
}

master_dummies = dummy.data.frame(cmp_titanic[cmp_titanic$Title ==
                                                "Master",c(3,5,7,8,10,15,17,18)])
for(i in 1:ncol(master_dummies)){
  master_dummies[,i] = fnScaling(master_dummies[,i])
}
master_dummies=cbind(master_dummies,cmp_titanic[cmp_titanic$Title == "Master",c(6)])
colnames(master_dummies)[colnames(master_dummies) ==
    'cmp_titanic[cmp_titanic$Title == "Master", c(6)]'] <- 'Age'

summary(cmp_titanic[cmp_titanic$Title=='Master','Age'])
summary(round(knnImputation(data = master_dummies, k= 3)[,'Age']))

#Here in the distribution there is a slite deviation in the Median, Mean and
#3rd quadrant due to inclusion of 8 NA records with imputed values.
cmp_titanic[cmp_titanic$Title=='Master','Age'] = 
  round(knnImputation(data = master_dummies, k= 3)[,'Age'])

mr_dummies = dummy.data.frame(cmp_titanic[cmp_titanic$Title ==
                                            "Mr",c(3,5,7,8,10,15,17,18)])
for(i in 1:ncol(mr_dummies)){
  mr_dummies[,i] = fnScaling(mr_dummies[,i])
}
mr_dummies=cbind(mr_dummies,cmp_titanic[cmp_titanic$Title == "Mr",c(6)])
colnames(mr_dummies)[colnames(mr_dummies) ==
                'cmp_titanic[cmp_titanic$Title == "Mr", c(6)]'] <- 'Age'

#let us see the distribution of age for title Mr with NA's and after imputation
summary(cmp_titanic[cmp_titanic$Title=='Mr','Age'])
summary(round(knnImputation(data = mr_dummies, k= 3)[,'Age']))

#Here in the distribution there is a slite deviation in the Mean,Median and 2rd quadrant 
#due to inclusion of 176 NA records with imputed values.

cmp_titanic[cmp_titanic$Title=='Mr','Age'] = 
  round(knnImputation(data = mr_dummies, k= 3)[,'Age'])

miss_dummies = dummy.data.frame(cmp_titanic[cmp_titanic$Title ==
                                              "Miss",c(3,5,7,8,10,15,17,18)])
for(i in 1:ncol(miss_dummies)){
  miss_dummies[,i] = fnScaling(miss_dummies[,i])
}
miss_dummies=cbind(miss_dummies,cmp_titanic[cmp_titanic$Title == "Miss",c(6)])
colnames(miss_dummies)[colnames(miss_dummies) ==
                       'cmp_titanic[cmp_titanic$Title == "Miss", c(6)]'] <- 'Age'
cmp_titanic[cmp_titanic$Title=='Miss','Age'] = 
  round(knnImputation(data = miss_dummies, k= 3)[,'Age'])

mrs_dummies = dummy.data.frame(cmp_titanic[cmp_titanic$Title ==
                                             "Mrs",c(3,5,7,8,10,15,17,18)])
for(i in 1:ncol(mrs_dummies)){
  mrs_dummies[,i] = fnScaling(mrs_dummies[,i])
}
mrs_dummies=cbind(mrs_dummies,cmp_titanic[cmp_titanic$Title == "Mrs",c(6)])
colnames(mrs_dummies)[colnames(mrs_dummies) ==
                         'cmp_titanic[cmp_titanic$Title == "Mrs", c(6)]'] <- 'Age'
cmp_titanic[cmp_titanic$Title=='Mrs','Age'] = 
  round(knnImputation(data = mrs_dummies, k= 3)[,'Age'])

rare_dummies = dummy.data.frame(cmp_titanic[cmp_titanic$Title ==
                                             "Rare",c(3,5,7,8,10,15,17,18)])
for(i in 1:ncol(rare_dummies)){
  rare_dummies[,i] = fnScaling(rare_dummies[,i])
}
rare_dummies=cbind(rare_dummies,cmp_titanic[cmp_titanic$Title == "Rare",c(6)])
colnames(rare_dummies)[colnames(rare_dummies) ==
                        'cmp_titanic[cmp_titanic$Title == "Rare", c(6)]'] <- 'Age'
cmp_titanic[cmp_titanic$Title=='Rare','Age'] = 
  round(knnImputation(data = rare_dummies, k= 3)[,'Age'])

titanic_train = cmp_titanic[!is.na(cmp_titanic$Survived),]
nrow(titanic_train)
titanic_test = cmp_titanic[is.na(cmp_titanic$Survived),]
nrow(titanic_test)

ids = sample(nrow(titanic_train), nrow(titanic_train)* 0.8)
train_rf = titanic_train[ids,]
test_rf = titanic_train[-ids,]

train_rf <- train_rf %>%  mutate_if(is.character,as.factor)
test_rf <- test_rf %>%  mutate_if(is.character,as.factor)
#str(train_rf)

rfmod = randomForest(Survived~.-PassengerId-Name-Ticket-Cabin-LastName-Fml_Tkt,
                     data = train_rf,
                     ntree=3500,
                     mtry=3,
                     nodesize=10,maxnodes=15,
                     classwt=c(1,1.5),
                     strata = train_rf$Survived)
rfmod

pred = prediction(as.vector(rfmod$votes[,2]) , train_rf$Survived)
perf= performance(pred, "tpr","fpr")
plot(perf, colorize=T,text.adj=c(1.2,1.2), avg="threshold", lwd=3,main= "ROC")
AUC = performance(pred, measure = 'auc')@y.values[[1]]
print(AUC)

pred = prediction(as.vector(predict(rfmod,test_rf,type='vote')[,2]),test_rf$Survived)
perf= performance(pred, "tpr","fpr")
plot(perf, colorize=T, text.adj=c(1.2,1.2), avg="threshold", lwd=3, main= "ROC")
AUC = performance(pred, measure = 'auc')@y.values[[1]]
print(AUC)

titanic_test <- titanic_test %>%  mutate_if(is.character,as.factor)
titanic_test$Survived = predict(rfmod,titanic_test,type = 'response')
write.csv(titanic_test[,c('PassengerId','Survived')],'Titanic_sub.csv',row.names=FALSE)
