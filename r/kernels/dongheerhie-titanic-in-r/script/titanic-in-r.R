
# loading packages

library("ggplot2")
library("corrplot")
library("pROC")



# Getting Data


train <- read.csv("../input/train.csv", header=TRUE,stringsAsFactors = F)
test <- read.csv("../input/test.csv", header=TRUE,stringsAsFactors = F)

test$Survived <- NA
test$cate <- 'test'
train$cate <- 'train'

full <- rbind(train,test)

# EDA -> Z 

d <- data.frame(Pclass=full$Pclass[full$cate=='train'],Survived=full$Survived[full$cate=='train'])
ggplot(d,aes(x=factor(Pclass),fill=factor(Survived))) + 
  geom_histogram(stat="count",position='dodge') + 
  scale_fill_brewer(palette="Pastel1") 


# EDA -> Sex

d <- data.frame(Sex=full$Sex[full$cate=='train'],Survived=full$Survived[full$cate=='train'])
ggplot(d,aes(x=factor(Sex),fill=factor(Survived))) + 
  geom_histogram(stat="count",position='dodge') + 
  scale_fill_brewer(palette="Pastel1") 

# EDA -> Age

table(is.na(full$Age))
## 263 NAs, replace with random numbers

set.seed(123)
n=nrow(full)

Age <- full$Age

for(i in 1:n) {
  if(is.na(full$Age[i])) { 
    Age[i] = sample(na.omit(full$Age),1)
  }
}

## No significant difference in distribution!

ggplot(data.frame(Age=Age),aes(x=Age)) +
  geom_histogram(binwidth=5) 

ggplot(na.omit(full),aes(x=Age)) +
  geom_histogram(binwidth=5) 

## applying and binning

par(mfrow=c(1,1))

d <- data.frame(Age=full$Age[full$cate=='train'],
                   Survived=full$Survived[full$cate=='train'])

cuts <- cut(d$Age,hist(d$Age,10,plot=F)$breaks)
rate <- tapply(d$Survived,cuts,mean)
d2 <- data.frame(Age=names(rate),rate)
barplot(d2$rate,xlab='Age',ylab="Survival Rate")

full$Age2[full$Age<=10] <- 1
full$Age2[full$Age>10 & full$Age<=60] <- 2
full$Age2[full$Age>60] <- 3


# EDA - SibSp, Parch

table(full$SibSp)
table(full$Parch)

## making new variable - Fsize

full$Fsize <- full$Parch + full$SibSp +1


par(mfrow=c(1,2))

table(full$Fsize)
d <- data.frame(Fsize=full$Fsize[full$cate=='train'],
                Survived=full$Survived[full$cate=='train'])
rate <- tapply(d$Survived,d$Fsize,mean)
d2 <- data.frame(x=names(rate),rate)
barplot(d2$rate,xlab='Fsize',ylab="Survival Rate")


full$Fsize2[full$Fsize>=2 & full$Fsize<=4] <- 1
full$Fsize2[full$Fsize<2 | full$Fsize>4] <- 2


# EDA - Ticket

table(full$Ticket)

full$Ticket2 <- substr(full$Ticket, start=1, stop=1)

d <- data.frame(Ticket2=full$Ticket2[full$cate=='train'],
                Survived=full$Survived[full$cate=='train'])
rate <- tapply(d$Survived,d$Ticket2,mean)
d2 <- data.frame(x=names(rate),rate)
barplot(d2$rate,xlab='Ticket2',ylab="Survival Rate")

table(full$Ticket2)

n=nrow(full)

for (i in 1:n) {
  
  if (full$Ticket2[i]==1 | full$Ticket2[i]==2 | full$Ticket2[i]==9 | full$Ticket2[i]=='F' |  full$Ticket2[i]=='P') {
      full$Tkt[i] = 1
  } else {full$Tkt[i]<-2}
}

table(full$Tkt)


# EDA - Fare

table(is.na(full$Fare))

full$Embarked[is.na(full$Fare)]
mean(full$Fare[full$Embarked=='S'],na.rm=T)

d <- data.frame(Fare=full$Fare, Embarked=full$Embarked)

ggplot(d,aes(x=Embarked, y=Fare)) +
  geom_boxplot()

full$Fare[is.na(full$Fare)] <- 27


par(mfrow=c(1,1))

d <- data.frame(Fare=full$Fare[full$cate=='train'],
                Survived=full$Survived[full$cate=='train'])

cuts <- cut(d$Fare,hist(d$Fare,10,plot=F)$breaks)
rate <- tapply(d$Survived,cuts,mean)
d2 <- data.frame(Fare=names(rate),rate)
barplot(d2$rate,xlab='Fare',ylab="Survival Rate")


full$Fare2[full$Fare<=50] <- 1
full$Fare2[full$Fare>50] <- 2


# EDA-Cabin

table(full$Cabin)
table(full$Deck)

full$Deck <- substr(full$Cabin,start=1, stop=1)
d <- data.frame(Deck=full$Deck[full$cate=='train'],
                Survived=full$Survived[full$cate=='train'])

rate <- tapply(d$Survived,d$Deck,mean)
d2 <- data.frame(names(rate),rate)
barplot(d2$rate)

n=nrow(full)

for (i in 1:n) {
  
  if (full$Deck[i] != 'A' & full$Deck[i] != 'B' & full$Deck[i] != 'C' & full$Deck[i] != 'D' &
      full$Deck[i] != 'E' & full$Deck[i] != 'F' & full$Deck[i] != 'G' & full$Deck[i] != 'T') {
  
    full$Deck[i]='N'    
  }
}

# EDA - embarked

table(full$Embarked)

for (i in 1:n) {
  if (full$Embarked[i] !='C' & full$Embarked[i] !='Q' & full$Embarked[i] !='S' ) {
    full$Embarked[i]='S'
  }
}

# EDA - Name

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

full$Title[full$Title=='Mlle'] <- 'Miss'
full$Title[full$Title=='Ms'] <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'


# creating set (train, valid, test)

str(full)

TRN <- full[full$cate=='train',]
TST <- full[full$cate=='test',]

TRN$rand <- sample(1:891,891,replace=F)

n = nrow(TRN)

for (i in 1:n) {
  if (TRN$rand[i]<=630) {
    TRN$cate[i]='train'
  } else {TRN$cate[i]='valid'}
}

TRNSET <- TRN[TRN$cate=='train',]
VALSET <- TRN[TRN$cate=='valid',]


# cor
str(TRNSET)
cordata <- data.frame(Age=TRNSET$Age,Fsize=TRNSET$Fsize
                      ,Pclass=TRNSET$Pclass,Fare=TRNSET$Fare)
cor <- cor(cordata)
corrplot.mixed(cor)

chi_data <- data.frame(
                factor(TRNSET$Pclass),
                factor(TRNSET$Sex),
                factor(TRNSET$Embarked),
                factor(TRNSET$Deck),
                factor(TRNSET$Title)
)




# building model - 1. logistic regression

str(TRNSET)

fit_logit <- glm(factor(Survived)~#factor(Pclass)+
                   factor(Sex)+factor(Age2)+factor(Fsize2)+
                   factor(Tkt) +factor(Fare2)+factor(Deck)
                   +factor(Embarked)+Title
                  ,data=TRNSET
                  ,family=binomial)

summary(fit_logit)

pred_valid <- predict(fit_logit,VALSET,type='response')
pred_roc <- roc(VALSET$Survived,pred_valid)
summary(pred_roc)
auc(pred_roc)
# Area under the curve: 0.8498
plot(pred_roc)
