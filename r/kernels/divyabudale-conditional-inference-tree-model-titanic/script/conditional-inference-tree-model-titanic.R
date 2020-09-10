#Loading the data
train <- read.csv('../input/train.csv', header=TRUE,stringsAsFactors = FALSE)
test <- read.csv('../input/test.csv', header=TRUE,stringsAsFactors = FALSE)
library(dplyr)
comb<-bind_rows(train,test)

#Title procesing
#Extracting the Title from name
comb$Title<-gsub('(.*,)|(\\..*)','',comb$Name)
table(comb$Sex,comb$Title)
#All the titles apart from Miss,Mrs,Master and Mr are recorded
f<-c('Dona','Lady','Mlle','Mme','Ms','the Countess')
m<-c('Capt','Col','Don','Jonkheer','Major','Rev','Sir')
library(stringr)
#The values in the f variable are changed to 'Miss'
comb$Title<-ifelse(str_trim(comb$Title) %in% f,'Miss',str_trim(comb$Title))
#The values in the m variable are changed to 'Mr'
comb$Title<-ifelse(str_trim(comb$Title) %in% m,'Mr',str_trim(comb$Title))
#Ttitle Dr is changed to Miss or Mr depending on the sex
comb[comb$Title=='Dr' & comb$Sex=='female',]$Title<-'Miss'
comb[comb$Title=='Dr' & comb$Sex=='male',]$Title<-'Mr'
comb$Title<-factor(comb$Title)


#Cabin procesing
#Cabin is extract as the first letter of the variable value
comb$Cabin<-substr(comb$Cabin,1,1)
table(comb$Cabin,comb$Pclass)
#There is only 1 value for cabin T and it belongs Pclass=1. Since cabin C has the largest value, the value of T is replaced with 'C'
comb[comb$Cabin=='T',]$Cabin<-"C"
#All the other null enteries are placed with O i.e Other
comb$Cabin<-factor(ifelse(comb$Cabin=="","O",comb$Cabin)) #All other in 'O'

#Embarked processing
table(comb$Embarked)
comb$Embarked<-as.character(comb$Embarked)
# The null entries for embarked are replaced with 'S', one with the highest count
comb$Embarked<-factor(ifelse(comb$Embarked=="","S",comb$Embarked)) 

#Fare processing
comb[is.na(comb$Fare),]
#The missing value for fare is replaced with mean for Cabin 'O' and Pclass = 3
comb[is.na(comb$Fare),]$Fare<-mean(comb[comb$Pclass==3 & comb$Cabin=='O',]$Fare,na.rm=TRUE)

#Extract information from ticket
#The dataset is sorted on the ticket number to see if there are same ticket numbers for 2 diferent passenger
comb<-comb[order(comb$Ticket),]
#Since, the passengers travlling together are assinged similiar ticket number, a Gr
comb$GroupNum<-factor(match(comb$Ticket,unique(comb$Ticket)))
#Grouping by GroupNUm to create a new column GroupSize
x<-group_by(comb,GroupNum)
y<-summarise(x,GroupSize=n())
y<-as.data.frame(y)
comb<-merge(x=comb,y=y,by="GroupNum",all=TRUE)
comb$GroupNum<-NULL
comb$GroupSize<-factor(comb$GroupSize)

comb$FamilySize<-factor(comb$SibSp+comb$Parch+1)

comb$Pclass<-factor(comb$Pclass)
comb$Sex<-factor(comb$Sex)


comb$Survived<-factor(comb$Survived)
comb$PassengerId<-factor(comb$PassengerId)



myformula_age<-Age~Pclass+Sex+Fare+Cabin+Embarked+Title #no error
model_age<-step(lm(myformula_age,data=comb),direction="backward")
comb$Age[is.na(comb$Age)] <- predict(model_age, comb[is.na(comb$Age),])
summary(comb$Age)

comb1<-comb[order(comb$PassengerId),]
set.seed(1234)
ind<-sample(2,nrow(comb1),replace=TRUE,prob=c(0.7,0.3))
train.data<-comb1[1:891,]
test.data<-comb1[892:1309,]
library(party)
myformula<-Survived~Pclass+Sex+Age+Fare+Cabin+Embarked+Title+GroupSize+FamilySize
fulltree<-ctree(myformula,data=train.data)
plot(fulltree,gp=gpar(fontsize=10))
Survived<-predict(fulltree,test.data)
PassengerId<-test.data$PassengerId
result<-data.frame(PassengerId,Survived)
write.csv(result, file = "my_solution.csv",row.names=FALSE)


