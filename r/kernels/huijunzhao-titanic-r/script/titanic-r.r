
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(ggthemes)
library(scales)
library(plyr)
library(stringr)
library(InformationValue) #to compute WOE and IV
library(MLmetrics)
library(rpart)
library(randomForest)
library(dplyr)
library(e1071)
library(Amelia)
library(party)
library(gbm)
library(class)


# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#list.files("../input")

# Any results you write to the current directory are saved as output.

train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")

data <- bind_rows(train,test)
train.row <- 1:nrow(train)
test.row <- (1+nrow(train)):(nrow(train)+nrow(test)) 

str(data)

data$Survived <- factor(data$Survived)

ggplot(data=data[1:nrow(train),], mapping=aes(x=Pclass,y=..count..,fill=Survived))+
geom_bar(stat="count", position='dodge')+xlab('Pclass')+ylab('Count')+ggtitle('Survival vs Pclass')+
geom_text(stat="count", aes(label=..count..),position=position_dodge(width = 1),vjust=-0.5)+
theme(plot.title=element_text(hjust=0.5),legend.position="bottom")

# We compute the WOE (weight of evidence) and IV (information value) of the above information.
WOETable(X=factor(data$Pclass[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
IV(X=factor(data$Pclass[1:nrow(train)]),Y=data$Survived[1:nrow(train)])

data$Title <- gsub('(.*, )|(\\..*)','',data$Name)
table(data$Sex, data$Title)

rareTitle <- c('Dona','Lady','the Countess','Capt','Col','Don','Dr','Major','Rev','Sir','Jonkheer')

data$Title[data$Title=='Mlle'] <- 'Miss'
data$Title[data$Title=='Ms'] <- 'Miss'
data$Title[data$Title=='Mme'] <- 'Miss'

data$Title[data$Title %in% rareTitle] <- 'Rare Title'

table(data$Sex, data$Title)

data$Title <- as.factor(data$Title)

ggplot(data=data[1:nrow(train),], mapping=aes(x=Title,y=..count..,fill=Survived))+
geom_bar(stat="count", position='dodge')+xlab('Title')+ylab('Count')+ggtitle('Survival vs Title')+
scale_fill_discrete(name="Survived",breaks=c(0,1))+
geom_text(stat="count", aes(label=..count..),position=position_dodge(width = 1),vjust=-0.5)+
theme(plot.title=element_text(hjust=0.5),legend.position="bottom")

WOETable(X=factor(data$Title[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
IV(X=factor(data$Title[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
# Also highly predictive

# Sex
data$Sex <- as.factor(data$Sex)

ggplot(data=data[1:nrow(train),], mapping=aes(x=Sex,y=..count..,fill=Survived))+
geom_bar(stat="count", position='dodge')+xlab('Sex')+ylab('Count')+ggtitle('Survival vs Sex')+
geom_text(stat="count", aes(label=..count..),position=position_dodge(width = 1),vjust=-0.5)+
theme(plot.title=element_text(hjust=0.5),legend.position="bottom")

WOETable(X=as.factor(data$Sex[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$Sex[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
# Highly predictable. Females are more likely to survive than males.

# Ages
ggplot(data=data[(!is.na(data$Age)) & row(data[,'Age'])<=891,], aes(x=Age,y=..count..,color=Survived))+
geom_line(stat='bin',binwidth=5)+labs(title="Survival vs Age", x="Age", y="Count", fill="Survived")

WOETable(X=factor(data$Age[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
IV(X=factor(data$Age[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
# Highly predictable. Children at ages <=7 are more likely to survive.

ftable(xtabs(~ Pclass+Sex+Survived, data=data))

#Females in higher classes (1 and 2) are more likely to survive than those in lower classes (3).

# Family size (SibSp and Parch)
data$FamilySize <- data$SibSp + data$Parch +1
ggplot(data=data[1:nrow(train),], mapping=aes(x=FamilySize,y=..count..,fill=Survived))+
geom_bar(stat="count", position='dodge')+xlab('FamilySize')+ylab('Count')+ggtitle('Survival vs Family Size')+
geom_text(stat="count", aes(label=..count..),position=position_dodge(width = 1),vjust=-0.5)+
theme(plot.title=element_text(hjust=0.5),legend.position="bottom")

WOETable(X=as.factor(data$FamilySize[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$FamilySize[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
# Highly predictable. Families of sizes 2-4 have higher survival rates.

#Fare
ggplot(data=data[(!is.na(data$Fare)) & row(data[,'Fare'])<=891,], aes(x=Fare,y=..count..,color=Survived))+
geom_line(stat='bin',binwidth=10)+labs(title="Survival vs Fare", x="Fare", y="Count", fill="Survived")

WOETable(X=as.factor(data$Fare[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$Fare[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
# Highly predictable. The higher the fare, the higher the survival rate.

#Cabin
ggplot(data=data[1:nrow(train),], mapping=aes(x=as.factor(sapply(data$Cabin[1:nrow(train)],function(x)
    str_sub(x,start=1,end=1))), y=..count.., fill=Survived))+
geom_bar(stat="count", position='dodge')+xlab('Cabin')+ylab('Count')+ggtitle('Survival vs Cabin')+
geom_text(stat="count", aes(label=..count..),position=position_dodge(width = 1),vjust=-0.5)+
theme(plot.title=element_text(hjust=0.5),legend.position="bottom")

data$Cabin <- sapply(data$Cabin, function(x) str_sub(x,start=1,end=1))

WOETable(X=as.factor(data$Cabin[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$Cabin[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
# Highly predictable. But there are too many missing values. Not really predictive.

# Embarked
ggplot(data=data[1:nrow(train),], mapping=aes(x=Embarked,y=..count..,fill=Survived))+
geom_bar(stat="count",position='dodge')+xlab('Embarked')+ylab('Count')+ggtitle('Survival vs Embarked')+
geom_text(stat="count", aes(label=..count..),position=position_dodge(width = 1),vjust=-0.5)+
theme(plot.title=element_text(hjust=0.5),legend.position="bottom")

WOETable(X=as.factor(data$Embarked[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
IV(X=as.factor(data$Embarked[1:nrow(train)]),Y=data$Survived[1:nrow(train)])
# Highly predictable. Survival rates C > Q > S.

# Count how many values are missing for each feature.
attach(data)
missing <- list(Pclass=nrow(data[is.na(Pclass), ]))
missing$Name <- nrow(data[is.na(Name), ])
missing$Sex <- nrow(data[is.na(Sex), ])
missing$Age <- nrow(data[is.na(Age), ])
missing$FamilySize <- nrow(data[is.na(FamilySize), ])
missing$Fare <- nrow(data[is.na(Fare), ])
missing$Cabin <- nrow(data[is.na(Cabin), ])
missing$Embarked <- nrow(data[is.na(Embarked), ])

for (name in names(missing)){
    if (missing[[name]][1] > 0){
        print(paste('',name,' miss ',missing[[name]][1],' values',sep=''))
    }
}
detach(data)
# Too many missing in Cabin. We give up this feature.

# Fill in Age values. We first use other features to predict Age and then fill with the predition.
age.model <- rpart(Age ~ Pclass+Sex+SibSp+Parch+Fare+Embarked+Title, data=data[!is.na(data$Age), ],
                  method='anova')
data$Age[is.na(data$Age)] <- predict(age.model,data[is.na(data$Age), ])

#Fill in Fare and Embarked. There are only 1 or 2 missing. So we find out the missing line 
#and fill with our estimation.
getMissingFare <- function(total_data){
    count <- 0
    for (i in 1:nrow(total_data)){
        if(is.na(total_data$Fare[i])){
            print(i);
            count <- count+1
        }
    }
    return(count)
}

getMissingFare(data)
data[1044,]

data$Fare[1044] <- median(data[data$Pclass=='3' & data$Embarked=='S',]$Fare, na.rm=TRUE)

data[is.na(data$Embarked),c('Pclass','Fare')]
# Both missing values for Embarked are for passengers in Pclass 1 who bought the tickets at the 
# price $80. We set their Embarked to be C.

data$Embarked[is.na(data$Embarked)] <- 'C'
data$Embarked <- as.factor(data$Embarked)

set.seed(415)
model <- cforest(Survived ~ Pclass+Title+Sex+Age+FamilySize+Fare+Embarked, data=data[train.row, ],
                controls=cforest_unbiased(ntree=2000, mtry=3))
# cross-validation
cv.summarize <- function(data.true,data.predict){
    print(paste('Recall:',Recall(data.true,data.predict)))
    print(paste('Precision:',Precision(data.true,data.predict)))
    print(paste('Accuracy:',Accuracy(data.true,data.predict)))
    print(paste('AUC (Area Under Curve):',AUC(data.true,data.predict)))
}

set.seed(415)
cv.sample <- sample(1:nrow(train),as.integer(0.3*nrow(train)),replace=TRUE)
cv.test <- data[cv.sample, ]
cv.prediction <- predict(model,cv.test,OOB=TRUE,type="response")
cv.summarize(cv.test$Survived,cv.prediction)

# The accuracy is acceptable. So we use this model to do prediction

predict.result <- predict(model, data[test.row, ], OOB=TRUE, type="response")
output <- data.frame(PassengerId=test$PassengerId, Survived=predict.result)
write.csv(output, file='output.csv', row.names=FALSE)
