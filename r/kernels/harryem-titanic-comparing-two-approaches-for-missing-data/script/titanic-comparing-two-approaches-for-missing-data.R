## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2) #charting
library(scales) #charting
library(grid) #charting
library(plyr) #data wrangling
library(dplyr) #data wrangling
library(tidyr) #data wrangling
library(Hmisc) #data wrangling
library(mice) #imputing variables
library(randomForest) #modelling
library(caret) #modelling


traindata <- read.csv('../input/train.csv', stringsAsFactors = F)
testdata <- read.csv('../input/test.csv', stringsAsFactors = F)

c(object.size(traindata),object.size(testdata))

testdata$Survived <- "NA"
merged <- rbind(traindata,testdata)

length(unique(merged$PassengerId)) == length(merged$PassengerId) # check no duped entries


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(merged)
colSums(is.na(merged))
colSums(merged=="")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
a <- colSums(is.na(testdata))+colSums(testdata=="")
a <- names(a[is.na(a)|a!=0])
a

missing <- c()

for (i in a) {
  missing <- paste(missing,as.integer(!is.na(merged[i])^!merged[i]==""),sep="")
                 }

merged[missing=="100",] 

#There is only one example of this combination and its in the test set. I will discuss this later.

table(missing)
merged$Missing <- missing


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged$Pclass <- as.factor(merged$Pclass)
merged$Sex <- as.factor(merged$Sex)

g <- ggplot(merged[1:891,], aes(x=Pclass,fill=factor(Survived))) + geom_bar(pos  = "dodge") + labs(fill = "Survived",title="Survivor split by ticket class")

dftemp <- merged[1:891,] %>%
    group_by(Pclass) %>%
    summarise(Survive = sum(Survived == 1) / n(),
              DidNotSurvive = sum(Survived == 0) / n()) %>%
    gather(key = Group,value = Surv,Survive:DidNotSurvive)

gn <- ggplot(dftemp, aes(x = Pclass,
                            y = Surv, 
                            fill = as.factor(Group))) + 
    geom_bar(position = "dodge",stat = "identity") + 
    scale_y_continuous(labels = percent_format()) +
    labs(y = "Proportion Survived",title="Survivor split by ticket class - Normalized") +
    theme(legend.title=element_blank(), plot.title = element_text(size=14))


vp <- viewport(width = 0.3, height = 0.3, x = 0.85,
     y = 0.85)

print(gn)
theme_set(theme_bw(base_size = 8))
print(g,vp=vp)

g <- ggplot(merged[1:891,], aes(x=Pclass,fill=factor(Survived))) + geom_bar(pos  = "fill") + facet_wrap(~Sex) + labs(y = "Proportion Survived",fill = "Survived",title="Survivor split by ticket class and gender")
g + theme(plot.title = element_text(size=14))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
qplot(merged$Age,fill=I("red"),xlab = "Age")

agebrackets <- c(0,13,18,30,55)
merged$Agebracket <- findInterval(merged$Age,agebrackets)

agetable <- data.frame(Agebracket=c(1,2,3,4,5),Age_range=c("<13","13-17","18-29","30-54","55+"))
merged <- join(merged,agetable,by="Agebracket")
merged$Agebracket <- as.factor(merged$Agebracket)

g <- ggplot(merged[1:891,], aes(x=Age_range,fill=factor(Survived))) + geom_bar(pos="dodge") + labs(fill = "Survived",title="Survivor split by age group") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

dftemp <- merged[1:891,] %>%
    group_by(Age_range) %>%
    summarise(Survive = sum(Survived == 1) / n(),
              DidNotSurvive = sum(Survived == 0) / n()) %>%
    gather(key = Group,value = Surv,Survive:DidNotSurvive)

gn <- ggplot(dftemp, aes(x = Age_range,
                            y = Surv, 
                            fill = as.factor(Group))) + 
    geom_bar(position = "dodge",stat = "identity") + 
    scale_y_continuous(labels = percent_format()) +
    labs(y = "Proportion Survived",title="Survivor split by age group - Normalized") +
    theme(legend.title=element_blank(),plot.title = element_text(size=14))

vp <- viewport(width = 0.3, height = 0.3, x = 0.85,
     y = 0.85)

print(gn)
theme_set(theme_bw(base_size = 8))
print(g,vp=vp)

g <- ggplot(merged[1:891,], aes(x=Age_range,fill=factor(Survived))) + geom_bar(pos="fill") + labs(y = "Proportion Survived",fill = "Survived",title="Survivor split by age and gender - Normalized") + facet_wrap(~Sex)
g + theme(plot.title = element_text(size=14))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(merged$Cabin,30)
length(unique(merged$Cabin))/length(merged$Cabin) ## only 14% are unique so there are a lot shared.
merged$Cabin[28] # this looks strange, multiple cabins on one ticket
subset(merged,Cabin == "C23 C25 C27") # it was one family, the Fortunes

merged$HasCabin <- as.factor(!(merged$Cabin==""))

g <- ggplot(merged[1:891,], aes(x=HasCabin,fill=factor(Survived))) + geom_bar()
g <- g +facet_wrap(~Pclass) + labs(title="Survivor split by class and Cabin") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

gn <- ggplot(merged[1:891,], aes(x=HasCabin,fill=factor(Survived))) + geom_bar(pos="fill")
gn <- gn +facet_wrap(~Pclass) +labs(y = "Proportion Survived",fill = "Survived",title="Survivor split by class and Cabin - Normalized") + theme(plot.title = element_text(size=14))

vp <- viewport(width = 0.35, height = 0.35, x = 0.85,
     y = 0.8)

print(gn)
theme_set(theme_bw(base_size = 8))
print(g,vp=vp)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
qplot(merged$Fare,bins=150,fill=I("red"),xlab = "Fare")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
a <- subset(merged,is.na(merged$Fare))
a
merged[a[,1],]$Fare <- mean(subset(merged,Pclass==3)$Fare,na.rm=TRUE)

merged$Farebracket <- as.factor(cut2(merged$Fare,g=5))

g <- ggplot(merged[1:891,], aes(x=Farebracket,fill=factor(Survived))) + geom_bar(pos="fill")
g <- g +facet_wrap(~Pclass) + labs(y = "Proportion Survived",fill = "Survived",title="Survivor split by class and Fare Bracket - Normalized")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1),plot.title = element_text(size=14))
g 

subset(merged,Fare==0)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(order(merged$Fare,decreasing = TRUE))
merged[259,]

subset(merged,Fare==merged$Fare[259])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g <- ggplot(merged[1:891,],aes(x=Fare,y=Age,shape=factor(Survived),color=factor(Survived))) + geom_point() + scale_shape_manual(values=c(1,3)) + xlim(0, 300)

g <- g +facet_wrap(~Pclass) + labs(fill="Survived",title="Survival scatterplot of Fare and Age, Split by Class") + theme(plot.title = element_text(size=14))
g


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged$Title <- gsub('(.*, )|(\\..*)', '', merged$Name)

count(merged,Title)

merged$Title <- as.factor(merged$Title)

a <- count(merged,Title)

a <- a[a$n>2,]$Title
dftemp <- merged[1:891,]
dftemp <- dftemp[dftemp$Title %in% a,]

g <- ggplot(dftemp, aes(x=Title,fill=factor(Survived))) + geom_bar(pos="fill")
g <- g +facet_wrap(~Pclass) + labs(y = "Proportion Survived",fill = "Survived",title="Survivor split by class and Title - Normalized") + theme(plot.title = element_text(size=14))
g


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
merged <- ddply(merged,.(Ticket),transform,Ticketsize=length(Ticket))
merged$Ticketsize <- as.factor(merged$Ticketsize)
merged <- merged[order(merged$PassengerId),] # ddply mixes up order


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
count(merged,Embarked)
subset(merged,Embarked == "")
merged[c(62,830),"Embarked"] <- "S"
merged$Embarked <- as.factor(merged$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
m1 <- merged[, !names(merged) %in% c("Agebracket","Age_range")]
mice_ages <- mice(m1[, !names(m1) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived','Missing')], method='rf',seed = 1234)
mice_out <- mice::complete(mice_ages)

merged$Age <- mice_out$Age
merged$Agebracket <- findInterval(merged$Age,agebrackets)
merged <- join(merged,agetable,by="Agebracket")

colSums(is.na(merged))+colSums(merged=="")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mergedtrain <- merged[1:891,]
mergedtest <- merged[892:1309,]
mergedtrain$Survived <- as.factor(traindata$Survived)

set.seed(414)
inTrain<- createDataPartition(y=mergedtrain$Survived,p=0.75, list=FALSE)
train <- mergedtrain[inTrain,]
test <- mergedtrain[-inTrain,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(414)

rf_agegroups <- randomForest(factor(Survived) ~ Pclass + Sex + Farebracket + Agebracket + HasCabin + Ticketsize + Embarked + Title,
                       data = mergedtrain ,na.action = na.pass,nodesize=20)

rf_agegroups

rf_age <- randomForest(factor(Survived) ~ Pclass + Sex + Farebracket + Age + HasCabin + Ticketsize + Embarked + Title,
                       data = mergedtrain ,na.action = na.pass,nodesize=20)

rf_age
rf_age$confusion
varImpPlot(rf_age)
importance(rf_age)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

unique(merged$Missing)

dftemp <- mergedtrain[,c("Survived","Pclass","Sex","Fare","Age","HasCabin","Ticketsize","Embarked","Title","Missing")]

rf110 <- randomForest(factor(Survived) ~ .,
                        data = subset(dftemp[,!names(dftemp) %in% c("HasCabin")],dftemp$Missing=="110"))

rf111 <- randomForest(factor(Survived) ~ .,
                        data = subset(dftemp[,!names(dftemp) %in% c()],dftemp$Missing=="111"))

rf010 <- randomForest(factor(Survived) ~ .,
                        data = subset(dftemp[,!names(dftemp) %in% c("Age","HasCabin")],dftemp$Missing=="010"))

rf011 <- randomForest(factor(Survived) ~ .,
                        data = subset(dftemp[,!names(dftemp) %in% c("Age")],dftemp$Missing=="011"))

rf100 <- rf111

rf110$confusion
rf111$confusion
rf010$confusion
rf011$confusion


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p110 <- predict(rf110,mergedtest[mergedtest$Missing=="110",])
p111 <- predict(rf110,mergedtest[mergedtest$Missing=="111",])
p010 <- predict(rf110,mergedtest[mergedtest$Missing=="010",])
p011 <- predict(rf110,mergedtest[mergedtest$Missing=="011",])
p100 <- predict(rf110,mergedtest[mergedtest$Missing=="100",])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
submission <- rbind(data.frame(PassengerId=names(p110),Survived=p110),data.frame(PassengerId=names(p111),Survived=p111),data.frame(PassengerId=names(p010),Survived=p010),data.frame(PassengerId=names(p011),Survived=p011),data.frame(PassengerId=names(p100),Survived=p100))

if(!file.exists("./predictions.csv")) {
        write.csv(submission, file = "./predictions.csv",row.names = F)}

