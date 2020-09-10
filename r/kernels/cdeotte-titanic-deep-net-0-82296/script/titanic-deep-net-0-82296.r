
train <- read.csv('../input/train.csv',stringsAsFactors=F)
test <- read.csv('../input/test.csv',stringsAsFactors=F)
test$Survived <- NA; allData <- rbind(train,test)
allData$TicketFreq <- ave(1:1309,allData$Ticket,FUN=length)
train$Title <- substring(train$Name,regexpr(',',train$Name)+2,regexpr('[.]',train$Name)-1)
train$Surname <- substring(train$Name,0,regexpr(',',train$Name)-1)
test$Title <- substring(test$Name,regexpr(',',test$Name)+2,regexpr('[.]',test$Name)-1)
test$Surname <- substring(test$Name,0,regexpr(',',test$Name)-1)
test$Fare[is.na(test$Fare)] = mean(allData$Fare[allData$Pclass==3],na.rm=T)
library(rpart)
ageFit <- rpart(Age ~ Title + Pclass + Sex + SibSp + Parch + Fare,train)

# RESPONSE: SURVIVED
trainData <- data.frame(Survived=train$Survived)
# FEATURE 1: GENDER
trainData$Sex <- 0
trainData$Sex[train$Sex=='female'] <- 1
# FEATURE 2: AGE
train$Age[is.na(train$Age)] <- predict(ageFit,train[is.na(train$Age),])
trainData$Age <- train$Age/80
# FEATURE 3: FAMILY SIZE
trainData$FamilySize <- (train$SibSp + train$Parch + 1)/11
# FEATURE 4: FARE ADJUSTED
trainData$FareAdj <- train$Fare/allData$TicketFreq[1:891]/222
# FEATURE 5: DID ONE FAMILY MEMBER SURVIVE?
trainData$FamilyOneSurvived <- 0
# FEATURE 6: DID ALL FAMILY MEMBERS DIE?
trainData$FamilyAllDied <- 0
for (i in 1:891){
    x = which(train$Surname==train$Surname[i] & train$PassengerId!=i)
    m <- mean(train$Survived[x])
    if (!is.na(m) & m>0) trainData$FamilyOneSurvived[i] <- 1
    if (!is.na(m) & m==0) trainData$FamilyAllDied[i] <- 1
}
write.csv(trainData,'trainData.csv',row.names=F)

head(trainData,10)

# FEATURE 1: GENDER
testData <- data.frame(Sex=rep(0,418))
testData$Sex[test$Sex=='female'] <- 1
# FEATURE 2: AGE
test$Age[is.na(test$Age)] <- predict(ageFit,test[is.na(test$Age),])
testData$Age <- test$Age/80
# FEATURE 3: FAMILY SIZE
testData$FamilySize <- (test$SibSp + test$Parch + 1)/11
# FEATURE 4: FARE ADJUSTED
test$FareAdj <- test$Fare/allData$TicketFreq[892:1309]
testData$FareAdj <- test$Fare/allData$TicketFreq[892:1309]/222
# FEATURE 5: DID ONE FAMILY MEMBER SURVIVE?
testData$FamilyOneSurvived <- 0
# FEATURE 6: DID ALL FAMILY MEMBERS DIE?
testData$FamilyAllDied <- 0
for (i in 1:418){
    x = which(train$Surname==test$Surname[i])
    m <- mean(train$Survived[x])
    if (!is.na(m) & m>0) testData$FamilyOneSurvived[i] <- 1
    if (!is.na(m) & m==0) testData$FamilyAllDied[i] <- 1
}
test$OneSurvived <- testData$FamilyOneSurvived
test$AllDied <- testData$FamilyAllDied
write.csv(testData,'testData.csv',row.names=F)

library(neuralnet)
f <- as.formula('Survived ~ Sex + Age + FamilySize + FareAdj + FamilyOneSurvived + FamilyAllDied')
set.seed(8)
fit <- neuralnet(f,trainData,hidden=5,linear.output=F)
plot(fit)

p <- compute(fit,testData); p <- ifelse(p[[2]]>0.5,1,0)
submit <- data.frame(PassengerId=892:1309,Survived=p)
write.csv(submit,'TitanicDeepNet.csv',row.names=F)
x = which( (test$Sex=='male' & p==1) | (test$Sex=='female' & p==0) )
row.names(test) <- 892:1309; test[x,c('Name','Sex','Age','SibSp','Parch','FareAdj','OneSurvived','AllDied')]

set.seed(1)
sum = 0
trials=10
cat(sprintf("Beginning %d trials of CV\n",trials))
for (i in 1:trials){
    s = sample(1:891,180)
    fit <- neuralnet(f,trainData[-s,],hidden=5,linear.output=F)
    p <- compute(fit,trainData[s,-1])
    p <- ifelse(p[[2]]>0.5,1,0)
    c = sum(abs(p-trainData$Survived[s]))
    cat(sprintf("  i=%d acc=%f\n",i,1-c/180))
    sum = sum + c
}
cat(sprintf("Average CV over %d trials = %f",trials,1-sum/(180*trials)))
