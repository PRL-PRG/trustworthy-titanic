
# The gender model achieves a 76.6% leaderboard score
test <- read.csv("../input/titanic/test.csv",stringsAsFactors=F)
test$Survived[test$Sex=='male'] <- 0
test$Survived[test$Sex=='female'] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit,"genderModel.csv",row.names=F)

train <- read.csv("../input/titanic/train.csv",stringsAsFactors=F)
table(train$Survived[train$Sex=='male' & train$Age<16])
table(train$Survived[train$Sex=='female' & train$Pclass==3])

# engineer titles in training dataset
train$Title <- substring(train$Name,regexpr(",",train$Name)+2,regexpr("\\.",train$Name)-1)
train$Title[train$Title %in% c("Capt","Don","Major","Col","Rev","Dr","Sir","Mr","Jonkheer")] <- "man"
train$Title[train$Title %in% c("Dona","the Countess","Mme","Mlle","Ms","Miss","Lady","Mrs")] <- "woman"
train$Title[train$Title %in% c("Master")] <- "boy"
# engineer "woman-child-groups" for training dataset
train$Surname <- substring(train$Name,0,regexpr(",",train$Name)-1)
train$Surname[train$Title=='man'] <- 'noGroup'
train$SurnameFreq <- ave(1:891,train$Surname,FUN=length)
train$Surname[train$SurnameFreq<=1] <- 'noGroup'
# calculate "woman-child-group" survival rates
train$SurnameSurvival <- ave(train$Survived,train$Surname)
table(train$SurnameSurvival[train$Surname!='noGroup'])

# the following "woman-child-groups" all perish
x=train[train$SurnameSurvival==0,c("Surname")]; unique(x[order(x)])

# the following "woman-child-groups" all survive
x=train[train$SurnameSurvival==1,c("Surname")]; unique(x[order(x)])

# the following "woman-child-groups" have mixed survival
train[train$SurnameSurvival==1/7,c("Surname","Title","Survived")]
train[train$SurnameSurvival==1/3,c("Surname","Title","Survived")]
train[train$SurnameSurvival==3/4,c("Surname","Title","Survived")]

library(ggplot2)

# adjust survival rates for use on training set
train$AdjustedSurvival <- (train$SurnameSurvival * train$SurnameFreq - train$Survived) / (train$SurnameFreq-1)
# apply gender model plus new predictor to training set
train$predict <- 0
train$predict[train$Title=='woman'] <- 1
train$predict[train$Title=='boy' & train$AdjustedSurvival==1] <- 1
train$predict[train$Title=='woman' & train$AdjustedSurvival==0] <- 0
# plot how new predictor changes gender model
ggplot(train[train$Title=='woman',]) +
    geom_jitter(aes(x=Pclass,y=predict,color=factor(Survived))) + 
    labs(title="36 female predictions change from gender model on training set") +
    labs(x="Pclass",y="New Predictor") +
    geom_rect(alpha=0,color="black",aes(xmin=2.5,xmax=3.5,ymin=-0.45,ymax=0.45))
table(train$Survived[train$Title=='woman' & train$predict==0])

# plot how new predictor changes gender model
ggplot(train[train$Title!='woman',]) +
    geom_jitter(aes(x=Title,y=predict,color=factor(Survived))) +
    labs(title="16 male predictions change from gender model on training set") +
    labs(x="Title",y="New Predictor") +
    geom_rect(alpha=0,color="black",aes(xmin=0.5,xmax=1.5,ymin=0.55,ymax=1.45))
table(train$Survived[train$Title!='woman' & train$predict==1])

# Perform 25 trials of 10-fold cross validation
trials = 25; sum = 0
for (j in 1:trials){
x = sample(1:890); s = 0
for (i in 0:9){
    # Engineer "woman-child-groups" from training subset
    train$Surname <- substring(train$Name,0,regexpr(",",train$Name)-1)
    train$Surname[train$Title=='man'] <- 'noGroup'
    train$SurnameFreq <- ave(1:891,train$Surname,FUN=length)
    train$Surname[train$SurnameFreq<=1] <- 'noGroup'
    train$SurnameSurvival <- NA
    # calculate training subset's surname survival rate
    train$SurnameSurvival[-x[1:89+i*89]] <- ave(train$Survived[-x[1:89+i*89]],train$Surname[-x[1:89+i*89]])
    # calculate testing subset's surname survival rate from training set's rate
    for (k in x[1:89+i*89]) 
    train$SurnameSurvival[k] <- train$SurnameSurvival[which(!is.na(train$SurnameSurvival) & train$Surname==train$Surname[k])[1]]
    # apply gender model plus new predictor
    train$predict <- 0
    train$predict[train$Title=='woman'] <- 1
    train$predict[train$Title=='boy' & train$SurnameSurvival==1] <- 1
    train$predict[train$Title=='woman' & train$SurnameSurvival==0] <- 0
    c = sum(abs(train$predict[x[1:89+i*89]] - train$Survived[x[1:89+i*89]]))
    s = s + c
}
cat( sprintf("Trial %d has 10-fold CV accuracy = %f\n",j,1-s/890))
sum = sum + 1-s/890
}
cat(sprintf("Average 10-fold CV accuracy from %d trials = %f\n",trials,sum/trials))

# engineer titles in test dataset
test$Title <- substring(test$Name,regexpr(",",test$Name)+2,regexpr("\\.",test$Name)-1)
test$Title[test$Title %in% c("Capt","Don","Major","Col","Rev","Dr","Sir","Mr","Jonkheer")] <- "man"
test$Title[test$Title %in% c("Dona","the Countess","Mme","Mlle","Ms","Miss","Lady","Mrs")] <- "woman"
test$Title[test$Title %in% c("Master")] <- "boy"
# engineer "woman-child-groups" for entire dataset
test$Survived <- NA; test$predict <- NA; train$AdjustedSurvival <- NULL
train$Surname <- NULL; train$SurnameFreq <- NULL; train$SurnameSurvival <- NULL
allData <- rbind(train,test)
allData$Surname <- substring(allData$Name,0,regexpr(",",allData$Name)-1)
allData$Surname[allData$Title=='man'] <- 'noGroup'
allData$SurnameFreq <- ave(1:1309,allData$Surname,FUN=length)
allData$Surname[allData$SurnameFreq<=1] <- 'noGroup'
# using only "Name" scores 0.81818, correcting surname groups with "Ticket" scores 0.82296
# search single woman and children and correct surname groups using Ticket
for (i in which(allData$Title!='man' & allData$Surname=='noGroup'))
    allData$Surname[i] = allData$Surname[allData$Ticket==allData$Ticket[i]][1]
allData$Surname[is.na(allData$Surname)] <- 'noGroup'
# calculate "woman-child-group" survival rates
allData$SurnameSurvival <- NA
allData$SurnameSurvival[1:891] <- ave(allData$Survived[1:891],allData$Surname[1:891])
for (i in 892:1309) allData$SurnameSurvival[i] <- allData$SurnameSurvival[which(allData$Surname==allData$Surname[i])[1]]
# apply gender model plus new predictor to test dataset
allData$predict <- 0
allData$predict[allData$Title=='woman'] <- 1
allData$predict[allData$Title=='boy' & allData$SurnameSurvival==1] <- 1
allData$predict[allData$Title=='woman' & allData$SurnameSurvival==0] <- 0
# plot predictions
ggplot(allData[892:1309,]) +
    geom_jitter(aes(x=Title,y=predict)) +
    labs(title="18 predictions change from gender model on test set") +
    labs(x="Title",y="Prediction") +
    geom_rect(alpha=0,color="black",aes(xmin=0.5,xmax=1.5,ymin=0.55,ymax=1.45)) +
    geom_rect(alpha=0,color="black",aes(xmin=2.5,xmax=3.5,ymin=-0.45,ymax=0.45))


# create CSV file to submit
submit <- data.frame(PassengerId = allData$PassengerId[892:1309], Survived = allData$predict[892:1309])
write.csv(submit,"genderSurnameModel.csv",row.names=F)

leader = read.csv("../input/kaggle-titanic-leaderboard-may-15-2018/leaderboard.csv")
c1 = "gray30"; c2 = "black"
colors <- c(rep(c1,6),c2,rep(c1,28),c2,rep(c1,11),c2,rep(c1,16))
ggplot(data=leader[leader$Score>=0.6 & leader$Score<=0.9,]) +
    geom_histogram (aes(x=Score),binwidth=1/209,fill=colors,na.rm=TRUE) +
    xlim(c(0.6,0.9)) + ylim(c(0,1500)) +
    labs(title='Histogram of Kaggle titanic public leaderboard scores, May 15 2018') +
    annotate('text', x=0.65, y=300, label='0.627 - All dead model') +
    annotate('text', x=0.662, y=240, label='(=131/209) top 96%') +
    annotate('text', x=0.79, y=1450, label='0.766 - Gender model') +
    annotate('text', x=0.805, y=1390, label='(=160/209) top 64%') +
    annotate('text', x=0.86, y=300, label='0.823 - Surname model') +
    annotate('text', x=0.86, y=240, label='(=172/209) top 2%')
