## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#libraries
library(randomForestSRC)
library(ggRandomForests)
library(mlr)
library(data.table)
library(ggplot2)
library(arules)

#init
seed = 314159
set.seed(seed)

#read data
colClassesTrain=c("integer","factor", "factor", "character", "factor", "numeric",  "integer", "integer", "character", "numeric", "character", "factor")
colClassesTest=c("integer", "factor", "character", "factor", "numeric",  "integer", "integer", "character", "numeric", "character", "factor")
trainData = read.csv("../input/train.csv", colClasses=colClassesTrain)
testData = read.csv("../input/test.csv", colClasses = colClassesTest)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#remove passengerId
rownames(trainData) = trainData$PassengerId
trainData$PassengerId = NULL
rownames(testData) = testData$PassengerId
testData$PassengerId = NULL

#combine train and test prior to preprocessing
survived = trainData$Survived
trainData$Survived = NULL
theData = rbind(trainData,testData)

# seperate some features that we will delete later #
delFeatures = c("Name", "Cabin", "Ticket","Embarked", "Parch", "SibSp")
tmpFeatures = theData[delFeatures]
theData[,delFeatures]=NULL


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#family size
theData$FamilySize = as.integer(tmpFeatures$SibSp + tmpFeatures$Parch +1)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#get title from Name
theData$Title = sapply(tmpFeatures$Name, function(name){
  tmp=strsplit(name, ',');
  title = NA
  if(length(tmp[[1]]>1)){
    tmp = strsplit(tmp[[1]][2], split= "[.]");
    if(length(tmp[[1]])>1){
      title = trimws(tmp[[1]][1])
    }
  }
  title
})
theData$Title[theData$Title=='Mme'] = "Mlle"
theData$Title[theData$Title %in% c('Capt', 'Col','Major', 'Dr', 'Rev')] = 'Officer'
theData$Title[theData$Title %in% c('Jonkheer','Don', 'Sir', 'the Countess', 'Dona', 'Lady')] = 'Royalty'
theData$Title[theData$Title %in% c('Mme', 'Ms','Mrs')] = 'Mrs'
theData$Title[theData$Title %in% c('Mlle', 'Miss')] = 'Miss'
theData$Title[theData$Title %in% c('Mr')] = 'Mr'
theData$Title[theData$Title %in% c('Master')] = 'Master'
theData$Title = as.factor(theData$Title)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#remove 'F ' from Cabin field
tmpFeatures$Cabin = gsub(pattern="F ", replacement="", x=tmpFeatures$Cabin)

#Parse Cabin
theData$Deck = NA
theData$CabinNumber = NA
for (i in 1:length(tmpFeatures$Cabin)){
  cabin = tmpFeatures$Cabin[i]
  if(cabin!=""){
    tmp=strsplit(cabin, ' ');
    theData$Deck[i] = substr(cabin, 0,1)#note: when a passenger has multiple rooms then they are all on the same deck.
    for(j in 1:length(tmp[[1]])){
      cbn = tmp[[1]][j]
      cbn = gsub("[a-zA-Z]", "", cbn)
      if(j==1){
        theData$CabinNumber[i]=as.integer(cbn)
      }
      #use j==2, j==3, j==4 to get the other cabins of a passenger
    }
    
  }
}
theData$Deck = as.factor(theData$Deck)
theData$CabinNumber = as.integer(theData$CabinNumber)

#Starboard-port information
theData$CabinMod4 = theData$CabinNumber%%4
theData$CabinMod4 = as.factor(theData$CabinMod4)

#Stern-bow information
boundaries = c(0,10,20,30,40,50,70,90,120,1000)
for(b in seq_len(length(boundaries)-1)){
  lower = boundaries[b]
  upper = boundaries[b+1]
  theData$CabinInterval[theData$CabinNumber>=lower & theData$CabinNumber<upper] = paste("Range", lower, upper)
}
theData$CabinInterval = as.factor(theData$CabinInterval)

#clean up
theData$CabinNumber = NULL



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#parse ticket information
theData$TicketNumber = NA
for (i in 1:length(tmpFeatures$Ticket)){
    ticket = tmpFeatures$Ticket[i]
    if(ticket!=""){
      tmp=strsplit(ticket, ' ');#should yield 1,2 or 3 elements. 
      nElts = length(tmp[[1]])
      
      if(nElts==1){#we have only a ticketnumber or a ticketletter
    	num = strtoi(tmp[[1]])
    	if(!is.na(num)){
    	  theData$TicketNumber[i] = num#field has only a number
    	}
      }
      else{#we have both ticketletter and ticketnumber.
    	theData$TicketNumber[i] = tail(tmp[[1]], n=1)
      }
    }
}
theData$TicketNumber = as.integer(theData$TicketNumber)
theData$TicketNumberInterval = discretize(theData$TicketNumber, method="frequency",categories=15)
theData$TicketNumber = NULL


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#impute simply from Sex, Embarked, Age, Pclass
idxs = theData$Sex=="male" & theData$Age>55 & theData$Age<65 & theData$Pclass=="3"
idxs[is.na(idxs)] = FALSE#there are some NAs in Age
theData$Fare[is.na(theData$Fare)] = median(theData[idxs,]$Fare, na.rm=TRUE)

#Fare per person
theData$FarePerPerson = NA
for(i in 1:length(tmpFeatures$Ticket)){
  idxs = tmpFeatures$Ticket == tmpFeatures$Ticket[i]
  theData$FarePerPerson[i] = theData$Fare[i]/sum(idxs)
}

#scale
theData$FarePerPerson = log10(1+theData$FarePerPerson)
theData$Fare = log10(1+theData$Fare)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#factors get NA as level
facts <- sapply(theData, is.factor)
for(i  in 1:ncol(theData)){
  if(facts[i]){
    idxs = is.na(theData[,i])
    if(any(idxs)){
      #theData[,i] = addNA(theData[,i], ifany=TRUE)  #I cant seem to select records later that have the NA level
      levels(theData[,i]) = c(levels(theData[,i]),"Unknown")
      theData[idxs,i] = "Unknown"
    }
    
    theData[,i] = droplevels(theData[,i])
  }
}


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#age to log
theData$Age = log10(theData$Age)

#impute Age with rf
imputeLrn = makeLearner("regr.randomForestSRC", fix.factors.prediction = TRUE)
pars = list(mtry=7, ntree=234, sampsize=1000, nsplit=11, nodesize=15)
imputeLrn = setHyperPars(imputeLrn, par.vals=pars)
imp = mlr::impute(obj=theData, cols = list(Age = imputeLearner(imputeLrn))) #
theData = imp$data


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ntrain = nrow(trainData)
trainData = theData[1:ntrain,]
testData = theData[(ntrain+1):nrow(theData),]
trainData$Survived = survived
rm(ntrain,theData, survived)

#and some cleanup
rm(tmpFeatures)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#setup mlr
levels(trainData$Survived) = c("Died", "Lived")
titanic.task = makeClassifTask(id = "titanic", data = trainData, target = 'Survived', positive = "Lived", fixup.data = "no")

#setup learner
set.seed(seed)
lrn = makeLearner("classif.randomForestSRC", fix.factors.prediction = TRUE)
pars = list(mtry=6, ntree=213, sampsize=750, nsplit=16, nodesize=15)
lrn = setHyperPars(lrn, par.vals = pars)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#cv performance
set.seed(seed)
resamp = makeResampleDesc("RepCV", folds = 10, reps=2, stratify = TRUE, predict="both")
r = resample(learner = lrn, task = titanic.task, resampling = resamp, show.info = TRUE, models = TRUE, measures=list(acc,setAggregation(acc, train.mean)))
r$aggr#test:

#learning curves
lrn = setHyperPars(lrn,par.vals=list(importance=FALSE))
resamp = makeResampleDesc("RepCV", folds=10, reps=3,stratify = TRUE, predict="both")
lc = generateLearningCurveData(learners = lrn,
                               task = titanic.task,
                               percs = seq(0.5, 1, by = 0.1), measures = list(acc, setAggregation(acc, train.mean)),
                               resampling = resamp
                               , show.info = TRUE)
plotLearningCurve(lc, facet="learner")+ scale_y_continuous(breaks=seq(0.8,1,by=0.01))



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#train
lrn = setHyperPars(lrn,par.vals=list(importance=TRUE))
mod = train(lrn, titanic.task)
pred = predict(mod, titanic.task)

#importance of variables
tmp = gg_vimp(getLearnerModel(mod, more.unwrap=TRUE))
plot(tmp)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

newSeed = 10
set.seed(newSeed)

#train
lrn = setHyperPars(lrn,par.vals=list(importance=TRUE, seed=-newSeed))
mod = train(lrn, titanic.task)
pred = predict(mod, titanic.task)


#write submission
pred = predict(mod, newdata = testData)
nTest = nrow(testData)
tmp = data.frame(PassengerId=integer(nTest), Survived=factor(nTest, levels=c("0", "1")))
tmp$PassengerId = as.integer(rownames(testData))
tmp$Survived = as.integer(pred$data$response)-1#note: rely on Died<Lived alphabetically
write.table(tmp, file = paste("submission",newSeed,".csv", sep=""), quote = FALSE, sep = ",",row.names = FALSE,col.names = TRUE)



## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
vals =c(0.80382,0.81339,0.79904,0.80861,0.80382,0.80861,0.7942,0.78947,0.80382,0.80861,0.80861,0.80861,0.80382,0.80382,0.81339,0.79904,0.81339,0.80861,0.80861,0.80382,0.79904,0.80861,0.79425,0.80861,0.79904,0.80861,0.79904,0.81339,0.80382,0.80861)
vals = as.data.frame(vals)
vals$group=" "
ggplot(data=vals, aes(x=group, y=vals)) +
    geom_boxplot(fill='#A4A4A4', color="black")+
    geom_dotplot(binaxis='y', stackdir='center', dotsize=1)+
    labs(x="",y="Accuracy on testset",title="Boxplot of accuracy on testset")



