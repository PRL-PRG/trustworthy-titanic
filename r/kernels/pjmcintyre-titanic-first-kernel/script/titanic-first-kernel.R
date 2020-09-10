## ---- warning=FALSE,message=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(data.table)
library(scales)
library(pander)
library(rpart)
library(pROC)
library(plyr)
require(randomForest)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#list.files("../input")
trainData = read.csv(file='../input/train.csv', stringsAsFactors = F)
trainData = setDT(trainData)
summary(trainData)
testData = read.csv(file='../input/test.csv',stringsAsFactors = F)
testData = setDT(testData)
summary(testData)
trainData$sample = 'training'
testData$sample = 'testing'
trainData = rbind.fill(trainData,testData)
trainData = setDT(trainData)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[,Pclass:=as.factor(Pclass),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$surname = gsub(',.*','',trainData$Name,perl=TRUE)
trainData$title = gsub('.*,\\s|\\..*','',trainData$Name) # (anything, comma, white space) or (full stop, anything) gets replaced with blank

head(trainData[,c('Name','surname','title')])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$cabinLetter = gsub('(\\d*)|\\s+','',trainData$Cabin)
trainData$cabinNumCabins = nchar(trainData$cabinLetter)
trainData$cabinLetter = substring(trainData$cabinLetter,1,1)
trainData$cabinMain = sapply(strsplit(trainData$Cabin,' '),function(x) {if (length(x)>0){x[[1]]}else{''}})
trainData$cabinNumber = as.numeric(gsub('\\D*','',trainData$cabinMain))
trainData$hasCabin = ifelse(trainData$cabinLetter == '',0,1)

head(trainData[,c('Cabin','cabinLetter','cabinNumber','hasCabin')])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(trainData[,c('Ticket')])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$ticketNumber = as.numeric(gsub('^.* ','',trainData$Ticket))
trainData$ticketPrefix = gsub('\\d*$','',trainData$Ticket)

head(trainData[,c('Ticket','ticketNumber','ticketPrefix')])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(trainData[,c('Embarked')])


## ----familysize------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$familySize = trainData$SibSp + trainData$Parch + 1
trainData[,numPplSameTicket := .N,.(ticketNumber)]
trainData[,travelGroupSize:=pmax(familySize, numPplSameTicket),]
trainData[,travellingSolo:=ifelse(travelGroupSize==1,1,0),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

checkColumn = function(df,colname){
  
  testData = df[[colname]]
  numMissing = max(sum(is.na(testData)|is.nan(testData)|testData==''),0)

  
  if (class(testData) == 'numeric' | class(testData) == 'Date' | class(testData) == 'difftime' | class(testData) == 'integer'){
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = sum(is.infinite(testData)), 'avgVal' = mean(testData,na.rm=TRUE), 'minVal' = round(min(testData,na.rm = TRUE)), 'maxVal' = round(max(testData,na.rm = TRUE)))
  } else{
    list('col' = colname,'class' = class(testData), 'num' = length(testData) - numMissing, 'numMissing' = numMissing, 'numInfinite' = NA,  'avgVal' = NA, 'minVal' = NA, 'maxVal' = NA)
  }
  
}
checkAllCols = function(df){
  resDF = data.frame()
  for (colName in names(df)){
    resDF = rbind(resDF,as.data.frame(checkColumn(df=df,colname=colName)))
  }
  resDF
}


## ----results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pandoc.table(checkAllCols(trainData))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[,likelyChild:=ifelse(SibSp > 1,1,
                        ifelse(Parch > 2,0,
                                        3)),] #Use '3' to denote 'no clues'


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ageTree = rpart(Age ~ Fare + Pclass + Embarked + title + SibSp + Parch + travelGroupSize + likelyChild,data = trainData)
rpart.plot::rpart.plot(ageTree)
trainData$predAge = predict(ageTree,trainData)
ggplot(data=trainData,aes(x=Age,y=predAge)) + geom_point() + geom_smooth()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[is.na(Age),Age := predAge,]
trainData[,predAge := NULL,]
trainData[,likelyChild := NULL,]
summary(trainData$Age)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[Cabin=='',c('Cabin','cabinMain','cabinLetter','cabinNumCabins','cabinNumber','hasCabin'):=.('U','U','U',0,0,0)]
trainData[is.na(cabinNumber),cabinNumber:=0,]


## ----echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data=trainData,aes(x=Embarked)) + geom_bar() + facet_grid(Pclass ~ .,scales='free_y')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[Embarked == '',,] # travelling on same ticket
trainData[ticketNumber == 113572,,] # anyone else on same ticket?
#No, bother


## ---- results='asis'-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
embarked1stTree = rpart(Embarked ~ Fare,data = trainData[Pclass==1,,])
rpart.plot::rpart.plot(embarked1stTree)
trainData$predictedEmbarkationPoint = predict(embarked1stTree,trainData,type="class")
pandoc.table(table(trainData[Pclass==1,.(Embarked,predictedEmbarkationPoint),]))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#trainData[Embarked=='',Embarked:=predict(embarked1stTree,.SD,type='class')]
trainData[Embarked=='',Embarked:=predictedEmbarkationPoint,]
trainData[,predictedEmbarkationPoint:=NULL,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[is.na(ticketNumber),ticketNumber:=0,]
trainData[ticketPrefix == '',ticketPrefix:='Unk',]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[is.na(Fare),.N,.(sample)]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[Fare==0,.N,.(sample)]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fareTree = rpart(Fare ~ Age + Pclass + Embarked + title + SibSp + Parch + travelGroupSize,data = trainData)
rpart.plot::rpart.plot(fareTree)
trainData$predFare = predict(fareTree,trainData)
ggplot(data=trainData,aes(x=Fare,y=predFare)) + geom_point() + geom_smooth()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[is.na(Fare) | Fare==0,Fare := predFare,]
trainData[,predFare := NULL,]
summary(trainData$Fare)


## ----results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pandoc.table(checkAllCols(trainData))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#logit
logit <- function(pr) {
  #pr = pmax(pmin(pr,0.99999999999),0.00000000001)
  log(pr/(1-pr)) 
} 

#format graph
formatGraph = function(x,addYPctFmt=FALSE,ymin=NA,ymax=NA){
  x = x + 
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color='gray50',linetype = 'dashed'),
          panel.grid.major.x = element_blank()) +
    theme(panel.background = element_blank())
  
  if (addYPctFmt==TRUE){
    #x = x + 
    #  scale_y_continuous(limits=c(0,1), labels=percent) 
    if (!is.na(ymin) | !is.na(ymax)){
      x = x +
        scale_y_continuous(labels=percent,limits=c(ymin,ymax))
    } else{
       x = x +
           scale_y_continuous(labels=percent)
    }
  } else {
    if (!is.na(ymin) | !is.na(ymax)){
      x = x +
        scale_y_continuous(limits=c(ymin,ymax))
    } 
  }
  
  x
}

#plot a barplot
barPlot = function(x,y,data){
    #pandoc.header(paste0('\n logitBar plotting ',x,'\n'),3)
    pandoc.p(' \n Colour and labels show percentage of population in each category \n')
    
    plotData = data[!is.na(get(y)),.(meanY = mean(get(y),na.rm=TRUE),num=.N),.(get(x))]
    names(plotData) = c(x,y,'num')
    plotData[,totNum:=sum(num),]
    plotData[,pct:=round(num/totNum,2),]
    plotData[,logitY:=logit(get(y)),]
    
    ggp1 = ggplot(data=plotData,aes(x=get(x),y=get(y),colour=pct,fill=pct)) + geom_bar(stat='identity') + labs(x=x,y=y) + geom_label(aes(label=pct),fill='white')
    
    print(formatGraph(ggp1))
}

#plot a graph of logit odds against density
logitloess <- function(x, y, data,plotDensity=TRUE) { 
  
  #pandoc.p(paste0(' \n Plotting ',y,' against ',x))
  
  xvals = data[[x]]
  yvals = data[[y]]

  # data fixes
  xvals = xvals[!is.na(yvals)]
  yvals = yvals[!is.na(yvals)]
  

  if (length(unique(xvals))>20){
  
  
    #message('setting quantiles')
    xquantiles = quantile(xvals,prob=c(0.05,0.95),na.rm=TRUE)
  
    #message('loessfit - building local regression model')
    loessfit <- predict(loess(yvals~xvals,span = 0.7)) 
    loessfit2 = predict(loess(yvals~xvals,span = 0.2))
    pi <- pmax(pmin(loessfit,0.99999999),0.0000001) 
    pi2 = pmax(pmin(loessfit2,0.99999999),0.0000001)
    logitfitted <- logit(pi) 
    logitfitted2 = logit(pi2)
  
  
    yquantiles = quantile(logitfitted2,prob=c(0.05,0.95),na.rm=TRUE)
    t=data.frame(xaxis=xvals[!is.na(xvals)],yaxis=logitfitted,yaxis2=yvals[!is.na(xvals)],yaxis3=logitfitted2)
    t=t[t$xaxis>xquantiles[1] & t$xaxis < xquantiles[2],]
    #message('printing graph')
    ggp1 = ggplot(data=t,aes(x=xaxis,y=yaxis)) + geom_line() + ylab(paste("logodds(",y,")")) + xlab(x) + geom_line(aes(y=yaxis3),linetype='dashed',alpha=0.3) + geom_point(aes(y=yaxis3),alpha=0.3,colour='red') + geom_hline(yintercept = 0)  
    ggp2 = ggplot(data=t,aes(x=xaxis,y=yaxis)) + ylab(paste("density(",y,")")) + xlab(x) + geom_density(colour = 'red',fill='red',alpha=0.05,aes(y=..scaled..),show.legend =FALSE) 
  
    print(formatGraph(ggp1,ymin = yquantiles[1],ymax=yquantiles[2]))
    if (plotDensity){
        print(formatGraph(ggp2,addYPctFmt=TRUE))
    }
  }else{
    barPlot(x,y,data)
  }
    
  
}



## ----dataexploration, results='asis', error=FALSE, warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------
for (n in names(trainData)){
    #message('examining ',n,' for plotting')
    pandoc.header(n,3)
    if (class(trainData[[n]]) == 'numeric' | class(trainData[[n]]) == 'integer'){
        logitloess(n,'Survived',trainData)
    } else {
        barPlot(n,'Survived',trainData)
    }
    pandoc.p(' \n ')
}


## ----fare1, echo=FALSE,warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logitloess('Fare','Survived',trainData,plotDensity=FALSE)


## ----fare2, results='asis',warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[,fareLn := log(pmax(0.01,Fare)),]
logitloess('fareLn','Survived',trainData)


## ----fare3, results='asis'-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$fareQuantile = cut(x=trainData$Fare,quantile(x=trainData$Fare,na.rm=TRUE),include.lowest=TRUE) #withotut include.lowest we end up with NAs
logitloess('fareQuantile','Survived',trainData)


## ---- echo=FALSE, results='asis',warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logitloess('Age','Survived',trainData,plotDensity=FALSE)


## ----age3, results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$ageBucket = ifelse(trainData$Age < 16, '<16',
                        ifelse(trainData$Age < 22, '16 - 22',
                        ifelse(trainData$Age < 35, '21 - 35',
                        ifelse(trainData$Age < 50, '35 - 50',
                        ifelse(trainData$Age >= 50, '> 50','Unk')))))


#change order for plotting
trainData$ageBucketFactor = factor(trainData$ageBucket, levels = unique(trainData$ageBucket[order(trainData$Age)]))
logitloess('ageBucketFactor','Survived',trainData)
#delete factor to avoid model picking it up accidentally - instead it'll get the raw ageBucket
trainData[,ageBucketFactor:=NULL,]


## ----titleconsol,echo=FALSE, results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
barPlot('title','Survived',trainData)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

t = trainData[,.(num = .N, pctSurvived =sum(Survived)/sum(!is.na(Survived))),.(title,Sex)] 
setkeyv(t,c('Sex','pctSurvived')) 
t



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[title %in% c('Mme','Ms','Lady','the Countess','Dr','Dona') & Sex=='female',title:='Mrs',]
trainData[title %in% c('Mlle') & Sex=='female',title:='Miss',]


## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t = trainData[,.(num = .N, pctSurvived =sum(Survived)/sum(!is.na(Survived))),.(title,Sex)] 
setkeyv(t,c('Sex','pctSurvived')) 
t


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[!(title %in% c('Mr','Master')) & Sex == 'male',title:='Mr',]

## ---- echo=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t = trainData[!is.na(Survived),.(num = .N, pctSurvived =sum(Survived)/sum(!is.na(Survived))),.(title,Sex)] 
setkeyv(t,c('Sex','pctSurvived')) 
t


## ---- echo=FALSE, results='asis'-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
barPlot('title','Survived',trainData)


## ----cabinPlot, warning=FALSE, error=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = trainData,aes(x=cabinNumber,y=Survived,colour=Fare,fill=Fare,alpha=Fare)) + geom_point() + geom_smooth(colour='forestgreen') + facet_grid(cabinLetter ~ .) + scale_y_continuous(limits=c(0,1))


## ----cabinPlot2, warning=FALSE, error=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = trainData,aes(x=cabinNumber,y=Survived,colour=Fare,fill=Fare,alpha=Fare)) + geom_point() + geom_smooth(colour='forestgreen') + facet_grid(cabinLetter ~ factor(Pclass)) + scale_y_continuous(limits=c(0,1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t = trainData[!is.na(Survived),.(num = .N, survivedPct = mean(Survived)),.(Pclass,Sex)]
ggplot(data = t, aes(x=Pclass,y=survivedPct,fill=num,colour=num)) + geom_bar(stat='identity') + facet_grid(Sex ~ .)



## ---- results='asis'-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[Pclass %in% c('1','2') & Sex == 'female',iSexClass := 'femaleUpper',]
trainData[Pclass == '3' & Sex == 'female',iSexClass := 'femaleLower',]
trainData[Pclass == '1' & Sex == 'male',iSexClass := 'maleUpper',]
trainData[Pclass %in% c('2','3') & Sex == 'male',iSexClass := 'maleLower',]
barPlot('iSexClass','Survived',trainData)


## ----echo=FALSE,results='asis'---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
barPlot('travelGroupSize','Survived',trainData)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
t = trainData[!is.na(Survived),.(survProp=mean(Survived,na.rm=TRUE),meanFare=mean(Fare,na.rm=TRUE)),.(travelGroupSize,Pclass)]
ggplot(data = t,aes(x=travelGroupSize,y=survProp,colour=meanFare,fill=meanFare,alpha=meanFare)) + geom_bar(stat='identity') + facet_grid(factor(Pclass) ~ .) 



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[travelGroupSize > 3 & Pclass == 1 & substr(Name,1,6)=='Carter',.(Name,Sex,Age,Ticket,Fare),]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[,fareAdj:=Fare / numPplSameTicket,]
trainData[,fareAdjLn:=log(fareAdj+1),] #+1 to avoid 0 fares causing -Infinities
ggplot(data=trainData[Fare < quantile(trainData$Fare[trainData$Pclass ==1])[4],,],aes(x=Fare,colour=Pclass,fill=Pclass)) + geom_density() + facet_grid(Pclass ~ .) + scale_y_continuous(limits=c(0,1))
ggplot(data=trainData[fareAdj < quantile(trainData$Fare[trainData$Pclass ==1])[4],,],aes(x=fareAdj,colour=Pclass,fill=Pclass)) + geom_density() + facet_grid(Pclass ~ .) + scale_y_continuous(limits=c(0,1))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data=trainData[fareAdj < quantile(trainData$Fare[trainData$Pclass ==1])[4],,],aes(x=fareAdj,colour=Pclass,fill=Pclass)) + geom_density() + facet_grid(Pclass ~ Embarked,scales='free_y') 
#trainData[,.(avg=mean(fareAdj),sd=sd(fareAdj)) ,keyby=.(Pclass,Embarked,ageBucket)]


## ----results='asis',warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logitloess('fareAdj','Survived',trainData)
logitloess('fareAdjLn','Survived',trainData) #-Inf in there


## ----results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
barPlot('ageBucket','fareAdj',trainData[Pclass==3,,])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testData = trainData[sample=='testing',,]
trainData = trainData[sample=='training',,]


## ----results='asis',warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pandoc.table(checkAllCols(trainData))


## ----results='asis',warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
convertToFactors = function(dt){
  for (c in names(dt)){
    if (class(dt[,get(c),]) == 'character'){
      dt[,c(c):=as.factor(get(c)),]
    }
  }
}
profileByGraph = function(trainData,testData){
  bothDT = as.data.table(rbind.fill(trainData,testData))
  varlist = names(bothDT)
  varlist = varlist[!(varlist %in% c('Name','Ticket','Cabin','surname','cabinMain','ticketPrefix'))]
  bothDT[,sample:=ifelse(is.na(Survived),'Dev','Test'),]
  convertToFactors(bothDT)
  for (v in varlist){
    #message(v)
    if (length(unique(bothDT[[v]])) < 30){
      print(formatGraph(ggplot(data=bothDT,aes(x=get(v))) + geom_histogram(stat='count') + facet_grid(sample ~ .,scales = 'free_y') + ggtitle(v)))
    }else{
      print(formatGraph(ggplot(data=bothDT,aes(x=get(v))) + geom_histogram() + facet_grid(sample ~ .,scales = 'free_y') + ggtitle(v)))
    }
  }
}
profileByGraph(trainData,testData)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

getPredictiveVars = function(df,addExcludedVars=c()){
  
  excludedVars = c('sample',
                   'Survived',
                   'Name',
                   'Ticket',
                   'Cabin',
                   'cabinMain',
                   'surname',
                   'ticketPrefix',
                   'ticketNumber',
                   'passengerId',
                   addExcludedVars
                   )
  
  allVars = names(df)
  
  predictorVars = allVars[!(allVars %in% excludedVars)]
  
  predictorVars
  
}

getPredictionFormula = function(df,targetVar,addExcludedVars=c()){
  varlist = getPredictiveVars(df=df,addExcludedVars)
  
  f = paste(targetVar,'~')
  for (v in varlist){
    f = paste(f,v,'+')
  }
  f = substr(x=f,start = 0,stop = nchar(f)-1)
  as.formula(f)
}


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logisticSurvivalModel = glm(getPredictionFormula(trainData,'Survived'),data=trainData,family='binomial')
summary(logisticSurvivalModel)
roc(response = logisticSurvivalModel$y, predictor = logisticSurvivalModel$fitted.values,plot=TRUE, auc.polygon=TRUE, grid=TRUE,print.auc=TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logisticSurvivalModelReduced = step(logisticSurvivalModel,direction='backward')
summary(logisticSurvivalModelReduced)
roc(response = logisticSurvivalModelReduced$y, predictor = logisticSurvivalModelReduced$fitted.values,plot=TRUE, auc.polygon=TRUE, grid=TRUE,print.auc=TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
alias(logisticSurvivalModelReduced)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logisticSurvivalModelManual = glm(getPredictionFormula(trainData,'Survived',addExcludedVars=c('Fare',
                                                                                            'fareLn',
                                                                                            'fareAdj',
                                                                                            'Embarked',
                                                                                            'title',
                                                                                            'Sex',
                                                                                            'numPplSameTicket',
                                                                                            'familySize',
                                                                                            'SibSp',
                                                                                            'Age',
                                                                                            'Parch')),
                                                                data=trainData,
                                                                family='binomial')
logisticSurvivalModelManual = step( logisticSurvivalModelManual,direction='backward')
summary(logisticSurvivalModelManual)
roc(response = logisticSurvivalModelManual$y, predictor = logisticSurvivalModelManual$fitted.values,plot=TRUE, auc.polygon=TRUE, grid=TRUE,print.auc=TRUE)



## ----results='asis'--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
predSurvived = ifelse(logisticSurvivalModelManual$fitted.values > 0.5,'predSurvived','predDied')
pandoc.table(table(logisticSurvivalModelManual$y,predSurvived))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction <- predict(logisticSurvivalModelManual, testData,type='response')
predictionB = ifelse(prediction > 0.5,1,0)
solution <- data.frame(PassengerID = testData$PassengerId, Survived = predictionB)
write.csv(solution, file = 'logisticSurvivalModelManual_Prediction.csv', row.names = F)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bothDT = as.data.table(rbind.fill(trainData,testData))
convertToFactors(bothDT)
testData = bothDT[is.na(Survived),,]
trainData = bothDT[!is.na(Survived),,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myPredForm = getPredictionFormula(trainData,'as.factor(Survived)')
myrandomForest = randomForest(myPredForm,
                      data=trainData, 
                      importance=TRUE, 
                      ntree=200)
varImpPlot(myrandomForest)
predictions  = predict(myrandomForest, trainData,type= 'prob')[,2]
roc(response = trainData$Survived, predictor = predictions, plot=TRUE, auc.polygon=TRUE, grid=TRUE,print.auc=TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prediction <- predict(myrandomForest, testData,type='response')
solution <- data.frame(PassengerID = testData$PassengerId, Survived = prediction)
write.csv(solution, file = 'randomForestModel_Prediction.csv', row.names = F)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
myPredForm = getPredictionFormula(trainData,'as.factor(Survived)',addExcludedVars=c('Fare',                                                                                   'fareLn',
                                                                                    'fareAdj',
                                                                                    'fareLn',
                                                                                    'Sex',
                                                                                    'Pclass',
                                                                                    'title',
                                                                                    'familySize',
                                                                                    'Parch',
                                                                                    'SibSp',
                                                                                    'numPplSameTicket',                                                                               'familySize',                                                                                    'SibSp',                                                                                     'Parch',
                                                                                    'PassengerId'))
myTunedrandomForest = randomForest(myPredForm,
                      data=trainData, 
                      importance=TRUE, 
                      ntree=200)
plot(myTunedrandomForest)
varImpPlot(myTunedrandomForest)
Tunedpredictions  = predict(myTunedrandomForest, trainData,type= 'prob')[,2]
roc(response = trainData$Survived, predictor = Tunedpredictions, plot=TRUE, auc.polygon=TRUE, grid=TRUE,print.auc=TRUE)
prediction <- predict(myTunedrandomForest, testData,type='response')
solution <- data.frame(PassengerID = testData$PassengerId, Survived = prediction)
write.csv(solution, file = 'randomForestTunedModel_Prediction.csv', row.names = F)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[,.N,Survived]
library(caret)
set.seed(42)
trainData2 = upSample(as.data.frame(trainData)[,getPredictiveVars(trainData)],
                      as.factor(trainData$Survived))
table(trainData2$Class)
trainData2$Survived = trainData2$Class
myPredForm = getPredictionFormula(trainData,'as.factor(Survived)',addExcludedVars=c('Fare',                                                                                   'fareLn',
                                                                                    'fareAdj',
                                                                                    'fareLn',
                                                                                    'Sex',
                                                                                    'Pclass',
                                                                                    'fareQuantile',
                                                                                    'numPplSameTicket',                                                                               'familySize',                                                                                    'SibSp',                                                                                     'Parch',
                                                                                    'PassengerId'))
myTunedrandomForest = randomForest(myPredForm,
                      data=trainData2, 
                      importance=TRUE, 
                      ntree=200)
plot(myTunedrandomForest)
varImpPlot(myTunedrandomForest)
Tunedpredictions  = predict(myTunedrandomForest, trainData2,type= 'prob')[,2]
roc(response = trainData2$Survived, predictor = Tunedpredictions, plot=TRUE, auc.polygon=TRUE, grid=TRUE,print.auc=TRUE)
prediction <- predict(myTunedrandomForest, testData,type='response')
solution <- data.frame(PassengerID = testData$PassengerId, Survived = prediction)
write.csv(solution, file = 'randomForestResampledModel_Prediction.csv', row.names = F)




## ----logisticregressionwtihresampling--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logisticSurvivalModelManual = glm(getPredictionFormula(trainData,'Survived',addExcludedVars=c('Fare',
                                                                                            'fareLn',
                                                                                            'iSexClass',
                                                                                            'numPplSameTicket',
                                                                                            'familySize',
                                                                                            'SibSp',
                                                                                            'Parch',
                                                                                            'iSexClass')),
                                                                data=trainData2,
                                                                family='binomial')
logisticSurvivalModelManual = step( logisticSurvivalModelManual,direction='backward')
summary(logisticSurvivalModelManual)
roc(response = logisticSurvivalModelManual$y, predictor = logisticSurvivalModelManual$fitted.values,plot=TRUE, auc.polygon=TRUE, grid=TRUE,print.auc=TRUE)
prediction <- predict(logisticSurvivalModelManual, testData,type='response')
predictionB = ifelse(prediction > 0.5,1,0)
solution <- data.frame(PassengerID = testData$PassengerId, Survived = predictionB)
write.csv(solution, file = 'logisticSurvivalModelManualResampled_Prediction.csv', row.names = F)


## ----finalmodelperformances,echo=FALSE,results='asis'----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
modelPerf = as.data.table(list(c('manual logistic','random forest', 'manual random forest','resampled RF','resampled logistic'),c(0.76077,0.78947,0.76555,0.76555, 0.74641)))
names(modelPerf) = c('modelName','accuracyOnUnseenData')
#ggplot(data=modelPerf,aes(x=modelName,y=accuracyOnUnseenData,fill=modelName,colour=modelName)) + geom_bar(stat='identity') + scale_y_continuous(limits=c(0,1))
pandoc.table(modelPerf)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData$type = 'train'
testData$type = 'test'
trainData = as.data.table(rbind.fill(trainData,testData))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[,cabinLetterClean:=cabinLetter,]
trainData[cabinLetter %in% c('G','T'),cabinLetterClean:='U',]
for (cl in unique(trainData$cabinLetterClean)){
  #message(cl)
  cabinSurvTree = rpart(Survived ~ cabinNumber ,data = trainData[cabinLetterClean==cl,,])
  #rpart.plot::rpart.plot(cabinSurvTree)
  predictions = predict(cabinSurvTree,trainData[cabinLetterClean==cl,,])
  trainData[cabinLetterClean==cl,cabinSurvProb:=predictions,]
}
trainData[,cabinLetterClean:=NULL,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainData[,ticketSurvProb:=mean(Survived,na.rm=TRUE),.(ticketNumber)]

trainData[is.nan(ticketSurvProb),ticketSurvProb:=mean(trainData$Survived,na.rm=TRUE),] #fix for those without a ticket in the training set


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testData=trainData[type=='test',,]
trainData=trainData[type=='train',,]
testData[,type:=NULL,]
trainData[,type:=NULL,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
convertToFactors(trainData)
myPredForm = getPredictionFormula(trainData,'as.factor(Survived)',addExcludedVars=c('Fare',                                                                                          'fareLn',
                                                                                    'fareAdj',
                                                                                    'iSexClass',
                                                                                    'title',
                                                                                    'familySize',
                                                                                    'ticketSurvProb',
                                                                                    'Parch',
                                                                                    'SibSp',
                                                                                 'numPplSameTicket',                                                               
                                                                                    'PassengerId'))
myOverFittedRF = randomForest(myPredForm,
                      data=trainData, 
                      importance=TRUE, 
                      ntree=200)
plot(myOverFittedRF)
varImpPlot(myOverFittedRF)
OverFittedpredictions  = predict(myOverFittedRF, trainData,type= 'prob')[,2]
roc(response = trainData$Survived, predictor = OverFittedpredictions, plot=TRUE, auc.polygon=TRUE, grid=TRUE,print.auc=TRUE)
prediction <- predict(myOverFittedRF, testData,type='response')
solution <- data.frame(PassengerID = testData$PassengerId, Survived = prediction)
write.csv(solution, file = 'overfittedRF_Prediction.csv', row.names = F)


