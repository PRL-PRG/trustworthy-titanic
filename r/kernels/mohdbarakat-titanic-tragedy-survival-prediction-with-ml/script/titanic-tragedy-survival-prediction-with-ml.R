## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(knitr)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)
library(dplyr)
library(qdap)
library(pROC)
library(caret)
library(qdap)
library(tm)
library(wordcloud)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
allSet <- bind_rows(train, test) # combining train and test data in one set


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainDim <- dim(train)
testDim <- dim(test)
trainR <- trainDim[1];testR <- testDim[1];allSetC <- dim(allSet)[2]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(allSet)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kable(head(allSet))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ifelse(length(unique(allSet[,1])) == nrow(allSet),"No duplicates","Duplicates detected!")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allSet$Survived <- as.factor(allSet$Survived)
allSet$Pclass <- as.factor(allSet$Pclass)
allSet$Sex <- as.factor(allSet$Sex)
allSet$Cabin <- as.factor(allSet$Cabin)
allSet$Embarked <- as.factor(allSet$Embarked)


## ----missingValues---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# replace missing values with NA across all features
for (i in 1:allSetC){
  allSet[,i][allSet[,i]== ""] <- NA
}

# define a function to get number of NAs in each feature
getNA <- function(dt,NumCol){
       varsNames <- names(dt)
        NAs <- 0

        for (i in 1:NumCol){
          NAs <- c(NAs, sum(is.na(dt[,i])))
        }

        NAs <- NAs[-1]
        names(NAs)<- varsNames # make a vector of variable name and count of NAs

        NAs <- NAs[NAs > 0]
        NAs 
}

getNA(allSet,allSetC)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allSet[,c("PassengerId","Pclass","Fare","Embarked")] %>% filter(is.na(Embarked))


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# filter for complete embarkation records
FareClassComp <- allSet %>% filter(!is.na(Embarked))

# plot embarkation ports versus fare mapped by passenger class
FareClassComp %>% 
        ggplot(aes(x = Embarked, y = Fare, fill = Pclass))+
        geom_boxplot()+
        geom_hline(aes(yintercept = 80),
                   colour = "red", linetype = "dashed", lwd = 2)+
        scale_y_continuous(labels = dollar_format())+
        theme_few()



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# impute missing embarkation values for both passengers
allSet$Embarked[is.na(allSet$Embarked)] <- "C"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allSet[,c("PassengerId","Pclass","Fare","Embarked")] %>% filter(is.na(Fare))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allSet$Fare[allSet$PassengerId == 1044] <-  median(allSet$Fare[allSet$Pclass == 3 & allSet$Embarked == "S"], na.rm = T)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allSet$Title <- gsub("(.*, )|(\\..*)","",allSet$Name)

# tabulate titles versus sex
table(allSet$Sex, allSet$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MrTitles <- c("Capt", "Col", "Don", "Jonkheer", "Major", "Master", "Mr", "Rev", "Sir")
MrsTitles <- c("Dona", "Lady", "Mme", "Mrs", "the Countess")
MissTitles <- c("Miss", "Mlle", "Ms")

allSet$Title[allSet$Title %in% MrTitles] <- "Mr"
allSet$Title[allSet$Title %in% MrsTitles] <- "Mrs"
allSet$Title[allSet$Title %in% MissTitles] <- "Miss"
allSet$Title[allSet$Title == "Dr" & allSet$Sex == "female"] <- "Mrs"
allSet$Title[allSet$Title == "Dr" & allSet$Sex == "male"] <- "Mr"

allSet$Title <- as.factor(allSet$Title)
table(allSet$Title)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allSet$FamSz <- allSet$SibSp + allSet$Parch + 1


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allSet$FamSzCat[allSet$FamSz == 1] <- "Singles"
allSet$FamSzCat[allSet$FamSz > 1 & allSet$FamSz <5] <- "Small"
allSet$FamSzCat[allSet$FamSz > 4] <- "Large"

allSet$FamSzCat <- as.factor(allSet$FamSzCat)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allSet$Surname <- sapply(allSet$Name, function(x) strsplit(x, split = "[,]")[[1]][1])
paste(nlevels(factor(allSet$Surname)), "families were onboard Titanic")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
allSet$AgeStg[allSet$Age < 18 & !is.na(allSet$Age)] <- "Child"
allSet$AgeStg[allSet$Age >= 18 & !is.na(allSet$Age)] <- "Adult"


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
length(allSet$AgeStg[is.na(allSet$AgeStg)])


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a vector with the prospect features including AgeStg and PassengerId
varsNames <- c("PassengerId","Pclass", "Sex", "SibSp", "Parch", "Fare", "FamSz", "FamSzCat", "AgeStg")

# subset the data by the selected features
allSetAgeStg <- allSet[,varsNames]

# subset into two sets: one with age stage complete, and one with age stage missing
allSetAgeStgComp <- allSetAgeStg[!is.na(allSetAgeStg$AgeStg),]
allSetAgeStgMiss <- allSetAgeStg[is.na(allSetAgeStg$AgeStg),]

# split the data set with complete age stage into train and test data sets (75/25 ratio)
## number of training rows
nTrain <- 0.75 * nrow(allSetAgeStgComp)

## sample row IDs
set.seed(3030)
sampleTrain <- sample(nrow(allSetAgeStgComp),nTrain)

## create train and test data sets
AgeStgTrain <- allSetAgeStgComp[sampleTrain,]
AgeStgTest <- allSetAgeStgComp[-sampleTrain,]

# use the glm Logistic Regression model to predict the age stage. Use Forward Stepwise algorithm to select the best predictors.

# build the null model with no predictors
set.seed(3030)
null_model <- glm(factor(AgeStg)~1, data = AgeStgTrain, family = "binomial")

# build the full model with all predictors
set.seed(3030)
full_model <- glm(factor(AgeStg)~Pclass+Sex+SibSp+Parch+Fare+FamSz+FamSzCat, data = AgeStgTrain, family = "binomial")

# perform forward stepwise algorithm to get an economic model with best predictors
step_model <- step(null_model, scope = list(lower= null_model,upper = full_model),direction = "forward")

# estimate the stepwise age stage probability in training and testing data
AgeStgTrain$stepProb <- predict(step_model, data = AgeStgTrain, type = "response")
AgeStgTest$stepProb <- predict(step_model, newdata = AgeStgTest, type = "response")

# create the ROC curve of the stepwise for training and testing data
ROC_train <- roc(AgeStgTrain$AgeStg,AgeStgTrain$stepProb)
ROC_test <- roc(AgeStgTest$AgeStg,AgeStgTest$stepProb)

# Plot the ROC of the stepwise model: training and testing
plot(ROC_train,col = "red")
plot(ROC_test,col = "red")

# calculate Area Under the Curve (AUC): training and testing
auc(ROC_train);auc(ROC_test)
trainAcc <- percent(auc(ROC_train));testAcc <- percent(auc(ROC_test))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# find the average number of children to set it as threshold of probability
AvgChildCount <- mean(AgeStgTrain$AgeStg == "Child")

# Predict Age Stage in testing data if its probability is greater than the average 
AgeStgTest$AgeStgPred <- ifelse(AgeStgTest$stepProb > AvgChildCount,"Child", "Adult")

# check accuracy of prediction in testing data
acc <- percent(mean(AgeStgTest$AgeStg == AgeStgTest$AgeStgPred))
acc


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# predicting missing Age Stage using the stepwise, logistic regression model
allSetAgeStgMiss$stepProb <- predict(step_model, newdata = allSetAgeStgMiss, type = "response")

allSetAgeStgMiss$AgeStg <- ifelse(allSetAgeStgMiss$stepProb > AvgChildCount,"Child", "Adult")

# update missing Age Stage in the full data
allSet <- left_join(allSet,allSetAgeStgMiss[,c("PassengerId","AgeStg")], by = "PassengerId", allSet.x = TRUE, allSet.y = FALSE)

allSet$AgeStg <- ifelse(is.na(allSet$AgeStg.x),allSet$AgeStg.y,allSet$AgeStg.x)
allSet <- allSet[,!colnames(allSet) %in% c("AgeStg.x","AgeStg.y")]   

allSet$AgeStg <- as.factor(allSet$AgeStg)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
getNA(allSet,length(allSet))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train <- allSet[!is.na(allSet$Survived),]
test <- allSet[is.na(allSet$Survived),]


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% 
ggplot(aes(x=Survived, fill = Survived))+
        geom_histogram(stat = "count")+
        labs(x = "Survival in the Titanic tragedy")+
        geom_label(stat='count',aes(label=..count..))+
        labs(fill = "Survival (0 = died, 1 = survived)")


## ---- warning = FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
survSumy <- summary(train$Survived)
died <- survSumy[[1]][1];suvd <- survSumy[[2]][1];surPerc <- percent(suvd/sum(suvd,died))


## ---- warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
p1 <- ggplot(train,aes(x=Survived, fill=Pclass))+
  geom_histogram(stat = "count")+
        labs(x = "P1: Survival vs Class")

p2 <- ggplot(train,aes(x=Survived, fill=Sex))+
  geom_histogram(stat = "count")+
        labs(x = "P2: Survival vs Sex")

p3 <- ggplot(train,aes(x= Survived, fill = AgeStg))+
  geom_histogram(stat = "count", position = "dodge")+
        labs(x = "P3: Survival vs Age Stage")

p4 <- ggplot(train,aes(x=Survived, fill=Embarked))+
  geom_histogram(stat = "count")+
        labs(x = "P4: Survival vs Embarkment Port")

p5 <- ggplot(train,aes(x= Survived, y = Fare))+
  geom_boxplot()+
        labs(x = "P5: Survival vs Fare")

p6 <- ggplot(train,aes(x= Survived, fill = FamSzCat))+
  geom_histogram(stat = "count")+
        labs(x = "P6: Survival vs Category of Family Size")

p7 <- ggplot(train, aes(x = FamSz, fill = Survived)) +
        geom_bar(stat='count', position='dodge') +
        scale_x_continuous(breaks=c(1:11)) +
        labs(x = 'P7: Survival vs Family Size')

grid.arrange(p1,p2,p3,p4,p5,p6,p7,ncol=2)



## ---- warning= FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
surN <- train %>% 
        group_by(Surname) %>% 
        summarize(count = n()) %>% 
        arrange(desc(count))

countSurN <- nrow(surN)
        
surN[1:30,]%>% 
ggplot(aes(x=reorder(Surname, count),y=count))+
        geom_bar(stat = "identity")+
        scale_y_continuous(breaks = c(1:10))+
        labs(x = "Surnames", y = "Number of Passengers")+
        coord_flip()


## ---- warning= FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(4040)
Embk_source <- VectorSource(train$Surname)
 
Embk_corp <- VCorpus(Embk_source)

Embk_tdm <- TermDocumentMatrix(Embk_corp)
Embk_m <- as.matrix(Embk_tdm)

term_freq <- rowSums(Embk_m)
term_freq <- sort(term_freq, decreasing = TRUE)
word_freqs <- data.frame(term = names(term_freq), num = term_freq)

wordcloud(word_freqs$term, word_freqs$num, max.words = 100, colors = "blue")



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# create a vector with the selected features
selVarsNames <- c("Pclass", "Sex", "SibSp", "Parch", "Fare","Embarked" ,"Title", "FamSzCat", "AgeStg")

nearZeroVar(train[,selVarsNames], saveMetrics = TRUE)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# split the train data set into train and test data sets (75/25 ratio).

## number of training rows
nTrain <- round(0.75 * nrow(train))

## sample row IDs
sampleTrain <- sample(nrow(train),nTrain)

## create trainTemp and testTemp data sets
trainTemp <- train[sampleTrain,]
testTemp <- train[-sampleTrain,]


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2020)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
modelRF <- train(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+Title+FamSzCat+AgeStg, data = trainTemp, method = "rf", trControl = control)

print(modelRF)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2020)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
modelGBM <- train(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+Title+FamSzCat+AgeStg, data = trainTemp, method = "gbm", trControl = control, verbose = FALSE)

print(modelGBM)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(2020)

control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
modelSVM <- train(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+Title+FamSzCat+AgeStg, data = trainTemp, method = "svmRadial", trControl = control)

print(modelSVM)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# collect resamples
results <- resamples(list(RF=modelRF, GBM=modelGBM, SVM = modelSVM))

# summarize the distributions
compSummary <- summary(results)
compSummary

modelRFAcc <- percent(median(compSummary$values$`RF~Accuracy`))
modelGBMAcc <- percent(median(compSummary$values$`GBM~Accuracy`))
modelSVMAcc <- percent(median(compSummary$values$`SVM~Accuracy`))

# boxplots of results
bwplot(results)

# dot plots of results
dotplot(results)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rfPred<-predict(modelRF,testTemp)
rfCM<-confusionMatrix(rfPred,testTemp$Survived)
rfCM

modelRFacc<-percent(as.numeric(rfCM$overall[1]))
modelRFerr<-percent(1-(as.numeric(rfCM$overall[1])))

modelRFacc;modelRFerr


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gbmPred<-predict(modelGBM,testTemp)
gbmCM<-confusionMatrix(gbmPred,testTemp$Survived)
gbmCM

modelGBMacc<-percent(as.numeric(gbmCM$overall[1]))
modelGBMerr<-percent(1-(as.numeric(gbmCM$overall[1])))

modelGBMacc;modelGBMerr


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
svmPred<-predict(modelSVM,testTemp)
svmCM<-confusionMatrix(svmPred,testTemp$Survived)
svmCM

modelSVMacc<-percent(as.numeric(svmCM$overall[1]))
modelSVMerr<-percent(1-(as.numeric(svmCM$overall[1])))

modelSVMacc;modelSVMerr


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tblAcc <- data.frame("Accuracy"=c(modelRFacc, modelGBMacc, modelSVMacc), "Error"= c(modelRFerr,modelGBMerr,modelSVMerr), row.names = c("RF","GBM","SVM"))

tblAcc


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kaggle_test <- predict(modelSVM, test)

kaggle_Submit <- data.frame(PassengerId = test$PassengerId, Survived = kaggle_test)

subSummary <- table(kaggle_Submit$Survived)
subSummary

survPerc <- percent(subSummary[2]/(subSummary[1]+subSummary[2]))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
write.csv(kaggle_Submit, file = "Titanic.csv", row.names = FALSE)

