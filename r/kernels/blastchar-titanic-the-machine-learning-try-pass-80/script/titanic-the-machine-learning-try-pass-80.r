
# supress anoing warnings for now
options(warn=-1)

# Necessary Libraries 
library(ggplot2)
library(cowplot)

library(lattice)
library(caret)
library(MASS)

# for describe the data
library(psych)

# need for the vim library 
library(colorspace)
library(grid)
library(data.table)

# for validate missing data 
library(mice) 
library(VIM)

library(dplyr)

#some color to life
library(wesanderson)

# load train.csv
DstTrain <- read.csv('../input/train.csv', stringsAsFactors = FALSE, na.strings = c("NA", ""))
# load test.csv
DstTest  <- read.csv('../input/test.csv', stringsAsFactors = FALSE, na.strings = c("NA", ""))

# set survived to NA on the test dataset
DstTest$Survived <- NA

# set the Set column 
DstTest$Set  <- "Test";
DstTrain$Set <- "Train";

# combine all the data
DstAll <- rbind(DstTrain, DstTest);

# set the TrainSet var
TrainSet <- DstAll$Set == 'Train'

# set the TestSet var
TestSet <- DstAll$Set == 'Test'

# check data sample 
head(DstAll, n=5L)

# check column type
sapply(DstAll, typeof)

# show columns info
str(DstAll)

# check the dataset dimensions
dim(DstAll)

# Describe the load data only numerics
describeBy(DstAll[,sapply(DstAll, is.numeric)], na.rm = TRUE)

# Unique values per column
apply(DstAll,2,function(x) length(unique(x)))

# Some corrections to the data
DstAll$SibSp[DstAll$PassengerId==280] <- 0
DstAll$Parch[DstAll$PassengerId==280] <- 2
DstAll$SibSp[DstAll$PassengerId==1284] <- 1
DstAll$Parch[DstAll$PassengerId==1284] <- 1

# Survived to categorical
DstAll$Survived <- factor(DstAll$Survived);

# check for the pattern of missing data
md.pattern(DstAll)

# The missing data percentage by variable (exclude Survived)
sapply(DstAll[,-c(2)], function(x) round((sum(is.na(x))/length(x)*100),2))

# Graphic view of the missing data
aggr(DstAll[,-c(1,2)], col=heat.colors(2), 
                  numbers=TRUE, sortVars=TRUE, 
     labels=names(DstTrain[,-c(1,2)]), ylab=c("Missing data","Pattern"))

# check the frequency of each option
table(DstAll$Embarked)

# lets clean it using the most common code to replace
missEmb <- is.na(DstAll$Embarked)

# replace the NA 
DstAll[missEmb,]
DstAll[missEmb,]$Embarked <- 'S'

# show result 
DstAll[missEmb,]

# Imputing the missing data
# The mice() function takes care of the imputing process
newData <- mice(DstAll,m=5,maxit=50,meth='pmm',seed=500)

# the complete clean dataset
DstAllClean <- complete(newData,1)

# Clear the survived variable
DstAllClean[DstAllClean$Set == 'Test',]$Survived <- NA

#check for missing
sapply(DstAllClean[,-c(2)], function(x) round((sum(is.na(x))/length(x)*100),2))

# Compute the largest y value used in the Age remove NA
NoNAAge <- DstAll[!is.na(DstAll$Age),c("Age")]
hist(NoNAAge, col=heat.colors(10), breaks=10, main="Original ages")

#Clean Ages dataset
hist(DstAllClean$Age, col=heat.colors(10), breaks=10, main="Clean NA ages")

# histogram of SibSp
plot.SibSp <- ggplot(DstAllClean[TrainSet,], aes(SibSp,fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    scale_fill_brewer()

# histogram of Pclass
plot.Pclass <- ggplot(DstAllClean[TrainSet,], aes(Pclass,fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    scale_fill_brewer()

# histogram of Sex
plot.Sex <- ggplot(DstAllClean[TrainSet,], aes(Sex,fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    scale_fill_brewer()

# histogram of Embarked
plot.Embarked <- ggplot(DstAllClean[TrainSet,], aes(Embarked,fill = factor(Survived))) +
    geom_histogram(stat = "count") +
    scale_fill_brewer()

# create the plot grid with all
plot_grid(plot.SibSp, plot.Pclass, plot.Sex, plot.Embarked )

# the kid factor
DstAllClean$Kid[DstAllClean$Age <= 14] <- 1
DstAllClean$Kid[DstAllClean$Age > 14] <- 0

# Kid
ggplot(DstAllClean[TrainSet,], aes(Kid,fill = factor(Survived))) + 
    geom_bar(stat = "count") +
    scale_fill_brewer()

# get the title from name
DstAllClean$Title <- gsub('(.*, )|(\\..*)', '', DstAllClean$Name)

# histogram of Title
ggplot(DstAllClean[TrainSet,], aes(Title,fill = factor(Survived))) + 
geom_histogram(stat="count") + 
theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    scale_fill_brewer()

# Titles by Sex
table(DstAllClean$Sex, DstAllClean$Title)

# Titles list
other <- c('Master', 'Dona', 'Lady', 'the Countess','Sir', 'Jonkheer', 'Don','Capt', 'Col', 'Dr', 'Major', 'Rev')

# Reassign titles
DstAllClean$Title[DstAllClean$Title == 'Mlle'] <- 'Miss' 
DstAllClean$Title[DstAllClean$Title == 'Ms'] <- 'Miss'
DstAllClean$Title[DstAllClean$Title == 'Mme'] <- 'Mrs' 
DstAllClean$Title[DstAllClean$Title %in% other] <- 'Other'

# title
ggplot(DstAllClean[TrainSet,], aes(Title,fill = factor(Survived))) + 
    geom_bar(stat = "count") +
    scale_fill_brewer()

# Family size
DstAllClean$FamilySize <- DstAllClean$SibSp + DstAllClean$Parch + 1

# Group the sizes
DstAllClean$FamilyType[DstAllClean$FamilySize == 1] <- 'Single'
DstAllClean$FamilyType[DstAllClean$FamilySize <= 3 & DstAllClean$FamilySize > 1] <- 'Small'
DstAllClean$FamilyType[DstAllClean$FamilySize > 3] <- 'Big'

# FamilySize factor
ggplot(DstAllClean[TrainSet,], aes(FamilySize,fill = factor(Survived))) + 
    geom_bar(stat = "count") +
    scale_fill_brewer()

# FamilyType factor
ggplot(DstAllClean[TrainSet,], aes(FamilyType,fill = factor(Survived))) + 
    geom_bar(stat = "count") +
    scale_fill_brewer()

# convert to factor
DstAllClean$Kid  <- factor(DstAllClean$Kid)
DstAllClean$Sex  <- factor(DstAllClean$Sex)
DstAllClean$Embarked  <- factor(DstAllClean$Embarked)
DstAllClean$Title  <- factor(DstAllClean$Title)
DstAllClean$Pclass  <- factor(DstAllClean$Pclass)
DstAllClean$FamilyType  <- factor(DstAllClean$FamilyType)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# create the test dataset with only the testing columns
varsToKeep <- c('Survived', 'Pclass','Sex','Age','SibSp','Parch','Fare','Embarked','Title','FamilyType','Kid')

# let's split the dataset into two
DstTrainTest <- DstAllClean[TrainSet,varsToKeep]
idxSplit <- createDataPartition(DstTrainTest$Survived, p = 0.75, list=FALSE)
DstTrainModel <- DstTrainTest[idxSplit,]
DstTestModel <- DstTrainTest[-idxSplit,]

# logistic regression
set.seed(7)
fit.glm <- train(Survived ~ ., data=DstTrainModel, method="glm", metric=metric, trControl=control)

# linear algorithms
set.seed(7)
fit.lda <- train(Survived ~ ., data=DstTrainModel, method="lda", metric=metric, trControl=control)

# CART
set.seed(7)
fit.cart <- train(Survived ~ ., data=DstTrainModel, method="rpart", metric=metric, trControl=control)

# kNN
set.seed(7)
fit.knn <- train(Survived ~ ., data=DstTrainModel, method="knn", metric=metric, trControl=control)

# SVM
set.seed(7)
fit.svm <- train(Survived ~ ., data=DstTrainModel, method="svmRadial", metric=metric, trControl=control)

# Random Forest
set.seed(7)
fit.rf <- train(Survived ~ ., data=DstTrainModel, method="rf", metric=metric, trControl=control)

# Gradient Boost Machine (GBM)
set.seed(7)
fit.gbm <- train(Survived ~ ., data=DstTrainModel, method="gbm", 
                 metric=metric, trControl=control, verbose=FALSE)


# summarize accuracy of models
results <- resamples(list(
    glm=fit.glm, 
    lda=fit.lda, 
    cart=fit.cart, 
    knn=fit.knn, 
    svm=fit.svm, 
    rf=fit.rf,
    gbm=fit.gbm
))
summary(results)

# compare accuracy of models
dotplot(results)

# summarize Best Model
print(fit.rf)

# prediction 
DstTestModelClean <- DstTestModel
DstTestModelClean$Survived <- NA

predictedval <- predict(fit.rf, newdata=DstTestModelClean)

# summarize results with confusion matrix
cm <- confusionMatrix(predictedval, DstTestModel$Survived)
cm$table

# calculate accuracy of the model
Accuracy<-round(cm$overall[1],2)
Accuracy
# 
# byclass
byC <- as.data.frame(cm$byClass)
names(byC) <- c("Values")
byC


# prediction 
predictedval <- predict(fit.rf, newdata=DstAllClean[TestSet,varsToKeep])

# create a csv file for submittion
TitanicResult <- data.frame(PassengerId = DstAllClean[TestSet,]$PassengerId, Survived = predictedval)
table(TitanicResult$Survived)
write.csv(TitanicResult,file = "TitanicResult.csv",row.names = FALSE)
