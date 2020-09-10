
# Necessary Libraries 
suppressMessages(library(gridExtra))
suppressMessages(library(grid))
suppressMessages(library(ggplot2))

suppressMessages(library(lattice))
suppressMessages(library(caret))

# for validate missing data 
suppressMessages(library(mice)) 
suppressMessages(library(VIM))

suppressMessages(library(dplyr))
suppressMessages(library(plyr))

suppressMessages(library(party))

# h2o modeling kit
suppressMessages(library(h2o))

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

# Unique values per column
apply(DstAll,2,function(x) length(unique(x)))

# Some corrections to the data
DstAll$SibSp[DstAll$PassengerId==280] <- 0
DstAll$Parch[DstAll$PassengerId==280] <- 2
DstAll$SibSp[DstAll$PassengerId==1284] <- 1
DstAll$Parch[DstAll$PassengerId==1284] <- 1

# The missing data percentage by variable (exclude Survived)
sapply(DstAll[,-c(2)], function(x) round((sum(is.na(x))/length(x)*100),2))

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
newData <- mice(DstAll,m=5,maxit=50,meth='pmm',seed=500, printFlag=FALSE)

# the complete clean dataset
DstAllClean <- complete(newData,1)

# Clear the survived variable
DstAllClean[DstAllClean$Set == 'Test',]$Survived <- NA

#check for missing
sapply(DstAllClean[,-c(2)], function(x) round((sum(is.na(x))/length(x)*100),2))

# set the grid size
par(mfrow=c(1,2)) # 1x2 grid 
par(pin=c(3,3)) # graph size

# Compute the largest y value used in the Age remove NA
NoNAAge <- DstAll[!is.na(DstAll$Age),c("Age")]
hist(NoNAAge, col=heat.colors(10), breaks=10, main="Original ages")

#Clean Ages dataset
hist(DstAllClean$Age, col=heat.colors(10), breaks=10, main="Clean NA ages")

# the kid factor
DstAllClean$Kid[DstAllClean$Age <= 18] <- "Yes"
DstAllClean$Kid[DstAllClean$Age > 18] <- "No"

# get the title from name
DstAllClean$Title <- gsub('(.*, )|(\\..*)', '', DstAllClean$Name)

# Titles by Sex
table(DstAllClean$Sex, DstAllClean$Title)

other <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')

DstAllClean$Title[DstAllClean$Title == 'Mlle']        <- 'Miss' 
DstAllClean$Title[DstAllClean$Title == 'Ms']          <- 'Miss'
DstAllClean$Title[DstAllClean$Title == 'Mme']         <- 'Mrs' 
DstAllClean$Title[DstAllClean$Title %in% royalty]  <- 'Royalty'
DstAllClean$Title[DstAllClean$Title %in% other]  <- 'Officer'

# Family size
DstAllClean$FamilySize <- DstAllClean$SibSp + DstAllClean$Parch + 1

# Group the sizes
DstAllClean$FamilyType[DstAllClean$FamilySize == 1] <- 'Alone'
DstAllClean$FamilyType[DstAllClean$FamilySize <= 3 & DstAllClean$FamilySize > 1] <- 'Small'
DstAllClean$FamilyType[DstAllClean$FamilySize > 3] <- 'Big'

# lets find the fare by person
DstAllClean$FareByP <- DstAllClean$Fare / DstAllClean$FamilySize

# Lets try to get the family together
DstAllClean$Surname <- sapply(DstAllClean$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
DstAllClean$FamilyID <- paste(as.character(DstAllClean$Family_size), DstAllClean$Surname, sep="")

# less than 2 so is alone for sure
DstAllClean$FamilyID[DstAllClean$FamilySize < 2] <- 'Alone'

# by frequency
famIDs <- data.frame(table(DstAllClean$FamilyID))
famIDs <- famIDs[famIDs$Freq < 2,]

# some are also alone
DstAllClean$FamilyID[DstAllClean$FamilyID %in% famIDs$Var1] <- 'Alone'

# default is No 
DstAllClean$IsMother <- 'No'
DstAllClean$IsMother[DstAllClean$Sex == 'female' & DstAllClean$Parch > 0 & DstAllClean$Age > 18 & DstAllClean$Title != 'Miss'] <- 'Yes'

# create the goupping function with the define intervals
CreateGrp <- function(val){
    if (val >= 0 & val <= 20){
        return('0-20')
    }else if(val > 20 & val <= 30){
        return('20-30')
    }else if (val > 30 & val <= 40){
        return('30-40')
    }else if (val > 40 & val <=50){
        return('40-50')
    }else if (val > 50 & val <=60){
        return('50-60')
    }else if (val > 60){
        return('> 60')
    }
}
# apply the Group function to the age column
DstAllClean$AgeGrp <- sapply(DstAllClean$Age,CreateGrp)
# check the frequency for each group bin
table(DstAllClean$AgeGrp)

# my plots will be very similar lets simplify using a function for it
createplot <- function(dst, column, name) {
    plt <- ggplot(dst, aes(x=column, fill = factor(Survived))) + 
        ggtitle(name) + 
        xlab(name) +
        ylab("Percentage")  +
        geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.7) + 
        theme_minimal() +
        theme(legend.position="none", axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_manual(values=c("#B22222","#D0D0D0"))
    return(plt)
}

dstPlot <- DstAllClean[TrainSet,]

# Plot 1
p1 <- createplot(dstPlot, dstPlot$Title, "Title")                      
# plot 2
p2 <- createplot(dstPlot, dstPlot$Kid, "Kid")
# plot 3
p3 <- createplot(dstPlot, dstPlot$IsMother, "Is Mother")
# plot 4
p4 <- createplot(dstPlot, dstPlot$FamilyType, "Family Type")
# plot 5
p5 <- createplot(dstPlot, dstPlot$Sex, "Sex")
# plot 6
p6 <- createplot(dstPlot, dstPlot$Pclass, "Class")

# draw the plot grid
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)

createplot(dstPlot, dstPlot$AgeGrp, "Group of Ages")     

h2o.init(nthreads=-1, max_mem_size="6G")
h2o.removeAll()    # clean slate - just in case the cluster was already running
h2o.no_progress()  # Don't show progress bars in RMarkdown output

sapply(DstAllClean[,-c(2)], function(x) round((sum(is.na(x))/length(x)*100),2))

keepVars <- c("Survived","Pclass","Sex","Parch","Fare","Title","FamilySize","FamilyType","FareByP","FamilyID","Kid")
# vars to drop
ToKeep <- which(names(DstAllClean) %in% keepVars)

# convert all factor or character columns to numeric
toNumeric <- function(dst) {
    cols <- colnames(dst)
    for (c in cols) {
      if ((class(dst[[c]])=="factor") || (class(dst[[c]])=="character")) {
        levels <- unique(dst[[c]])
        dst[[c]] <- as.numeric(factor(dst[[c]], levels=levels))
      }
    }
    toNumeric <- dst
}

DstTrainTest <- toNumeric(DstAllClean[TrainSet,ToKeep])

head(DstTrainTest)

# For binary classification, response should be a factor
DstTrainTest$Survived <- as.factor(DstTrainTest$Survived)
levels(DstTrainTest$Survived) = make.names(unique(DstTrainTest$Survived))

# convert to h2o frame 
h2o_TrainAux = as.h2o(DstTrainTest)

# define the splits
h2o_splits <- h2o.splitFrame(h2o_TrainAux, 0.75, seed=1234)
h2o_DstTrain  <- h2o.assign(h2o_splits[[1]], "train.hex") # 75%
h2o_DstTest  <- h2o.assign(h2o_splits[[2]], "test.hex") # 25%

# Identify predictors and response
response <- "Survived"
predictors <- setdiff(names(h2o_DstTrain), response)

# Number of CV folds (to generate level-one data for stacking)
cvfolds <- 5

# Train & Cross-validate a GBM
my_gbm <- h2o.gbm(x = predictors,
                  y = response,
                  training_frame = h2o_DstTrain,
                  distribution = "bernoulli",
                  max_depth = 3,
                  min_rows = 2,
                  learn_rate = 0.2,
                  nfolds = cvfolds,
                  fold_assignment = "Modulo",
                  keep_cross_validation_predictions = TRUE,
                  seed = 1)

# Train & Cross-validate a RF
my_rf <- h2o.randomForest(x = predictors,
                          y = response,
                          training_frame = h2o_DstTrain,
                          nfolds = cvfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)

# Train & Cross-validate a DNN
my_dl <- h2o.deeplearning(x = predictors,
                          y = response,
                          training_frame = h2o_DstTrain,
                          l1 = 0.001,
                          l2 = 0.001,
                          hidden = c(200, 200, 200),
                          nfolds = cvfolds,
                          fold_assignment = "Modulo",
                          keep_cross_validation_predictions = TRUE,
                          seed = 1)


# Train & Cross-validate a (shallow) XGB-GBM
my_xgb1 <- h2o.xgboost(x = predictors,
                       y = response,
                       training_frame = h2o_DstTrain,
                       distribution = "bernoulli",
                       ntrees = 50,
                       max_depth = 3,
                       min_rows = 2,
                       learn_rate = 0.2,
                       nfolds = cvfolds,
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)


# Train & Cross-validate another (deeper) XGB-GBM
my_xgb2 <- h2o.xgboost(x = predictors,
                       y = response,
                       training_frame = h2o_DstTrain,
                       distribution = "bernoulli",
                       ntrees = 50,
                       max_depth = 8,
                       min_rows = 1,
                       learn_rate = 0.1,
                       sample_rate = 0.7,
                       col_sample_rate = 0.9,
                       nfolds = cvfolds,
                       fold_assignment = "Modulo",
                       keep_cross_validation_predictions = TRUE,
                       seed = 1)


# Train a stacked ensemble using the H2O and XGBoost models from above
base_models <- list(my_gbm@model_id, my_rf@model_id, my_dl@model_id,  
                    my_xgb1@model_id, my_xgb2@model_id)

ensemble <- h2o.stackedEnsemble(x = predictors,
                                y = response,
                                training_frame = h2o_DstTrain,
                                base_models = base_models)

# Eval ensemble performance on a test set
perf <- h2o.performance(ensemble, newdata = h2o_DstTest)


# Compare to base learner performance on the test set
get_auc <- function(mm) h2o.auc(h2o.performance(h2o.getModel(mm), newdata = h2o_DstTest))
baselearner_aucs <- sapply(base_models, get_auc)
baselearner_best_auc_test <- max(baselearner_aucs)
ensemble_auc_test <- h2o.auc(perf)

print(sprintf("Best Base-learner Test AUC:  %s", baselearner_best_auc_test))
print(sprintf("Ensemble Test AUC:  %s", ensemble_auc_test))

# convert to h2o frame 
#h2o_FinalTest = as.h2o(DstTest[,ToDropTest])
h2o_FinalTest = as.h2o(toNumeric(DstAllClean[TestSet,ToKeep]))

# predict with the model
predictFinal <- h2o.predict(ensemble, h2o_FinalTest)

# convert H2O format into data frame and save as csv
predictFinal.df <- as.data.frame(predictFinal)

predictFinal.df$Survived <- as.factor(mapvalues(predictFinal.df$predict,c("X0", "X1"),c(0,1)))

# create a csv file for submittion
TitanicResult <- data.frame(PassengerId = DstAllClean[TestSet,]$PassengerId, Survived = predictFinal.df$Survived)
head(TitanicResult,n=5L)
# write the submition file
write.csv(TitanicResult,file = "Result.csv",row.names = FALSE)
table(TitanicResult$Survived)
