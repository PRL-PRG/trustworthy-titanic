## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----ini-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Informs the process start date/time
dateIni <- Sys.time()
cat("\n Start of execution: ", as.character(dateIni))


## ----libraries, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Lists the libraries that will be used
libs = c("data.table","ggplot2","randomForest","ROSE","DMwR","corrplot","caret",
         "xgboost","e1071","PRROC","klaR","dplyr","tidyr")
# Loads or installs the package
for(i in libs)
{
  if(i %in% row.names(installed.packages()) == FALSE){
    cat("Load/install the library: ", i, "\n\n")
    install.packages(i, repos = "http://cran.us.r-project.org")
    library(i, character.only = TRUE)
  } else {
    cat("Load the library: ", i, "\n\n")
    library(i, character.only = TRUE)
  }
}


## ----load------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
trainPath <- "../input/train.csv"
testPath  <- "../input/test.csv"
trainOri <- read.csv(trainPath, na.strings = c("NA","NaN", ""))
testOri <- read.csv(testPath, na.strings = c("NA","NaN", ""))


## ----createModels, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
createModels <- function(formula, 
                         data, 
                         methods, 
                         ctrl, 
                         preProcess = NULL,
                         tuneLength = 1) {
  
  # Suppresses the warning messages
  options(warn=-1)
  
  # Sets metric and seed
  metric <- "ROC"
  seed   <- 54321
  
  # Transforms the variable into type formula
  formula <- as.formula(formula)
  
  # Creates lists to store models
  modelsOri    <- list()
  modelsUp     <- list()  
  modelsDown   <- list()
  modelsRose   <- list()
  modelsSmote  <- list()
  
  # Creates a dataframe to store the performances
  score <- data.frame(method=character(),
                      score=numeric(),
                      model=character(), 
                      stringsAsFactors=FALSE)
  
  # Generates 5 models (original, up, down, ROSE and SMOTE) for each informed method
  for (i in methods) {
    
    tryCatch({
      
      cat("METHOD: ", i, '\n\n')
      
      #################################################################
      #         MODEL 1 - Original - No resampling technique          #
      #################################################################
      ctrl$sampling <- NULL
      nameOri <- paste0(i,".ori")
      set.seed(seed)
      modelsOri[[nameOri]] <- train(form = formula, 
                                    data = data, 
                                    method = i, 
                                    metric = metric,
                                    preProcess = preProcess,
                                    tuneLength = tuneLength,
                                    trControl = ctrl)
      cat('Importance of variables \n\n')
      tryCatch({
        importanceOri <- varImp(modelsOri[[nameOri]], scale=FALSE)
        print(importanceOri)
        # Displays the plot with the importance of the variables
        print(plot(importanceOri))
      }, error=function(e){
        cat("It wasn't possible to verify the importance of the variables: ",i, " - ERROR :",conditionMessage(e),"\n")})
      # Print the score
      cat("\n\n PERFORMANCE - ORIGINAL MODEL: \n\n")
      print(getTrainPerf(modelsOri[[nameOri]]))
      
      # Save the model
      nameFile <- paste0(nameOri,".model.rds")
      saveRDS(modelsOri[[nameOri]],paste0("./",nameFile))
      
      # Store the score in the dataframe
      score[nrow(score) + 1,] = list(nameOri,
                                     getTrainPerf(modelsOri[[nameOri]])[, "TrainROC"],
                                     nameFile)
      
      #################################################################
      #              MODEL 2 - UP (Oversampling)                      #
      #################################################################
      ctrl$sampling <- "up"
      
      nameUp <- paste0(i,".up")
      set.seed(seed)
      modelsUp[[nameUp]] <- train(form = formula, 
                                  data = data, 
                                  method = i, 
                                  metric = metric,
                                  preProcess = preProcess,
                                  tuneLength = tuneLength,
                                  trControl = ctrl)
      
      # Print the score
      cat("\n\n PERFORMANCE - UP MODEL: \n\n")
      print(getTrainPerf(modelsUp[[nameUp]]))
      
      # Save the model
      nameFile <- paste0(nameUp,".model.rds")
      saveRDS(modelsUp[[nameUp]],paste0("./",nameFile))
      
      # Store the score in the dataframe
      score[nrow(score) + 1,] = list(nameUp,
                                     getTrainPerf(modelsUp[[nameUp]])[, "TrainROC"],
                                     nameFile)
      
      #################################################################
      #                 MODEL 3 - DOWN (Undersampling)                #
      #################################################################
      ctrl$sampling <- "down"
      
      nameDown <- paste0(i,".down")
      set.seed(seed)
      modelsDown[[nameDown]] <- train(form = formula, 
                                      data = data, 
                                      method = i,
                                      metric = metric,
                                      preProcess = preProcess,
                                      tuneLength = tuneLength,
                                      trControl = ctrl)
      
      # Print the score
      cat("\n\n PERFORMANCE - DOWN MODEL: \n\n")
      print(getTrainPerf(modelsDown[[nameDown]]))
      
      # Save the model
      nameFile <- paste0(nameDown,".model.rds")
      saveRDS(modelsDown[[nameDown]],paste0("./",nameFile))
      
      # Store the score in the dataframe
      score[nrow(score) + 1,] = list(nameDown,
                                     getTrainPerf(modelsDown[[nameDown]])[, "TrainROC"],
                                     nameFile)
      
      
      ##########################################################################
      #              MODEL 4 - ROSE (Random Over-Sampling Examples)            #
      ##########################################################################
      ctrl$sampling <- "rose"
      
      nameRose <- paste0(i,".rose")
      set.seed(seed)
      modelsRose[[nameRose]] <- train(form = formula, 
                                      data = data, 
                                      method = i, 
                                      metric = metric,
                                      preProcess = preProcess,
                                      tuneLength = tuneLength,
                                      trControl = ctrl)
      
      # Print the score
      cat("\n\n PERFORMANCE - ROSE MODEL: \n\n")
      print(getTrainPerf(modelsRose[[nameRose]]))
      
      # Save the model
      nameFile <- paste0(nameRose,".model.rds")
      saveRDS(modelsRose[[nameRose]],paste0("./",nameFile))
      
      # Store the score in the dataframe
      score[nrow(score) + 1,] = list(nameRose,
                                     getTrainPerf(modelsRose[[nameRose]])[, "TrainROC"],
                                     nameFile)
      
      #######################################################################################
      #             MODEL 5 - SMOTE (Synthetic Minority Oversampling Technique)             #
      #######################################################################################
      ctrl$sampling <- "smote"
      
      nameSmote <- paste0(i,".smote")
      set.seed(seed)
      modelsSmote[[nameSmote]] <- train(form = formula, 
                                        data = data, 
                                        method = i, 
                                        metric = metric,
                                        preProcess = preProcess,
                                        tuneLength = tuneLength,
                                        trControl = ctrl)
      
      # Print the score
      cat("\n\n PERFORMANCE - SMOTE MODEL: \n\n")
      print(getTrainPerf(modelsSmote[[nameSmote]]))
      
      # Save the model
      nameFile <- paste0(nameSmote,".model.rds")
      saveRDS(modelsSmote[[nameSmote]],paste0("./",nameFile))
      
      # Store the score in the dataframe
      score[nrow(score) + 1,] = list(nameSmote,
                                     getTrainPerf(modelsSmote[[nameSmote]])[, "TrainROC"],
                                     nameFile)
      
      ###################################################################
      #  Evaluates the result of the original model and the resamplings #
      ###################################################################
      models <- list(original = modelsOri[[nameOri]],
                     down = modelsDown[[nameDown]],
                     up = modelsUp[[nameUp]],
                     smote = modelsSmote[[nameSmote]],
                     rose = modelsRose[[nameRose]])
      
      #Remove null values, if exists
      models[sapply(models, is.null)] <- NULL
      
      cat("EVALUATE THE MODELS USING THE ROC METRIC \n\n")
      resampling <- resamples(models)
      print(summary(resampling, metric = metric))
      
      cat("DOTPLOT \n\n")
      scales <- list(x=list(relation="free"), y=list(relation="free"))
      print(dotplot(resampling, scales=scales, main=paste("Evaluating all models of the method",i)))
      
    }, error=function(e){
      cat("It wasn't possible to train the model ", i, " - ERROR :",conditionMessage(e), "\n")          
      
    })
    
  }
  
  ################################################################
  #                   Evaluate all models                        #
  ################################################################
  
  # Concatenates all generated models
  modelsList <- c(modelsOri,modelsDown,modelsUp,modelsSmote,modelsRose)
  
  # Only if you have more than one method does the overall evaluation
  if(length(modelsOri) > 1){
    
    cat("\n\n EVALUATING THE RESULT OF ALL METHODS AND MODELS \n\n")
    
    resampling <- resamples(modelsList)
    print(summary(resampling, metric = metric))
    
    scales <- list(x=list(relation="free"), y=list(relation="free"))
    print(dotplot(resampling, scales=scales, main="Evaluating all methods used"))
  }
  
  cat("\n\n MODEL WITH THE BEST PERFORMANCE: \n\n")
  best <- score %>% top_n(1, score) %>% head(1)
  print(best)
  
  # Delete all models except winner
  rdsFiles = list.files(pattern='.rds')
  rdsFiles <- rdsFiles [! rdsFiles %in% best$model]
  file.remove(rdsFiles)
  # Returns the name of the best model
  return(best$model)
  
}


## ----strTrain--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## Train
glimpse(trainOri)
sapply(trainOri, class)
head(trainOri,5)
## Test
glimpse(testOri)
sapply(testOri, class)
head(testOri,5)


## ----summary---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(trainOri)
summary(testOri)


## ----checkNA---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Train
sapply(trainOri, function(x) round(sum(is.na(x))/nrow(trainOri) * 100,1))
# Test
sapply(testOri, function(x) round(sum(is.na(x))/nrow(testOri) * 100,1))


## ----balanc----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
cbind(freq=table(trainOri$Survived), percent=round(prop.table(table(trainOri$Survived))*100,1))


## ----unique----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Train
apply(trainOri,2,function(x) length(unique(x)))
# Test
apply(trainOri,2,function(x) length(unique(x)))


## ----fullData--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
testOri$Survived <- NA;
fullData <- rbind(trainOri, testOri)
trainIdx <- seq(nrow(trainOri)) #Training data index


## ----plotSexSurvived-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(fullData[trainIdx,], aes(Sex, fill = factor(Survived))) + 
  geom_bar(stat = "count", position = 'dodge')+
  xlab("Sex") +
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Sex X Survived")


## ----plotPclassSurvived----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(fullData[trainIdx,], aes(Pclass, fill = factor(Survived))) + 
  geom_bar(stat = "count")+
  xlab("Pclass") +
  ylab("Count") +
  scale_fill_discrete(name = "Survived") + 
  ggtitle("Pclass X Survived")


## ----hist------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
numberCols <- dplyr::select_if(trainOri, is.numeric)
par(mfrow=c(2,2))
for(i in 1:7) {
  hist(numberCols[,i], main=names(numberCols)[i], xlab = "")
}


## ----boxplot---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
for(i in 1:7) {
  boxplot(numberCols[,i], main=names(numberCols)[i])
}


## ----barplot---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
par(mfrow=c(2,2))
for(i in 1:7) {
  barplot(table(numberCols$Survived,numberCols[,i]), 
          main=names(numberCols)[i], 
          legend.text=unique(numberCols$Survived))
}


## ----removeNAs-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Age and Fare
fullTemp <- fullData %>% 
            group_by(Pclass) %>%
            mutate(Age = ifelse(is.na(Age), round(mean(Age, na.rm = TRUE)), Age)) %>%
            mutate(Fare = ifelse(is.na(Fare), round(mean(Fare, na.rm = TRUE)), Fare))
fullData$Age <- fullTemp$Age
fullData$Fare <- fullTemp$Fare
#Embarked 
maxEmbarked <- names(sort(table(fullData$Embarked),decreasing = T)[1])
fullData$Embarked[is.na(fullData$Embarked)] <- maxEmbarked


## ----ohe-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Sex
dummies <- predict(dummyVars(~ Sex, data = fullData), newdata = fullData)
fullData <- cbind(fullData,dummies)
# Embarked
dummies <- predict(dummyVars(~ Embarked, data = fullData), newdata = fullData)
fullData <- cbind(fullData,dummies)
# Pclass
fullData$Pclass <- factor(fullData$Pclass)
dummies <- predict(dummyVars(~ Pclass, data = fullData), newdata = fullData)
fullData <- cbind(fullData,dummies)


## ----newVars---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Title
fullData$Title <- gsub('(.*, )|(\\..*)', '', fullData$Name)
## Create only one category for similar titles
officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')
fullData$Title[fullData$Title == 'Mlle'] <- 'Miss' 
fullData$Title[fullData$Title == 'Ms']   <- 'Miss' 
fullData$Title[fullData$Title == 'Mme']  <- 'Mrs' 
fullData$Title[fullData$Title %in% royalty]  <- 'Royalty'
fullData$Title[fullData$Title %in% officer]  <- 'Officer'
## One-hot-enconding
fullData$Title <- factor(fullData$Title)
dummies <- predict(dummyVars(~ Title, data = fullData), newdata = fullData)
fullData <- cbind(fullData,dummies)
#FamilyType
fullData$FamilySize <- fullData$SibSp + fullData$Parch + 1
fullData$FamilyType[fullData$FamilySize == 1] <- 'A' #Alone
fullData$FamilyType[fullData$FamilySize > 1 & fullData$FamilySize < 5] <- 'S' #Small
fullData$FamilyType[fullData$FamilySize >= 5] <- 'B' #Big
## One-hot-enconding
fullData$FamilyType <- factor(fullData$FamilyType)
dummies <- predict(dummyVars(~ FamilyType, data = fullData), newdata = fullData)
fullData <- cbind(fullData,dummies)


## ----plotTitleSurvived-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(fullData[trainIdx,], aes(Title,fill = factor(Survived))) +
  geom_bar(stat = "count")+
  xlab('Title') +
  ylab("Count") +
  scale_fill_discrete(name = " Survived") + 
  ggtitle("Title X Survived")


## ----plotFamilyTypeSurvived------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(fullData[trainIdx,], aes(FamilyType,fill = factor(Survived))) +
  geom_bar(stat = "count")+
  xlab('FamilyType') +
  ylab("Count") +
  scale_fill_discrete(name = " Survived") + 
  ggtitle("FamilyType X Survived")


## ----removeColumns---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fullData$Ticket <- NULL
fullData$Cabin <- NULL
fullData$Sex <- NULL
fullData$Embarked <- NULL
fullData$Pclass <- NULL
fullData$Title <- NULL
fullData$Name <- NULL
fullData$FamilyType <- NULL
fullData$FamilySize <- NULL
glimpse(fullData)


## ----correlacao, fig.width = 17, fig.height = 15, fig.align = "center"-----------------------------------------------------------------------------------------------------------------------------------------------------------
cor(fullData[trainIdx,]) %>% corrplot(addCoef.col = "grey", number.cex = 1.4)


## ----split-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Selecting the training variables, the "PassengerId" will not be used because it is just a ID
trainData <- subset(fullData[trainIdx, ], select=-PassengerId) 
# Changing the target variable to factor
trainData$Survived <- factor(trainData$Survived)
levels(trainData$Survived) <- c("no", "yes")
print(table(trainData$Survived, useNA = "always"))
# Divide data into training and validation
index <- createDataPartition(y = trainData$Survived, p = 0.7, list = FALSE)
train <- trainData[index,]
valid <- trainData[-index,]


## ----train-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Choose the models that will be trained
methods <- list("knn","nb","glmboost")
# Set the variables for the function "createModels"
formula <- "Survived ~ ."
preProcess <- c("center", "scale")          
tuneLength <- 25                             
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 3, 
                     allowParallel = TRUE,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)
# Train the models
nameModel <- createModels(formula,train,methods,ctrl,preProcess)
cat("Best model: ",nameModel)


## ----valid-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Recover saved model
bestModel <- readRDS(paste0("./",nameModel))
# Check the best of the hyperparameters
print(bestModel$bestTune)
# Testing the model in the validation data
xValid <- subset(valid, select=-Survived)
yValid <- valid$Survived
predValues <- predict(object = bestModel, 
                       newdata = xValid, 
                       type = "raw")
head(predValues,5)
# Generates confusion matrix with positive class (yes)
confusionMatrix(predValues,yValid,positive = "yes")
# Returns the probability of the positive class (yes)
predProbs <- predict(object = bestModel, newdata = xValid, type="prob")[,2] 
head(predProbs)
predPos <- predProbs[yValid=="yes"]  #prediction for true positives
predNeg <- predProbs[yValid=="no"]   #prediction for true negatives
# Generates a plot showing the ROC curve and PR
# ROC Curve    
roc <- PRROC::roc.curve(scores.class0 = predPos, 
                        scores.class1 = predNeg, 
                        curve = T)
print(roc)
plot(roc)
# PR Curve
pr <- PRROC::pr.curve(scores.class0 = predPos, 
                      scores.class1 = predNeg, 
                      curve = T)
print(pr)
plot(pr)


## ----test------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Selects the columns of the dataframe that will be used
testData <- subset(fullData[-trainIdx, ], select=-Survived) 
print(head(testData,5))
print(str(testData))
# Testing the model in test data
predTestValues <- predict(object = bestModel, 
                          newdata = testData[,-1], #The PassengerId will not be used
                          type = "raw")
print(head(predTestValues,5))
# Converts data to a dataframe
predTest <- as.data.frame(predTestValues)
print(head(predTest,10))


## ----submit----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Creates the object that will store the data that will be submitted
sub <- data.table(PassengerId = testData$PassengerId, Survived = NA)
sub$Survived = as.numeric(ifelse(predTest == "no", 0, 1))
print(head(sub,10))
# Save the CSV file
nameFile <- paste0(nameModel,".submission.csv")
fwrite(sub, nameFile)
# Informs the end date/time of the process
dateFin <- Sys.time()
cat("\n End of execution: ", as.character(dateFin))

