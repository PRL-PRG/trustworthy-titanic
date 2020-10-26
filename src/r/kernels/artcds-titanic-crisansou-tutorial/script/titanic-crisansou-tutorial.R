knitr::opts_chunk$set(echo = TRUE)
dateIni <- Sys.time()
cat("\n Start of execution: ", as.character(dateIni))
libs = c("data.table", "ggplot2", "randomForest", "ROSE", "DMwR", "corrplot", "caret", "xgboost", "e1071", "PRROC", "klaR", "dplyr", "tidyr")
for (i in libs) {
    if (i %in% row.names(installed.packages()) == FALSE) {
        cat("Load/install the library: ", i, "\n\n")
        install.packages(i, repos = "http://cran.us.r-project.org")
        library(i, character.only = TRUE)
    }
    else {
        cat("Load the library: ", i, "\n\n")
        library(i, character.only = TRUE)
    }
}
trainPath <- "../input/train.csv"
testPath <- "../input/test.csv"
trainOri <- read.csv(trainPath, na.strings = c("NA", "NaN", ""))
testOri <- read.csv(testPath, na.strings = c("NA", "NaN", ""))
createModels <- function(formula, data, methods, ctrl, preProcess = NULL, tuneLength = 1) {
    options(warn = -1)
    metric <- "ROC"
    seed <- 54321
    formula <- as.formula(formula)
    modelsOri <- list()
    modelsUp <- list()
    modelsDown <- list()
    modelsRose <- list()
    modelsSmote <- list()
    score <- data.frame(method = character(), score = numeric(), model = character(), stringsAsFactors = FALSE)
    for (i in methods) {
        tryCatch({
            cat("METHOD: ", i, "\n\n")
            ctrl$sampling <- NULL
            nameOri <- paste0(i, ".ori")
            set.seed(seed)
            modelsOri[[nameOri]] <- train(form = formula, data = data, method = i, metric = metric, preProcess = preProcess, tuneLength = tuneLength, trControl = ctrl)
            cat("Importance of variables \n\n")
            tryCatch({
                importanceOri <- varImp(modelsOri[[nameOri]], scale = FALSE)
                print(importanceOri)
                print(plot(importanceOri))
            }, error = function(e) {
                cat("It wasn't possible to verify the importance of the variables: ", i, " - ERROR :", conditionMessage(e), "\n")
            })
            cat("\n\n PERFORMANCE - ORIGINAL MODEL: \n\n")
            print(getTrainPerf(modelsOri[[nameOri]]))
            nameFile <- paste0(nameOri, ".model.rds")
            saveRDS(modelsOri[[nameOri]], paste0("./", nameFile))
            score[nrow(score) + 1, ] = list(nameOri, getTrainPerf(modelsOri[[nameOri]])[, "TrainROC"], nameFile)
            ctrl$sampling <- "up"
            nameUp <- paste0(i, ".up")
            set.seed(seed)
            modelsUp[[nameUp]] <- train(form = formula, data = data, method = i, metric = metric, preProcess = preProcess, tuneLength = tuneLength, trControl = ctrl)
            cat("\n\n PERFORMANCE - UP MODEL: \n\n")
            print(getTrainPerf(modelsUp[[nameUp]]))
            nameFile <- paste0(nameUp, ".model.rds")
            saveRDS(modelsUp[[nameUp]], paste0("./", nameFile))
            score[nrow(score) + 1, ] = list(nameUp, getTrainPerf(modelsUp[[nameUp]])[, "TrainROC"], nameFile)
            ctrl$sampling <- "down"
            nameDown <- paste0(i, ".down")
            set.seed(seed)
            modelsDown[[nameDown]] <- train(form = formula, data = data, method = i, metric = metric, preProcess = preProcess, tuneLength = tuneLength, trControl = ctrl)
            cat("\n\n PERFORMANCE - DOWN MODEL: \n\n")
            print(getTrainPerf(modelsDown[[nameDown]]))
            nameFile <- paste0(nameDown, ".model.rds")
            saveRDS(modelsDown[[nameDown]], paste0("./", nameFile))
            score[nrow(score) + 1, ] = list(nameDown, getTrainPerf(modelsDown[[nameDown]])[, "TrainROC"], nameFile)
            ctrl$sampling <- "rose"
            nameRose <- paste0(i, ".rose")
            set.seed(seed)
            modelsRose[[nameRose]] <- train(form = formula, data = data, method = i, metric = metric, preProcess = preProcess, tuneLength = tuneLength, trControl = ctrl)
            cat("\n\n PERFORMANCE - ROSE MODEL: \n\n")
            print(getTrainPerf(modelsRose[[nameRose]]))
            nameFile <- paste0(nameRose, ".model.rds")
            saveRDS(modelsRose[[nameRose]], paste0("./", nameFile))
            score[nrow(score) + 1, ] = list(nameRose, getTrainPerf(modelsRose[[nameRose]])[, "TrainROC"], nameFile)
            ctrl$sampling <- "smote"
            nameSmote <- paste0(i, ".smote")
            set.seed(seed)
            modelsSmote[[nameSmote]] <- train(form = formula, data = data, method = i, metric = metric, preProcess = preProcess, tuneLength = tuneLength, trControl = ctrl)
            cat("\n\n PERFORMANCE - SMOTE MODEL: \n\n")
            print(getTrainPerf(modelsSmote[[nameSmote]]))
            nameFile <- paste0(nameSmote, ".model.rds")
            saveRDS(modelsSmote[[nameSmote]], paste0("./", nameFile))
            score[nrow(score) + 1, ] = list(nameSmote, getTrainPerf(modelsSmote[[nameSmote]])[, "TrainROC"], nameFile)
            models <- list(original = modelsOri[[nameOri]], down = modelsDown[[nameDown]], up = modelsUp[[nameUp]], smote = modelsSmote[[nameSmote]], rose = modelsRose[[nameRose]])
            models[sapply(models, is.null)] <- NULL
            cat("EVALUATE THE MODELS USING THE ROC METRIC \n\n")
            resampling <- resamples(models)
            print(summary(resampling, metric = metric))
            cat("DOTPLOT \n\n")
            scales <- list(x = list(relation = "free"), y = list(relation = "free"))
            print(dotplot(resampling, scales = scales, main = paste("Evaluating all models of the method", i)))
        }, error = function(e) {
            cat("It wasn't possible to train the model ", i, " - ERROR :", conditionMessage(e), "\n")
        })
    }
    modelsList <- c(modelsOri, modelsDown, modelsUp, modelsSmote, modelsRose)
    if (length(modelsOri) > 1) {
        cat("\n\n EVALUATING THE RESULT OF ALL METHODS AND MODELS \n\n")
        resampling <- resamples(modelsList)
        print(summary(resampling, metric = metric))
        scales <- list(x = list(relation = "free"), y = list(relation = "free"))
        print(dotplot(resampling, scales = scales, main = "Evaluating all methods used"))
    }
    cat("\n\n MODEL WITH THE BEST PERFORMANCE: \n\n")
    best <- score %>% top_n(1, score) %>% head(1)
    print(best)
    rdsFiles = list.files(pattern = ".rds")
    rdsFiles <- rdsFiles[!rdsFiles %in% best$model]
    file.remove(rdsFiles)
    return(best$model)
}
glimpse(trainOri)
sapply(trainOri, class)
head(trainOri, 5)
glimpse(testOri)
sapply(testOri, class)
head(testOri, 5)
summary(trainOri)
summary(testOri)
sapply(trainOri, function(x) round(sum(is.na(x))/nrow(trainOri) * 100, 1))
sapply(testOri, function(x) round(sum(is.na(x))/nrow(testOri) * 100, 1))
cbind(freq = table(trainOri$Survived), percent = round(prop.table(table(trainOri$Survived)) * 100, 1))
apply(trainOri, 2, function(x) length(unique(x)))
apply(trainOri, 2, function(x) length(unique(x)))
testOri$Survived <- NA
fullData <- rbind(trainOri, testOri)
trainIdx <- seq(nrow(trainOri))
ggplot(fullData[trainIdx, ], aes(Sex, fill = factor(Survived))) + geom_bar(stat = "count", position = "dodge") + xlab("Sex") + ylab("Count") + scale_fill_discrete(name = "Survived") + ggtitle("Sex X Survived")
ggplot(fullData[trainIdx, ], aes(Pclass, fill = factor(Survived))) + geom_bar(stat = "count") + xlab("Pclass") + ylab("Count") + scale_fill_discrete(name = "Survived") + ggtitle("Pclass X Survived")
numberCols <- dplyr::select_if(trainOri, is.numeric)
par(mfrow = c(2, 2))
for (i in 1:7) {
    hist(numberCols[, i], main = names(numberCols)[i], xlab = "")
}
par(mfrow = c(2, 2))
for (i in 1:7) {
    boxplot(numberCols[, i], main = names(numberCols)[i])
}
par(mfrow = c(2, 2))
for (i in 1:7) {
    barplot(table(numberCols$Survived, numberCols[, i]), main = names(numberCols)[i], legend.text = unique(numberCols$Survived))
}
fullTemp <- fullData %>% group_by(Pclass) %>% mutate(Age = ifelse(is.na(Age), round(mean(Age, na.rm = TRUE)), Age)) %>% mutate(Fare = ifelse(is.na(Fare), round(mean(Fare, na.rm = TRUE)), Fare))
fullData$Age <- fullTemp$Age
fullData$Fare <- fullTemp$Fare
maxEmbarked <- names(sort(table(fullData$Embarked), decreasing = T)[1])
fullData$Embarked[is.na(fullData$Embarked)] <- maxEmbarked
dummies <- predict(dummyVars(~Sex, data = fullData), newdata = fullData)
fullData <- cbind(fullData, dummies)
dummies <- predict(dummyVars(~Embarked, data = fullData), newdata = fullData)
fullData <- cbind(fullData, dummies)
fullData$Pclass <- factor(fullData$Pclass)
dummies <- predict(dummyVars(~Pclass, data = fullData), newdata = fullData)
fullData <- cbind(fullData, dummies)
fullData$Title <- gsub("(.*, )|(\\..*)", "", fullData$Name)
officer <- c("Capt", "Col", "Don", "Dr", "Major", "Rev")
royalty <- c("Dona", "Lady", "the Countess", "Sir", "Jonkheer")
fullData$Title[fullData$Title == "Mlle"] <- "Miss"
fullData$Title[fullData$Title == "Ms"] <- "Miss"
fullData$Title[fullData$Title == "Mme"] <- "Mrs"
fullData$Title[fullData$Title %in% royalty] <- "Royalty"
fullData$Title[fullData$Title %in% officer] <- "Officer"
fullData$Title <- factor(fullData$Title)
dummies <- predict(dummyVars(~Title, data = fullData), newdata = fullData)
fullData <- cbind(fullData, dummies)
fullData$FamilySize <- fullData$SibSp + fullData$Parch + 1
fullData$FamilyType[fullData$FamilySize == 1] <- "A"
fullData$FamilyType[fullData$FamilySize > 1 & fullData$FamilySize < 5] <- "S"
fullData$FamilyType[fullData$FamilySize >= 5] <- "B"
fullData$FamilyType <- factor(fullData$FamilyType)
dummies <- predict(dummyVars(~FamilyType, data = fullData), newdata = fullData)
fullData <- cbind(fullData, dummies)
ggplot(fullData[trainIdx, ], aes(Title, fill = factor(Survived))) + geom_bar(stat = "count") + xlab("Title") + ylab("Count") + scale_fill_discrete(name = " Survived") + ggtitle("Title X Survived")
ggplot(fullData[trainIdx, ], aes(FamilyType, fill = factor(Survived))) + geom_bar(stat = "count") + xlab("FamilyType") + ylab("Count") + scale_fill_discrete(name = " Survived") + ggtitle("FamilyType X Survived")
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
cor(fullData[trainIdx, ]) %>% corrplot(addCoef.col = "grey", number.cex = 1.4)
trainData <- subset(fullData[trainIdx, ], select = -PassengerId)
trainData$Survived <- factor(trainData$Survived)
levels(trainData$Survived) <- c("no", "yes")
print(table(trainData$Survived, useNA = "always"))
index <- createDataPartition(y = trainData$Survived, p = 0.7, list = FALSE)
train <- trainData[index, ]
valid <- trainData[-index, ]
methods <- list("knn", "nb", "glmboost")
formula <- "Survived ~ ."
preProcess <- c("center", "scale")
tuneLength <- 25
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, allowParallel = TRUE, summaryFunction = twoClassSummary, classProbs = TRUE)
nameModel <- createModels(formula, train, methods, ctrl, preProcess)
cat("Best model: ", nameModel)
bestModel <- readRDS(paste0("./", nameModel))
print(bestModel$bestTune)
xValid <- subset(valid, select = -Survived)
yValid <- valid$Survived
predValues <- predict(object = bestModel, newdata = xValid, type = "raw")
head(predValues, 5)
confusionMatrix(predValues, yValid, positive = "yes")
predProbs <- predict(object = bestModel, newdata = xValid, type = "prob")[, 2]
head(predProbs)
predPos <- predProbs[yValid == "yes"]
predNeg <- predProbs[yValid == "no"]
roc <- PRROC::roc.curve(scores.class0 = predPos, scores.class1 = predNeg, curve = T)
print(roc)
plot(roc)
pr <- PRROC::pr.curve(scores.class0 = predPos, scores.class1 = predNeg, curve = T)
print(pr)
plot(pr)
testData <- subset(fullData[-trainIdx, ], select = -Survived)
print(head(testData, 5))
print(str(testData))
predTestValues <- predict(object = bestModel, newdata = testData[, -1], type = "raw")
print(head(predTestValues, 5))
predTest <- as.data.frame(predTestValues)
print(head(predTest, 10))
sub <- data.table(PassengerId = testData$PassengerId, Survived = NA)
sub$Survived = as.numeric(ifelse(predTest == "no", 0, 1))
print(head(sub, 10))
nameFile <- paste0(nameModel, ".submission.csv")
fwrite(sub, nameFile)
dateFin <- Sys.time()
cat("\n End of execution: ", as.character(dateFin))
