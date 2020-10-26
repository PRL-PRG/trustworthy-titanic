knitr::opts_chunk$set(echo = TRUE)
library(caret)
readData <- function(path.name, file.name, column.types, missing.types) {
    read.csv(paste(path.name, file.name, sep = ""), colClasses = column.types, na.strings = missing.types)
}
Titanic.path <- "../input/"
train.file.name <- "train.csv"
test.file.name <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c("integer", "factor", "factor", "character", "factor", "numeric", "integer", "integer", "character", "numeric", "character", "factor")
test.column.types <- train.column.types[-2]
train <- readData(Titanic.path, train.file.name, train.column.types, missing.types)
test <- readData(Titanic.path, test.file.name, test.column.types, missing.types)
test$Survived <- NA
combi <- rbind(train, test)
str(train)
summary(train)
library(ggplot2)
p <- ggplot(train, aes(x = Survived, fill = Sex)) + geom_bar(color = "black")
p
p2 <- ggplot(train[-which(is.na(train$Age)), ], aes(x = Age, fill = Survived)) + geom_density(alpha = 0.6) + facet_grid(. ~ Sex)
p2
p3 <- ggplot(train, aes(x = Fare, fill = Survived)) + geom_histogram() + facet_grid(. ~ Pclass)
p3
prop.table(table(train$Survived, train$Pclass), margin = 2)
p4 <- ggplot(train[!is.na(train$Embarked), ], aes(x = Survived, fill = Embarked)) + geom_bar(color = "black")
p4
title.extract <- function(x) {
    strsplit(x, split = "[,.]")[[1]][2]
}
combi$Title <- sapply(combi$Name, FUN = title.extract)
combi$Title <- sub(" ", "", combi$Title)
combi$Title[combi$PassengerId == 797] <- "Mrs"
combi$Title[combi$Title %in% c("Mlle", "Mme")] <- "Mlle"
combi$Title[combi$Title %in% c("Capt", "Don", "Major", "Sir", "Jonkheer")] <- "Sir"
combi$Title[combi$Title %in% c("Dona", "Lady", "the Countess")] <- "lady"
combi$Title <- as.factor(combi$Title)
combi$Surname <- sapply(combi$Name, FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][1]
})
combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")
combi$FamilyID[combi$FamilySize <= 2] <- "Small"
famIDs <- data.frame(table(combi$FamilyID))
famIDs <- famIDs[famIDs$Freq <= 2, ]
combi$FamilyID[combi$FamilyID %in% famIDs$Var1] <- "Small"
combi$FamilyID <- as.factor(combi$FamilyID)
extractCabin <- function(combi) {
    combi$Cabin <- sapply(combi$Cabin, FUN = function(x) {
        strsplit(x, split = "")[[1]][1]
    })
    combi$Cabin[is.na(combi$Cabin)] <- "N"
    combi$Cabin <- as.factor(combi$Cabin)
    combi.ticket <- table(factor(combi$Ticket))
    combi.ticket.moreThanOne <- combi.ticket[combi.ticket > 1]
    combi.temp <- combi[combi$Ticket %in% names(combi.ticket.moreThanOne), ]
    for (name in names(combi.ticket.moreThanOne)) {
        row.sameTicket <- combi[combi$Ticket == name, ]
        Cabin_boolean <- row.sameTicket$Cabin %in% c("A", "B", "C", "D", "E", "F", "G")
        if (sum(Cabin_boolean) > 0) {
            correctCabin <- names(sort(table(row.sameTicket$Cabin[Cabin_boolean]), decreasing = TRUE))[1]
            row.sameTicket$Cabin[row.sameTicket$Cabin == "N"] <- correctCabin
            combi$Cabin[row.sameTicket$PassengerId] <- row.sameTicket$Cabin
        }
    }
    combi$Cabin <- as.factor(combi$Cabin)
    return(combi)
}
combi <- extractCabin(combi)
extractTicket <- function(ticket) {
    pattern <- c("\\/", "\\.", "\\s", "[[:digit:]]")
    for (p in pattern) {
        ticket <- gsub(p, "", ticket)
    }
    ticket <- substr(toupper(ticket), 1, 1)
    ticket[ticket == ""] <- "N"
    ticket <- as.factor(ticket)
}
combi$Ticket <- extractTicket(combi$Ticket)
combi$Fare[is.na(combi$Fare)] <- median(combi$Fare, na.rm = TRUE)
combi$Embarked[is.na(combi$Embarked)] <- "S"
library(rpart)
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = combi[!is.na(combi$Age), ], method = "anova")
combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age), ])
train <- combi[1:nrow(train), ]
test <- combi[nrow(train) + 1:nrow(test), ]
extractFeatures <- function(data) {
    features <- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Survived", "Title", "FamilySize", "FamilyID")
    fea <- data[, features]
    return(fea)
}
library(glmnet)
x <- model.matrix(Survived ~ ., data = extractFeatures(train))
y <- extractFeatures(train)$Survived
newx <- model.matrix(~., data = extractFeatures(test)[, -which(names(extractFeatures(test)) %in% "Survived")])
set.seed(1)
fit_ridge <- cv.glmnet(x, y, alpha = 0, family = "binomial", type.measure = "deviance")
pred_ridge <- predict(fit_ridge, newx = newx, s = "lambda.min", type = "class")
submission <- data.frame(PassengerId = test$PassengerId, Survived = pred_ridge)
write.csv(submission, file = "ridge.csv", row.names = FALSE)
library(caret)
fitControl <- trainControl(method = "repeatedcv", number = 3, repeats = 3)
newGrid <- expand.grid(n.trees = c(50, 100, 200, 300), interaction.depth = c(6), shrinkage = 0.01, n.minobsinnode = 10)
fit_gbm <- train(Survived ~ ., data = extractFeatures(train), method = "gbm", trControl = fitControl, tuneGrid = newGrid, bag.fraction = 0.5, verbose = FALSE)
fit_gbm$bestTune
set.seed(1234)
newGrid <- expand.grid(n.trees = c(200), interaction.depth = c(4:12), shrinkage = 0.01, n.minobsinnode = 10)
fit_gbm <- train(Survived ~ ., data = extractFeatures(train), method = "gbm", trControl = fitControl, tuneGrid = newGrid, bag.fraction = 0.5, verbose = FALSE)
fit_gbm$bestTune
set.seed(1234)
newGrid <- expand.grid(n.trees = c(2000), interaction.depth = c(10), shrinkage = 0.001, n.minobsinnode = 10)
fit_gbm_LowerRate <- train(Survived ~ ., data = extractFeatures(train), method = "gbm", trControl = fitControl, tuneGrid = newGrid, bag.fraction = 0.5, verbose = FALSE)
fit_gbm_LowerRate$results
pred_gbm <- predict(fit_gbm_LowerRate, extractFeatures(test))
submission <- data.frame(PassengerId = test$PassengerId, Survived = pred_gbm)
write.csv(submission, file = "gbm_ntree-2000_rate-0.001_inter-10.csv", row.names = FALSE)
library(party)
set.seed(1)
fit_crf <- cforest(Survived ~ ., data = extractFeatures(train), controls = cforest_unbiased(ntree = 2000, mtry = 3))
pred_crf <- predict(fit_crf, extractFeatures(test), OOB = TRUE, type = "response")
submission <- data.frame(PassengerId = test$PassengerId, Survived = pred_crf)
write.csv(submission, file = "crf_seed1.csv", row.names = FALSE)
cat("Difference ratio between ridge and conditional random forest:", sum(pred_ridge != pred_crf)/nrow(test))
cat("Difference ratio between ridge and conditional gbm:", sum(pred_ridge != pred_gbm)/nrow(test))
cat("Difference ratio between conditional random forest and gbm:", sum(pred_crf != pred_gbm)/nrow(test))
ensemble <- as.numeric(pred_ridge) + as.numeric(pred_gbm) - 1 + as.numeric(pred_crf) - 1
ensemble <- sapply(ensemble/3, round)
submission <- data.frame(PassengerId = test$PassengerId, Survived = ensemble)
write.csv(submission, file = "ensemble_vote.csv", row.names = FALSE)
