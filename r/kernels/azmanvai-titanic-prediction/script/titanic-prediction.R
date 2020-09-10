## ---- include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)

library(caret)
library(ggplot2)
library(lubridate)


## ---- echo = TRUE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
raw <- read.csv("train.csv", sep = "," , strip.white = TRUE, stringsAsFactors = FALSE, header = TRUE, na.strings = "")
submit <- read.csv("test.csv", sep = "," , strip.white = TRUE, stringsAsFactors = FALSE, header = TRUE, na.strings = "")


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(raw); dim(submit)
names(raw); names(submit)


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(raw$PassengerId)
sum(duplicated(raw$PassengerId))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(raw$Survived)
sum(is.na(raw$Survived))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(raw$Pclass)
sum(is.na(raw$Pclass))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(raw$Sex)
sum(is.na(raw$Sex))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(raw$Age)
sum(is.na(raw$Age))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(raw$SibSp)
sum(is.na(raw$SibSp))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(raw$Parch)
sum(is.na(raw$Parch))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(raw$Ticket)
sum(duplicated(raw$Ticket))
sum(is.na(raw$Ticket))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(raw$Fare)
sum(is.na(raw$Fare))
g <- ggplot(raw, aes(x = factor(Pclass), y = Fare)) + geom_boxplot() + ylim(c(0, 200))
g


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(raw$Cabin)
sum(is.na(raw$Cabin))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(raw$Embarked)
sum(is.na(raw$Embarked))


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data <- raw
data$Sex <- as.factor(data$Sex)
data$Embarked <- as.factor(data$Embarked)
data$Survived <- as.factor(data$Survived)
str(data)


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nas <- apply(data, 2, function(x) sum(is.na(x)))
nas


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
na <- data[is.na(data$Embarked),]
na


## ---- echo =TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ticket <- grep("113", data$Ticket)  # find passengers with ticket number starting from 113xxx
ticket
embark <- data[ticket,]
rownames(embark) <- c(1: nrow(embark)) # reset row names
table(embark$Embarked)


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(36)
library(party)

embark$Ticket <- as.numeric(embark$Ticket) #set ticket as numeric for this prediction only

train.embark <- embark[-c(6, 48), -c(1, 4, 6,11)]  #exclude passengers with missing port from training set
test.embark <- embark[c(6, 48),-c(1, 4, 6)]        #passengers with missing port
fit.ctree <- ctree(Embarked~., data = train.embark)
pred.embark <- predict(fit.ctree, test.embark)
pred.embark


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data$Embarked[62] <- "S"
data$Embarked[830] <- "S"
str(data)


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(data$Age))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(456)
library(party)
ages <- data

train.age <- ages[complete.cases(ages$Age),-c(1,4,9,11)]
test.age <- ages[is.na(ages$Age), -c(1,4,6, 9,11)]

fit.ctree <- ctree(Age~., data = train.age)
pred.age <- predict(fit.ctree, test.age)
summary(pred.age)


## ----echo=, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test.age$Age <- round(pred.age,1)
y <- test.age[test.age$Age < 20,]
y


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g <- ggplot(test.age, aes(x = factor(Pclass), y = Fare, color = Age)) + geom_point() + ggtitle("Imputed Age")
g


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data$Age[is.na(data$Age)] <- round(pred.age,1)
sum(is.na(data$Age))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nas <- apply(data, 2, function(x) sum(is.na(x)))
nas/nrow(data)
data <- data[,-c(1,11)]  #exclude PasengerId and Cabin


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nzv <- nearZeroVar(data, saveMetrics = TRUE, names = TRUE)
nzv


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(data$Name)
data <- data[,-3]


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(data$Ticket)
data <- data[,-7]


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
names(data)
table(data$Survived)


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
index <- createDataPartition(data$Survived, p = 0.7, list = FALSE)
train <- data[index,]
test <- data[-index,]
x <- train[,-1]
y <- train[,1]


## ----glm, echo=TRUE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

time.glm <- system.time(glm(Survived~., data = train, family = "binomial"))
fit.glm<- glm(Survived~., data = train, family = "binomial") 
pred.glm <- predict(fit.glm, test, type = "response")
pred.glm <- ifelse(pred.glm < 0.37, 0, 1)
acc.glm <- confusionMatrix(test$Survived, pred.glm)
acc.glm
glm <- c(family = "GLM", round(c(acc.glm$byClass, time.glm[3]),3))
glm


## ----glmnet, echo=TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(glmnet)
library(Matrix)

xm <- data.matrix(x)
testxm <- data.matrix(test[,-1])
ym <- as.numeric(y) - 1
testym <- as.numeric(test$Survived) - 1

time.glmnet <- system.time(cv.glmnet(xm, ym, nfolds = 10))
fit.glmnet <- cv.glmnet(xm, ym, nfolds = 10)
pred.glmnet <- predict(fit.glmnet, testxm, s = fit.glmnet$lambda.min, type = "response")
pred.glmnet <- ifelse(pred.glmnet < 0.36, 0, 1)
acc.glmnet <- confusionMatrix(testym, pred.glmnet)
acc.glmnet
glmnet <- c(family = "GLM", round(c(acc.glmnet$byClass, time.glmnet[3]), 3))
glmnet


## ----naivebayes, echo=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(naivebayes)

time.naivebayes <- system.time(naive_bayes(x, y ))
fit.naivebayes <- naive_bayes(x, y)
pred.naivebayes <- predict(fit.naivebayes, test)
acc.naivebayes <- confusionMatrix(test$Survived, pred.naivebayes)

naivebayes <- c(family = "Naive Bayes", round(c(acc.naivebayes$byClass, time.naivebayes[3]),3))
acc.naivebayes; naivebayes


## ----ctree, echo=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(party)

# some tuning
control <- ctree_control(testtype = "Teststatistic", mincriterion = 0.95, mtry = 0)

# run model
time.ctree <- system.time(ctree(Survived~., data = train, controls = control ))
fit.ctree <- ctree(Survived~., data = train, controls = control)
pred.ctree <- predict(fit.ctree, test, type = "response")

acc.ctree <- confusionMatrix(test$Survived, pred.ctree)

ctree <- c(family = "Decision trees", round(c(acc.ctree$byClass, time.ctree[3]),3))
ctree


## ----rpart, echo=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(rpart)

# run model
time.rpart <- system.time(rpart(Survived ~ ., data = train, method = "class"))
fit.rpart <- rpart(Survived ~ ., data = train, method = "class")
cpmin <- printcp(fit.rpart)
control <- rpart.control(cp = 0.015, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10, surrogatestyle = 0, maxdepth = 30)
fit.rpart <- rpart(Survived ~ ., data = train, method = "class", control = control)
pred.rpart <- predict(fit.rpart, test, type = "class")
acc.rpart <- confusionMatrix(test$Survived, pred.rpart)

rpart <- c(family = "Decision trees", round(c(acc.rpart$byClass, time.rpart[3]),3))
rpart



## ----ranger----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ranger)


time.ranger <- system.time(csrf(Survived ~ ., training_data = train, test_data = test, params1 = list(num.trees = 300, mtry = 4), params2 = list(num.trees = 5)))
#fit.ranger <- ranger(Survived~., data = train)
#pred.ranger <- predict(fit.ranger, test)

tune.ranger <- csrf(Survived ~ ., training_data = train, test_data = test, params1 = list(num.trees = 300, mtry = 4), params2 = list(num.trees = 10))
acc.ranger <- confusionMatrix(tune.ranger, test$Survived)
ranger <- c(family = "Decision trees", round(c(acc.ranger$byClass, time.ranger[3]),3))
ranger



## ----randomForest, echo=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(randomForest)

# some tuning
cv.randomforest <- rfcv(x, y, cv.fold = 10, scale = "log", step = 0.5, recursive = TRUE)
tune.randomforest <- tuneRF(x, y, ntreeTry = 150, stepFactor = 2, doBest = TRUE)

# run model
time.randomforest <- system.time(tuneRF(x, y, ntreeTry = 150, stepFactor = 2, doBest = TRUE))

pred.randomforest <- predict(tune.randomforest, test) 
acc.randomforest <- confusionMatrix(pred.randomforest, test$Survived)

randomforest <- c(family = "Decision trees", round(c(acc.randomforest$byClass, time.randomforest[3]),3))
randomforest



## ----comp, echo= FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
comp <- data.frame(rbind(glm, glmnet,  rpart, ctree, ranger, randomforest), stringsAsFactors = FALSE)
comp[,13] <- as.numeric(comp[,13])
c <- comp[order(comp$Balanced.Accuracy, decreasing = TRUE), c(1,13,12, 2:5)]
c


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(submit)
str(submit)


## ---- echo= TRUE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
na <- sum(is.na(submit))
na
nap <- apply(submit, 2, function(x) sum(is.na(x)))
nap


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(party)
ages2 <- submit
ages2$Sex <- as.factor(ages2$Sex)
ages2$Embarked <- as.factor(ages2$Embarked)

train.age2 <- ages2[complete.cases(ages2$Age),-c(1,3, 8, 10)]  # complete Age
test.age2 <- ages2[is.na(ages2$Age), -c(1,3, 8, 10)]           # missing Age

fit.age2 <- ctree(Age~., data = train.age2)
pred.age2 <- predict(fit.age2, test.age2)
summary(pred.age2)


## ----echo=, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test.age2$Age <- round(pred.age2,1)
y2 <- test.age2[test.age2$Age < 18,]
y2


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
g <- ggplot(test.age2, aes(x = factor(Pclass), y = Fare, color = Age)) + geom_point() + ggtitle("Imputed Age")
g


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
submit$Age[is.na(submit$Age)] <- round(pred.age2,1)
sum(is.na(submit$Age))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fare <- submit[is.na(submit$Fare),]
fare



## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fare2 <- submit[complete.cases(submit$Fare),]
fare2 <- fare2[fare2$Pclass == 3,]
fare2 <- fare2[fare2$Sex == "male",]
fare2 <- fare2[fare2$Age >= 40,]
fare2 <- fare2[fare2$Embarked == "S",]
meanfare <- mean(fare2$Fare)
meanfare


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
submit$Fare[submit$PassengerId == 1044] <- meanfare
apply(submit, 2, function(x) sum(is.na(x)))


## ---- echo=TRUE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
setdiff(names(submit), names(data))


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
names(submit)
submit <- submit[,-c(1,3,8,10)]
str(data)


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
submit$Sex <- as.factor(submit$Sex)
submit$Embarked <- as.factor(submit$Embarked)
str(submit)


## ---- echo = TRUE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pred.randomforest <- predict(tune.randomforest, submit) 
pred.randomforest
submit2 <- read.csv("test.csv", sep = "," , strip.white = TRUE, stringsAsFactors = FALSE, header = TRUE, na.strings = "")
submission <- data.frame(PassengerId = submit2$PassengerId, Survived = pred.randomforest)
submission

