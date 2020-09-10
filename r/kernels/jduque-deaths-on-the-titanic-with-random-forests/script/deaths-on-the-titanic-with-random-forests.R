## ----setup, include=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----message=FALSE, results='hide'-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
packages <- c("ggplot2", "rpart", "randomForest")
lapply(packages, library, character.only = TRUE)
rm(packages)
training.set <- read.csv('../input/train.csv')
test.set <- read.csv('../input/test.csv')


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(training.set)
summary(training.set)


## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mode.fun <- function(x) {
  a <- unique(x)
  return(a[which.max(tabulate(match(x, a)))])
}

for(i in 1:ncol(training.set)) {
  cat(sum(is.na(training.set[,i])), sum(which(training.set[,i] == "")), '\n')
}

for(i in 1:ncol(test.set)) {
  cat(sum(is.na(test.set[,i])), sum(which(test.set[,i] == "")), '\n')
}

for(i in 1:ncol(training.set)) {
  if(is.factor(training.set[,i])) {
    training.set[,i][is.na(training.set[,i])] <- mode.fun(training.set[,i])
    training.set[,i][which(training.set[,i] == "")] <- mode.fun(training.set[,i])
  }
  else {
    training.set[,i][is.na(training.set[,i])] <- mean(training.set[,i], na.rm=TRUE)
    training.set[,i][which(training.set[,i] == "")] <- mean(training.set[,i], na.rm=TRUE)
  }
}

for(i in 1:ncol(test.set)) {
  if(is.factor(test.set[,i])) {
    test.set[,i][is.na(test.set[,i])] <- mode.fun(test.set[,i])
    test.set[,i][which(test.set[,i] == "")] <- mode.fun(test.set[,i])
  }
  else {
    test.set[,i][is.na(test.set[,i])] <- mean(test.set[,i],na.rm=TRUE)
    test.set[,i][which(test.set[,i] == "")] <- mean(test.set[,i],na.rm=TRUE)
  }
}

#Droping the empty factors that are no longer being used
training.set$Embarked <- droplevels(training.set$Embarked)
test.set$Embarked <- droplevels(test.set$Embarked)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Fix some of the other data classes because the number of factors for some of them is too large
training.set[,"Survived"] <- as.factor(training.set[,"Survived"])
training.set[,"Pclass"] <- as.factor(training.set[,"Pclass"])
test.set[,"Pclass"] <- as.factor(test.set[,"Pclass"])
training.set[,"Ticket"] <- as.numeric(training.set[, "Ticket"])
test.set[,"Ticket"] <- as.numeric(test.set[, "Ticket"])
training.set[,"Cabin"] <- as.numeric(training.set[, "Cabin"])
test.set[,"Cabin"] <- as.numeric(test.set[, "Cabin"])


#Checking to see if we have any NAs one last time
for(i in 1:ncol(training.set)) {
  cat(sum(is.na(training.set[,i])), sum(which(training.set[,i] == "")), '\n')
}

for(i in 1:ncol(test.set)) {
  cat(sum(is.na(test.set[,i])), sum(which(test.set[,i] == "")), '\n')
}
summary(training.set)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
qplot(Survived, Age, data = training.set, fill =  Survived, geom = "boxplot")
qplot(Fare, data = training.set, fill = Survived, bins = 50)
qplot(Sex, data = training.set, fill = Survived, geom = "bar")
qplot(Pclass, data = training.set, fill = Survived, geom = "bar")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
New.Dat <- training.set[, -4]
r.tree.er <- 0
r.for1.er <- 0
r.for2.er <- 0
log.rg.er <- 0
mod.en.er <- 0 

for(i in seq(1, nrow(New.Dat), 99)) {
  tst.dat <- New.Dat[i:(i + 98),]
  trn.dat <- New.Dat[- (i:(i + 98)),]
  
  regtree <- rpart(Survived ~ ., data = trn.dat, method = "class") 
  ranfor1 <- randomForest(Survived ~ ., data = trn.dat, ntree = 30)
  ranfor2 <- randomForest(Survived ~ ., data = trn.dat, ntree = 50)
  logirgr <- glm(Survived ~ ., data = trn.dat, family = binomial)
  
  #Preictions of original four models
  predict.logirgr <- as.numeric(predict(logirgr, tst.dat, type = "response") >= 0.5) 
  predict.regtree <- as.numeric(colnames(predict(regtree, tst.dat))[max.col(predict(regtree, tst.dat), ties.method="first")])
  predict.ranfor1 <- as.numeric(predict(ranfor1, tst.dat)) - 1
  predict.ranfor2 <- as.numeric(predict(ranfor2, tst.dat)) - 1
  
  #Predictions of mode of models
  predict.mode.en <- numeric()
  for(i in 1:nrow(tst.dat)) {
    x <- c(predict.logirgr[i], predict.regtree[i], predict.ranfor1[i], predict.ranfor2[i])
    predict.mode.en <- c(predict.mode.en, mode.fun(x))
  }
  
  
  #Counting how many times the predictions are correct
  r.tree.er <- r.tree.er + sum(tst.dat[,"Survived"] == predict.regtree)
  r.for1.er <- r.tree.er + sum(tst.dat[,"Survived"] == predict.ranfor1)
  r.for2.er <- r.tree.er + sum(tst.dat[,"Survived"] == predict.ranfor2)
  log.rg.er <- r.tree.er + sum(tst.dat[,"Survived"] == predict.logirgr)
  mod.en.er <- mod.en.er + sum(tst.dat[,"Survived"] == predict.mode.en)
}

#Accuracy of the models
r.tree.er <- r.tree.er / nrow(New.Dat)
r.for1.er <- r.for1.er / nrow(New.Dat)
r.for2.er <- r.for2.er / nrow(New.Dat)
log.rg.er <- log.rg.er / nrow(New.Dat)
mod.en.er <- mod.en.er / nrow(New.Dat)



## ----warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
st.log.er <- 0
st.for.er <- 0

for(i in seq(1, nrow(New.Dat), 99)) {
  tst.dat <- New.Dat[i:(i + 98),]
  trn.dat <- New.Dat[- (i:(i + 98)),]
  
  #Splitting training data to train layer one and layer two
  trn.inds <- sample(1:792, 693, replace = FALSE)
  trn1.dat <- trn.dat[trn.inds,]
  trn2.dat <- trn.dat[-trn.inds,]
  
  
  #First layer
  regtree.1 <- rpart(Survived ~ ., data = trn1.dat, method = "class") 
  ranfor1.1 <- randomForest(Survived ~ ., data = trn1.dat, ntree = 30)
  ranfor2.1 <- randomForest(Survived ~ ., data = trn1.dat, ntree = 50)
  logirgr.1 <- glm(Survived ~ ., data = trn1.dat, family = binomial)
  
  predict.logirgr.1 <- as.numeric(predict.glm(logirgr.1, trn2.dat, type = "response") >= 0.5) #issue with predict
  predict.regtree.1 <- as.numeric(colnames(predict(regtree.1, trn2.dat))[max.col(predict(regtree.1, trn2.dat), ties.method="first")])
  predict.ranfor1.1 <- as.numeric(predict(ranfor1.1, trn2.dat)) - 1
  predict.ranfor2.1 <- as.numeric(predict(ranfor2.1, trn2.dat)) - 1
  
  
  #predictions of logistic regression of four models (second layer)
  staklog <- glm(Survived ~ predict.logirgr.1 + predict.regtree.1 + predict.ranfor1.1 + predict.ranfor2.1, data = trn2.dat, family = binomial)
  predict.stk.log <- as.numeric(predict(staklog, tst.dat, type = "response") >= 0.5) 
  
  
  #predictions of random forest of four models (second layer)
  stakrtr <- randomForest(Survived ~ predict.logirgr.1 + predict.regtree.1 + predict.ranfor1.1 + predict.ranfor2.1, data = trn2.dat, ntree = 20)
  predict.stk.for <-  as.numeric(predict(stakrtr, tst.dat)) - 1
  

  st.log.er <- st.log.er + sum(tst.dat[,"Survived"] == predict.stk.log)
  st.for.er <- st.for.er + sum(tst.dat[,"Survived"] == predict.stk.for)
}

st.log.er <- st.log.er / nrow(New.Dat)
st.for.er <- st.for.er / nrow(New.Dat)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Before I start the search I clear up some memory because my environment thingy is looking gross.
rm(list = setdiff(ls(), c("training.set", "New.Dat", "test.set"))) 

#I probably should've made something like this earlier, but meh. This will serve like our as our cost function 
cost <- function(ntrees) {
  acc <- 0
  for(i in seq(1, nrow(New.Dat), 99)) {
    tst.dat <- New.Dat[i:(i + 98),]
    trn.dat <- New.Dat[- (i:(i + 98)),]
    
    model <- randomForest(Survived ~ ., data = trn.dat, ntree = ntrees)
    predictions <- as.numeric(predict(model, tst.dat)) - 1
    acc <- acc + sum(tst.dat[,"Survived"] == predictions)
  }
  error <- 1 - (acc / nrow(New.Dat))
  return(error)
}

numtrees <- sample(1:750, 1)
temp <- 1
t.mn <- 0.001
alph <- 0.8
while(temp > t.mn) {
  old.cost <- cost(numtrees)
  new.numb <- sample(1:750, 1)
  new.cost <- cost(new.numb)
  if(new.cost < old.cost) {
    numtrees <- new.numb
  }
  else { 
    probabilty <- exp((old.cost - new.cost) / temp)
    if(probabilty > runif(1)) {
      numtrees <- new.numb
    }
  }
  temp <- temp * alph
  cat('temp: ', temp, 'number of trees: ', numtrees, 'cost:', cost(numtrees), '\n')
}



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
numtrees <- 77
cost(numtrees) #Let's just check again to make sure it's ok

#Removing "Name" from the test set
test.set <- test.set[,-3]
model <- randomForest(Survived ~ ., data = New.Dat, ntree = numtrees)
predics <- predict(model, test.set) 
x <- cbind(test.set$PassengerId, as.numeric(predics) - 1)
write.csv(x, file = "Submission.csv", row.names = FALSE)

