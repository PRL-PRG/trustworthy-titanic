
# This R script will run on our backend. You can write arbitrary code here!

library(caret)
library(rpart.plot)
impute_median <- function(x){
  ind_na <- is.na(x)
  x[ind_na] <- median(x[!ind_na])
  as.numeric(x)
}
train <- read.csv('../input/train.csv', stringsAsFactors = FALSE, na.strings = '' )
test <- read.csv('../input/test.csv', stringsAsFactors = FALSE, na.strings = '' )
train2 <- subset(train, select = c(2,3,5,6,7,8))
test2 <- subset(test, select = c(2,4,5,6,7,8))

firstClass <- c()
secondClass <- c()
thirdClass <- c()

for(k in 1:length(train2$Pclass)){
  
  if(train2$Pclass[k] == 1){
    firstClass <- c(firstClass, 1)
  }
  else{
    firstClass <-c(firstClass, 0)
  }
  if(train2$Pclass[k] == 2){
    secondClass <- c(secondClass, 1)
  }
  else{
    secondClass <-c(secondClass, 0)
  }
  if(train2$Pclass[k] == 3){
    thirdClass <- c(thirdClass, 1)
  }
  else{
    thirdClass <-c(thirdClass, 0)
  }
  
}

train3 <- subset(train, select = c(2,5,6,7,8) )
train4 <- cbind(train3, firstC = firstClass, secondC = secondClass, thirdC = thirdClass)

for(k in 1:length(train4$Sex)){
  if(train4$Sex[k] == 'female'){
    train4$Sex[k] <- 0
  }
  if(train4$Sex[k] == 'male'){
    train4$Sex[k] <- 1
  }
}

firstClass2 <- c()
secondClass2 <- c()
thirdClass2 <- c()

for(k in 1:length(test2$Pclass)){
  
  if(test2$Pclass[k] == 1){
    firstClass2 <- c(firstClass2, 1)
  }
  else{
    firstClass2 <-c(firstClass2, 0)
  }
  if(test2$Pclass[k] == 2){
    secondClass2 <- c(secondClass2, 1)
  }
  else{
    secondClass2 <-c(secondClass2, 0)
  }
  if(test2$Pclass[k] == 3){
    thirdClass2 <- c(thirdClass2, 1)
  }
  else{
    thirdClass2 <-c(thirdClass2, 0)
  }
  
}

test3 <- subset(test, select = c(4,5,6,7))
test4 <- cbind(test3, firstC = firstClass2, secondC = secondClass2, thirdC = thirdClass2)

for(k in 1:length(test4$Sex)){
  if(test4$Sex[k] == 'female'){
    test4$Sex[k] <- 0
  }
  if(test4$Sex[k] == 'male'){
    test4$Sex[k] <- 1
  }
}

# Build Caret Models: binary logistic regression, glmnet and random forest
set.seed(42)
y <- train$Survived

for (x in 1:length(y)){
  if(y[x] == 1){
    y[x] <- 'yes'
  }
  if(y[x] == 0){
    y[x] <- 'no'
  }
}

# Create custom indices: myFolds
myFolds <- createFolds(y, k = 3)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)

train4 <- subset(train4, select = c(2,3,4,5,6,7,8))

# Fit glm model: glm with median imputation
model_glm <- train(
  x = train4, y = as.factor(y),
  method = 'glm',
  trControl = myControl,
  preProcess = c('medianImpute', 'center','scale')
)

# Fit glm model: glm with knn imputation
model_glm2 <- train(
  x = train4, y = as.factor(y),
  method = 'glm',
  trControl = myControl,
  preProcess = 'knnImpute'
)

# Fit glm model: glm with knn imputation and pca for handling low variance columns
model_glm3 <- train(
  x = train4, y = as.factor(y),
  method = 'glm',
  trControl = myControl,
  preProcess = c('knnImpute', 'center','scale', 'pca')
)

age <- impute_median(train4$Age)

ageSex <- c()
for (k in 1:length(age)){
  ageSex[k] <- age[k] * as.numeric(train4$Sex[k])
}

trainT <- data.frame(Age = age, sex = train4$Sex, fClass = train4$firstC, sClass = train4$secondC, tClass = train4$thirdC,
                     Sibp = train4$SibSp, Parch = train4$Parch, ageSex = ageSex )

model_ranger <- train(
  x = trainT, y = y,
  method = 'rf',
  metric = 'ROC',
  trControl = myControl
)

trainT <- cbind(y, trainT)

model_decisionTree <- train(
  y ~ ., trainT,
  method = 'rpart',
  metric = 'ROC',
  trControl = myControl
)

# Create model_list
model_list <- list(item1 = model_glm2, item2 = model_ranger)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)

# Create bwplot
bwplot(resamples, metric = 'ROC')
plot(model_ranger$finalModel)
rpart.plot(model_decisionTree$finalModel)

######predict on test with glm2
age2 <- impute_median(test4$Age)
ageSex2 <- c()
for (k in 1:length(age2)){
  ageSex2[k] <- age2[k] * as.numeric(test4$Sex[k])
}
test5 <- data.frame(sex = test4$Sex, Age = age2, fClass = firstClass2, sClass = secondClass2, tClass = thirdClass2,
                    Sibp = test4$SibSp, Parch = test4$Parch, ageSex = ageSex2)

#fClass = train4$firstC, sClass = train4$secondC
p <- predict(model_ranger, test5, type = 'raw')
p <- as.character(p)

for(k in 1:length(p)){
  if(p[k] == 'yes'){
    p[k] <- 1
  }
  if(p[k] == 'no'){
    p[k] <- 0
  }
}

p <- as.numeric(p)

write.csv(data.frame(PassengerId = test$PassengerId, Survived = p), file = 'Submission8.csv')
