##############################################################
# Ensenble Model using Caret Package                         #
# First Level: Random Forest, SVM, ADA, swdPoly, ANN, HDRDA  #
# Second Level: XGBOOST,SVM with Weight and ANN              #
# Third Level: mayor voting                                  #
# Score: 0.79                                                #
##############################################################
# libraries
library(caTools)
library(caret)
library(ggplot2)
library(ggthemes) 
library(scales)
library(dplyr)
library(DMwR)
library(qdapRegex)
library(e1071)
library(xgboost)

# joining  the data
train = read.csv('../input/train.csv') 
test  = read.csv('../input/test.csv') 
test = as.data.frame(append(test, list(Survived = 0), after = 1)) 
dataset = rbind(train,test) 



### Filling NA

# Cabin
dataset$Cabin =sapply(dataset$Cabin, function(x) {
  if(substr(x, 1, 1) != "") {
    return(substr(x, 1, 1))
  } else {
    return ("U")
  }
}
)

dataset$Cabin = factor(dataset$Cabin,
                       levels = c("A","B","C","D","E","F","G","T","U"),
                       labels = c(1,2,3,4,5,6,7,8,9))
levels(dataset$Cabin) =  c(1,2,3,4,5,6,7,8,9)
dataset$Cabin = factor(dataset$Cabin,
                       levels = c(1,2,3,4,5,6,7,8,9),
                       labels = c("A","B","C","D","E","F","G","T","U"))

# Embarked
subset(dataset, Embarked=="")


EmbarkedNA <- dataset %>%
  filter(PassengerId != 62 & PassengerId != 830)


ggplot(EmbarkedNA, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) + 
  xlab('Embarked') + ylab('Fare')

dataset$Embarked[c(62, 830)] <- 'C'

# Filling numeric values
dataset = knnImputation(data = dataset, k =10)
sum(is.na(dataset))



### Feature engineering

# Title
Mr = c("Mr", "Don", "Major", "Capt", "Jonkheer", "Rev", "Col", "Sir")
Mrs = c("Countess", "Mme", "Mrs")
Miss = c("Mlle", "Ms", "Miss", "Lady", "the Countess")
Dr = c("Dr")
Master = c("Master")



Title = sapply(dataset$Name, FUN = function(x) {
  if (is.element(rm_between(x, ",", ".", extract=TRUE), Mr)) {
    return("Mr") }
  else if (is.element(rm_between(x, ",", ".", extract=TRUE), Mrs)) {
    return("Mrs")} 
  else if (is.element(rm_between(x, ",", ".", extract=TRUE), Miss)) {
    return("Miss")}
  else if (is.element(rm_between(x, ",", ".", extract=TRUE), Master)) {
    return("Master")}
  else if (is.element(rm_between(x, ",", ".", extract=TRUE), Dr)) {
    return("Dr")}
  else {
    return("Ha")}
})


dataset$Title = Title
table(dataset$Title)
str(dataset)

dataset$Title = factor(dataset$Title,
                        levels = c("Mr", "Mrs", "Miss", "Master", "Dr", "Ha"),
                        labels = c(1,2,3,4,5,6))
dataset$Title[c(1306)] <- 2 # I do this to solve future problems with somes models. Because we have  diferents levels between test and train sets

# AgeRange
dataset$AgeRange[dataset$Age <11] = 1
dataset$AgeRange[dataset$Age > 9 & dataset$Age < 20 ] = 2
dataset$AgeRange[dataset$Age > 19 & dataset$Age < 30] = 3
dataset$AgeRange[dataset$Age > 29 & dataset$Age < 40] = 4
dataset$AgeRange[dataset$Age > 39 & dataset$Age < 55] = 5
dataset$AgeRange[dataset$Age > 54] = 6



dataset$AgeRange = factor(dataset$AgeRange,
                           levels = c(1,2,3,4,5,6),
                           labels = c("C"," T","Y","A","M","O"))

# Family Size
dataset = dataset %>% mutate(Fsize = SibSp + Parch +1)

dataset$Fsize[dataset$Fsize == 1] = 1 # Travel Alone
dataset$Fsize[dataset$Fsize == 2 ] = 2 # Small 
dataset$Fsize[dataset$Fsize > 2 & dataset$TamFam < 5] = 3 # Medium
dataset$Fsize[dataset$Fsize > 4] = 4 # Big

dataset$Fsize = factor(dataset$Fsize,
                        levels = c(1,2,3,4),
                        labels = c("Alone","Small","Medium","Big"))


#### Clean the dataset
str(dataset)
dataset = dataset[,-c(4,9)] # Droput the name and ticket features. I'm not going to use

dataset$Survived = factor(dataset$Survived,
                          levels = c(0,1),
                          labels = c("N","Y"))
dataset$Pclass = factor(dataset$Pclass,
                        levels = c(1,2,3),
                        labels =c('F','S','T'))

# Scaling the numerical values
dataset$Age = scale(dataset$Age)
dataset$SibSp = scale(dataset$SibSp)
dataset$Parch = scale(dataset$Parch)
dataset$Fare = scale(dataset$Fare)

### Split the data
train = dataset[1:891,]
test = dataset[892:1309,]
test = subset(test, select = -Survived)

set.seed(123)
split = sample.split(train$Survived, SplitRatio = 0.60)
training_set = subset(train, split == TRUE)
test_set = subset(train, split == FALSE)

### Definition of control sequence
equation1 = "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Cabin + Embarked + Title + AgeRange + Fsize"
formula_Survived = as.formula(equation1)

fitControl = trainControl(
  method = "cv",
  number = 5,
  savePredictions = 'final',
  classProbs = T)

### 1º level

model_rf = train(form = formula_Survived, data  = training_set,
                 method='rf', trControl=fitControl,tuneLength=5)

model_svm = train(form = formula_Survived, data  = training_set,
                 method="svmRadial", trControl=fitControl,tuneLength=5, scale = T)

model_ada = train(form = formula_Survived, data  = training_set,
                 method='adaboost', trControl=fitControl,tuneLength=3)

model_Knn = train(form = formula_Survived, data  = training_set,
                 method='knn', trControl=fitControl,tuneLength=3)

model_dwdPoly = train(form = formula_Survived, data  = training_set,
                  method='dwdPoly', trControl=fitControl,tuneLength=3)

model_avnnet = train(form = formula_Survived, data  = training_set,
                  method='avNNet', trControl=fitControl,tuneLength=3)


model_HDA = train(form = formula_Survived, data  = training_set,
                  method='hdrda', trControl=fitControl, tuneLength=3)


training_set$OOF_pred_rf = model_rf$pred$Y[order(model_rf$pred$rowIndex)]
training_set$OOF_pred_knn = model_Knn$pred$Y[order(model_Knn$pred$rowIndex)]
training_set$OOF_pred_svm = model_svm$pred$Y[order(model_svm$pred$rowIndex)]
training_set$OOF_pred_ada = model_ada$pred$Y[order(model_ada$pred$rowIndex)]
training_set$OOF_pred_dwd  = model_dwdPoly$pred$Y[order(model_dwdPoly$pred$rowIndex)]
training_set$OOF_pred_avnnet = model_avnnet$pred$Y[order(model_avnnet$pred$rowIndex)]
training_set$OOF_pred_HDA = model_HDA$pred$Y[order(model_HDA$pred$rowIndex)]

test_set$OOF_pred_rf = predict(model_rf,test_set[-c(1,2)],type='prob')$Y
test_set$OOF_pred_knn = predict(model_Knn,test_set[-c(1,2)],type='prob')$Y
test_set$OOF_pred_svm = predict(model_svm,test_set[-c(1,2)],type='prob')$Y
test_set$OOF_pred_ada = predict(model_ada,test_set[-c(1,2)],type='prob')$Y
test_set$OOF_pred_dwd = predict(model_dwdPoly,test_set[-c(1,2)],type='prob')$Y
test_set$OOF_pred_avnnet = predict(model_avnnet,test_set[-c(1,2)],type='prob')$Y
test_set$OOF_pred_HDA = predict(model_HDA,test_set[-c(1,2)],type='prob')$Y

test$OOF_pred_rf = predict(model_rf,test[-c(1)],type='prob')$Y
test$OOF_pred_knn = predict(model_Knn,test[-c(1)],type='prob')$Y
test$OOF_pred_svm = predict(model_svm,test[-c(1)],type='prob')$Y
test$OOF_pred_ada = predict(model_ada,test[-c(1)],type='prob')$Y
test$OOF_pred_dwd = predict(model_dwdPoly,test[-c(1)],type='prob')$Y
test$OOF_pred_avnnet = predict(model_avnnet,test[-c(1)],type='prob')$Y
test$OOF_pred_HDA = predict(model_HDA,test[-c(1)],type='prob')$Y

### Level 2

equation2 = "Survived ~  OOF_pred_rf + OOF_pred_knn + OOF_pred_svm + OOF_pred_ada + OOF_pred_dwd + OOF_pred_avnnet"
formula_2lvl = as.formula(equation2)

model_xgb = train(form = formula_2lvl, data  = test_set,
                 method='xgbLinear', trControl=fitControl, tuneLength=3)
test$stack_XGB = predict(model_xgbTree,test[-c(1)])


model_SVMW = train(form = formula_2lvl, data  = test_set,
                    method='svmLinearWeights2', trControl=fitControl, tuneLength=3)
test$stack_SVMW = predict(model_SVMW,test[-c(1)])

model_ANN = train(form = formula_2lvl, data  = test_set,
                   method='avNNet', trControl=fitControl, tuneLength=3)

test$stack_ANN = predict(model_ANN,test[-c(1)])

# Next Lvl Mayor voting

test$pred_majority<-as.factor(ifelse(test$stack_XGB=='Y' & test$stack_SVMW=='Y','Y',
                                        ifelse(test$stack_XGB=='Y' & test$stack_ANN=='Y','Y',
                                               ifelse(test$stack_SVMW=='Y' & test$stack_ANN=='Y','Y','N'))))
### Write the solution
solution = data.frame(PassengerId = test$PassengerId, Survived = test$stack_XGB == "Y", row.names = NULL)
solution$Survived = (solution$Survived == T)
solution$Survived = as.integer(solution$Survived)
write.csv(solution, 'solution.csv', row.names = F)