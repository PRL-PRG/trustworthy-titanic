library(dplyr);library(caret);library(ggplot2);library(purrr);

train <- read.csv("train.csv",stringsAsFactors=FALSE)
test <- read.csv("test.csv",stringsAsFactors=FALSE)

#combine them 
test$Survived <- NA
Full <- rbind(train,test)

head(Full)
str(Full)


################## Data Cleaning ######################################

age <- Full$Age
n <- length(age)
     #replace the missing values with a random sample from the data
set.seed(123)
  for(i in 1:n){
    if(is.na(age[i])){
      age[i] = sample(na.omit(Full$Age),1)
    }
  }

# plot effect of age cleaning

par(mfrow=c(1,2))
hist(Full$Age, freq=F, main='Before Replacement', 
     col='lightblue', ylim=c(0,0.04),xlab = "age")
hist(age, freq=F, main='After Replacement', 
     col='darkblue', ylim=c(0,0.04))

# see the number of cabins each passenger has 

cabin <- Full$Cabin
n = length(cabin)
for(i in 1:n){
  if(nchar(cabin[i]) == 0){
    cabin[i] = 0
  } else{
    s = strsplit(cabin[i]," ")
    cabin[i] = length(s[[1]])
  }
} 
table(cabin)

# check the missing row value in Fare

which(is.na(Full$Fare))

# for Fare column, we repace the missing value with median fare of according Pclass and Embarked
Full$Fare[1044] <- median(Full[Full$Pclass == '3' & Full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# process embarked column

embarked <- Full$Embarked
n = length(embarked)
for(i in 1:n){
  if(embarked[i] != "S" && embarked[i] != "C" && embarked[i] != "Q"){
    embarked[i] = "S"
  }
}
table(embarked)

############################## Data processing and Ex.Analysis ###############

# Age ~ Survival 

# number of survivals and nonsurvivals across different age
d <- data.frame(Age = age[1:891], Survived = train$Survived)
ggplot(d, aes(Age,fill = factor(Survived))) +
  geom_histogram()

# create bar chart to show relationship between survival rate and age intervals
cuts <- cut(d$Age,hist(d$Age,10,plot = F)$breaks)
rate <- tapply(d$Survived,cuts,mean)
d2 <- data.frame(age = names(rate),rate)
barplot(d2$rate, xlab = "age",ylab = "survival rate")

# create histgram to show effect of Sex on survival
ggplot(train, aes(Sex,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# calculate survival rate
tapply(train$Survived,train$Sex,mean)


# extract title from Name 
# (here I process Full data set but only plot title vs survival in train 
#    data set because there is no survival value for test data set)
n = length(Full$Survived)
title = rep(NA,n)
for (i in 1:n){
  lastname = strsplit(Full$Name[i],", ")[[1]][2]
  title[i] = strsplit(lastname,". ")[[1]][1]
}

# make a histogram of title v.s survival
d <- data.frame(title = title[1:891],Survived = train$Survived)
ggplot(d, aes(title,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# count of title
table(title)

# survival rate
tapply(d$Survived,d$title,mean)

# replace rare titles to 'Rare'
title[title != 'Mr' & title != 'Miss' & title != 'Mrs' & title != 'Master'] <- 'Rare'
table(title)

# make a histogram Pclass v.s. Survival
ggplot(train, aes(Pclass,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# calculate survival rate
tapply(train$Survived,train$Pclass,mean)


# histogram of Parch / Family Size v.s. Survival
ggplot(train, aes(Parch,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# histogram of SibSp /  Family Size v.s. Survival
ggplot(train, aes(SibSp,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# combine SibSp and Parch 
family <- Full$SibSp + Full$Parch
d <- data.frame(family = family[1:891],Survived = train$Survived)
ggplot(d, aes(family,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# calculate survival rate
tapply(d$Survived,d$family,mean)

# Cabin vs Survival 

# create histogram
d <- data.frame(Cabin = cabin[1:891],Survived = train$Survived)
ggplot(d, aes(Cabin,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# calculate survival rate
tapply(d$Survived,d$Cabin,mean)

#Fare v.s. Survival

# make a histogram
ggplot(train, aes(Fare,fill = factor(Survived))) +
  geom_histogram()

# calculate
cuts <- cut(train$Fare,hist(train$Fare,10,plot = F)$breaks)
rate <- tapply(train$Survived,cuts,mean)
d <- data.frame(fare = names(rate),rate)
barplot(d$rate, xlab = "fare",ylab = "survival rate")

# Embarked v.s. Survival

# make histogram
d <- data.frame(Embarked = embarked[1:891], Survived = train$Survived)
ggplot(d, aes(Embarked,fill = factor(Survived))) +
  geom_histogram(stat = "count")

# calculate survival rate
tapply(train$Survived,train$Embarked,mean)

###################################################### MODELING ###################################

## Feature engeneering 

# response variable
f.survived = train$Survived

# feature

 # 1. age
f.age = age[1:891]    # for fiting
t.age = age[892:1309]  # for testing

# 2. fare
 f.fare = Full$Fare[1:891]
t.fare = Full$Fare[892:1309]

# 3. cabin
f.cabin = cabin[1:891]
t.cabin = cabin[892:1309]

# 4. title
f.title = title[1:891]
t.title = title[892:1309]

# 5. family
family <- Full$SibSp + Full$Parch
f.family = family[1:891]
t.family = family[892:1309]

# 6. plcass
f.pclass = train$Pclass
t.pclass = test$Pclass

# 7. sex
f.sex = train$Sex
t.sex = test$Sex

# 8. embarked
f.embarked = embarked[1:891]
t.embarked = embarked[892:1309]

##### Training #########

# construct training data frame
new_train = data.frame(survived = f.survived, age = f.age, fare = f.fare , sex = f.sex, 
                       embarked = f.embarked ,family = f.family ,title = f.title ,cabin =  f.cabin, pclass= f.pclass)

# logistic regression
fit_logit <- glm(factor(survived) ~ age + fare + sex + embarked + family 
                 + title + cabin + pclass,data = new_train,family = binomial)
# predicted result of regression
ans_logit = rep(NA,891)
for(i in 1:891){
  ans_logit[i] = round(fit_logit$fitted.values[[i]],0)
}
# check result
mean(ans_logit == train$Survived)
table(ans_logit)

trControl <- trainControl(method = "cv", number = 5,
                         repeats = 5, verboseIter = TRUE,
                         classProbs = TRUE)

fit_glmnet <- train(factor(survived) ~ .,
                    data = new_train, 
                    method = "glmnet",
                    trControl = trainControl(method = "cv", number = 5,
                                  repeats = 5, verboseIter = TRUE ),
                    preProcess = c("center","scale"))
fit_glmnet                   
              

# Random Forest 

fit_forest <- train(factor(survived) ~ .,
                    new_train,
                    method = "ranger",
                    tuneLength = 5,
                    trControl = trainControl(method = "cv", number = 5,
                                             repeats = 5, verboseIter = TRUE ))

fit_forest


# Support Vector Machine / SVM

fit_SVM <- train(factor(survived) ~ .,
                 new_train,
                 method = "svmLinear",
                 trControl = trainControl(method = "cv", number = 5,
                                          repeats = 5, verboseIter = TRUE )
                 )

fit_SVM



fit_support <- svm(factor(survived) ~ .,data = new_train)

# predicted result of regression
svm.fitted = predict(fit_support)
ans_svm = rep(NA,891)
for(i in 1:891){
  ans_svm[i] = as.integer(svm.fitted[[i]]) - 1
}
# check result
mean(ans_svm == train$Survived)
table(ans_svm)





list_caret <- make.names(caretList(as.factor(survived) ~ .,
          new_train,
          methodList = c("glmnet","svmLinear"),
          trControl = trainControl(method = "cv", number = 5,
                                   repeats = 5, verboseIter = TRUE, classProbs = TRUE )),unique = TRUE)

########### predictions #################


# construct testing data frame
test_data_set <- data.frame(age = t.age, fare = t.fare, sex = t.sex, embarked = t.embarked, 
                            family = t.family, title = t.title,cabin =  t.cabin, pclass = t.pclass)
# make prediction
svm_predict = predict(fit_SVM,newdata = test_data_set )
ans_svm_predict = rep(NA,418)
for(i in 1:418){
  ans_svm_predict[i] = as.integer(svm_predict[[i]]) - 1
}
table(ans_svm_predict)


glmnet_predict = predict(fit_glmnet,newdata = test_data_set )
ans_glmnet_predict = rep(NA,418)
for(i in 1:418){
  ans_glmnet_predict[i] = as.integer(glmnet_predict[[i]]) - 1
}
table(ans_glmnet_predict)


forest_predict = predict(fit_forest,newdata = test_data_set )
ans_forest_predict = rep(NA,418)
for(i in 1:418){
  ans_forest_predict[i] = as.integer(forest_predict[[i]]) - 1
}
table(ans_forest_predict)

# make prediction
support_predict = predict(fit_support,newdata = test_data_set )
ans_support_predict = rep(NA,418)
for(i in 1:418){
  ans_support_predict[i] = as.integer(support_predict[[i]]) - 1
}
table(ans_support_predict)

# Ensamble 

model_list <- list(glmnet = fit_glmnet, SVM = fit_SVM)

# Pass model_list to resamples(): resamples
resamples <- resamples(list_caret)

# Summarize the results
summary(resamples)

bwplot(resamples)

summary.resamples()

#######STACK#############

library(caretEnsemble);
stack <- caretStack(list_caret, 
                    method = "glmnet")

summary(stack)

# create a csv file for submittion
d<-data.frame(PassengerId = test$PassengerId, Survived = ans_support_predict)
write.csv(d,file = "TitanicResult1.csv",row.names = F)


