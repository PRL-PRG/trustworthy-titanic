## Disclaimer : Preprocessing steps are identical to this kernel :
# https://www.kaggle.com/jeetranjeet619/titanic-dataset-play-around-in-r/code
# Have a look at the above kernel for other ML example algorithms like SVM, Logistic regression,
# Random Forest, Gradient Boosting Machine etc.

## Libraries ####
library(dplyr)
# library(keras)
# library(randomForest)
library(caret)
library(mice) # missing value imputation
library(ggplot2)
library(ggthemes)
library(caTools) #for train and test split
# library(gbm)
# library(e1071) # for SVM 
# library(rpart) # for decision tree classifier
library(glue)
library(xgboost)
library(Matrix)

# Data Loading #### 
train = read.csv("../input/train.csv", stringsAsFactors = FALSE)
test = read.csv("../input/test.csv", stringsAsFactors = FALSE)
# combine both the test and train dataset 
# Why: to apply same cleaning steps to both of them simultaneously.
complete_data = bind_rows(train, test)

complete_data$Survived = as.factor(complete_data$Survived)
complete_data$Pclass = as.ordered(complete_data$Pclass)
complete_data$Sex = as.factor(complete_data$Sex)
complete_data$Age = as.numeric(complete_data$Age)
complete_data$Embarked = as.factor(complete_data$Embarked)


# check for completeness of the data ie checking NAs in the dataset
cbind(colSums(is.na(complete_data)))
# alternative way to do the same using sapply
# sapply(complete_data, function(x) {sum(is.na(x))})

# Fare ####
# one NA in Fare, 
which(complete_data$Fare %in% NA) # 1044
# check other details of row 1044 to impute it
complete_data[1044,]
# Pclass = 3, Embarked = S
glue("Mean of Fare for Pclass = 3 and Embarked = S : {mean(complete_data$Fare[complete_data$Pclass == '3' & complete_data$Embarked == 'S'], na.rm = T)}")
glue("Median of Fare for Pclass = 3 and Embarked = S : {median(complete_data$Fare[complete_data$Pclass == '3' & complete_data$Embarked == 'S'], na.rm = T)}")

ggplot(complete_data[complete_data$Pclass == '3' & complete_data$Embarked == 'S', ], 
       aes(x = Fare)) +
    geom_density(fill = 'green', alpha=0.9) + 
    geom_vline(aes(xintercept=median(Fare, na.rm=T)),
               colour='red', linetype='dashed', lwd=1) +
    geom_vline(aes(xintercept=mean(Fare, na.rm=T)),
               colour='blue', linetype='dashed', lwd=1) +
    theme_few() +
    labs(title = "Density Plot for Fare Distribution with Pclass = 3 and Embarked = S")

# ggplot(complete_data[complete_data$Pclass == '3' & complete_data$Embarked == 'S', ], 
#        aes(x = Fare)) +
#     geom_boxplot() + 
#     theme_few()

# impute fare with median of the Fare for Pclass = 3 and Embarked = S 
complete_data$Fare[1044] = median(complete_data$Fare[complete_data$Pclass == 3 & complete_data$Embarked == "S"], na.rm = T)

## Age ####
glue("Total NAs in Age : {sum(is.na(complete_data$Age))}.") # 263
# distribution of fare 
glue("Mean of Age :  {mean(complete_data$Age, na.rm = T)}")
glue("Median of Age :  {median(complete_data$Age, na.rm = T)}")

ggplot(data = complete_data, aes(x =Age, y = Age)) +
    geom_boxplot() +
    labs(title = "Age Distribution", x = "Age")

ggplot(data =  complete_data,aes(x = Age)) +
    geom_density(fill = "#B86997", alpha = 0.8) +
    geom_vline(aes(xintercept=median(Age, na.rm=T)),
               colour='black', linetype='dashed', lwd=1)+
    geom_vline(aes(xintercept=mean(Age, na.rm=T)),
               colour='blue', linetype='dashed', lwd=1) +
    labs(title = "Fare Distribution")
# impute Age with median : more values populated near median
complete_data$Age[is.na(complete_data$Age) == T] = median(complete_data$Age, na.rm = TRUE)
# alternate way to do the same imputation
# complete_data$Age = ifelse(is.na(complete_data$Age), median(complete_data$Age, na.rm = TRUE),complete_data$Age) 

### Cabin ####
# most of the Rows in Cabin are empty, not NA.
sum(is.na(complete_data$Cabin) | complete_data$Cabin == "") # 1014 empty rows
# 1014 out of 1309 ie 77.46% missing data therefore drop this from the feature list

# Embarked #####
# in Embarked, the rows are not exactly marked as NA but instead some have few empty strings so just checking for NA wont give you anything.
glue("NA/empty rows in Embarked Column : {sum(is.na(complete_data$Embarked) | complete_data$Embarked == '')}") 
# 2 empty  rows, check the row numbers so that we can impute it
which(is.na(complete_data$Embarked) | complete_data$Embarked == "") 
# passengerId = (62, 830)

# check other details for row number 62 and 830
complete_data[c(62, 830),]
# both have fare  = 80 and Pclass = 1, Sex = Female 
# Embarkment Distribution with plcass = 1 and Fare between 75-85
table(complete_data$Embarked[complete_data$Pclass ==1 & complete_data$Fare >= 75 & complete_data$Fare <= 85.0 & complete_data$Sex == "female"])
# C:14 and S:10
# lets check mean and median for both the cases to understand the class distribution
# average fare for class 1 female Passengers 
mean(complete_data$Fare[complete_data$Pclass == 1 & complete_data$Sex == 'female']) #109.4124
# median :
median(complete_data$Fare[complete_data$Pclass == 1 & complete_data$Sex == 'female']) #80.92915
# mean :
mean(complete_data$Fare[complete_data$Embarked == "C" & complete_data$Pclass == 1 & complete_data$Sex == 'female']) # 118.8959
mean(complete_data$Fare[complete_data$Embarked == "S" & complete_data$Pclass == 1 & complete_data$Sex == 'female']) # 101.0691
# S is closer
# median
median(complete_data$Fare[complete_data$Embarked == "C" & complete_data$Pclass == 1 & complete_data$Sex == 'female']) # 83.1583
median(complete_data$Fare[complete_data$Embarked == "S" & complete_data$Pclass == 1 & complete_data$Sex == 'female']) # 78.85
# again S is closer 
ggplot(complete_data[complete_data$Sex == 'female',], aes(x = Embarked, y = Fare, fill = factor(Pclass))) + 
    geom_boxplot() +
    geom_hline(aes(yintercept=80), 
               colour='red', linetype='dashed', lwd=2) +
    # scale_y_continuous(labels=dollar_format()) +
    theme_few()
# from the plot we can either use C or S for Embarkment. But from mean and median it is S. 
# conclusion : From the mean and median of the data we can observe that the S embarkment is more suitable in this case
complete_data$Embarked[c(62,830)] = "S"

# Plcass ####
# passenger of class 1 has higher survival rate
ggplot(complete_data[1:891,], aes(x = Pclass, fill = Survived)) +
    geom_bar(stat='count', position='dodge', color = "black") +
    labs(title = "Survival By Passenger Class", x = 'Passenger Class') +
    theme_few() + 
    geom_label(stat = "count",position = position_dodge(width = 1),hjust = "center", aes(label = ..count..)) 


# Feature Enginnering #####
# titles ####
for(i in 1:nrow(complete_data)){
    if(grepl(pattern = "Mr. ", x = complete_data$Name[i], ignore.case = TRUE) ==  1){
        complete_data$Title[i] = "Mr"
    }else if(grepl(pattern = "Mrs. ", x = complete_data$Name[i], ignore.case = TRUE) ==  1){
        complete_data$Title[i] = "Mrs"  
    }else if(grepl(pattern = "Miss. ", x = complete_data$Name[i], ignore.case = TRUE) == 1){
        complete_data$Title[i] = "Miss"
    }else if(grepl(pattern = "Master. ", x = complete_data$Name[i], ignore.case = TRUE) ==  1){
        complete_data$Title[i] = "Master"  
    }else{
        complete_data$Title[i] = "Rare" 
    }
}
complete_data$Title = as.factor(complete_data$Title)
sum(is.na(complete_data$Title)) 
# Title Distribution
cbind(table(complete_data$Title))

ggplot(complete_data[1:891,], aes(x = Title, fill = Survived)) +
    geom_histogram(stat='count', position='dodge', color = "black") +
    labs(title = "Title Wise Survival", x = 'Title', y = "Count") +
    geom_label(stat = "count",position = position_dodge(width = 1) ,hjust = "middle", aes(label = ..count..)) +
    theme_few()

# family surname ####
complete_data$Surname = sapply(complete_data$Name, function(x) strsplit(x, "[,.]")[[1]][1])
complete_data$Surname = as.factor(complete_data$Surname)        
glue("Number of Distinct Surname : {n_distinct(complete_data$Surname)}")

# family size onboard the ship ######
complete_data$Fsize = complete_data$SibSp + complete_data$Parch + 1 # including himself/herself

ggplot(complete_data[1:891,], aes(x = Fsize, fill = factor(Survived))) +
    geom_bar(stat='count', position='dodge', color = "black") +
    scale_x_continuous(breaks=c(min(complete_data$Fsize):max(complete_data$Fsize))) +
    labs(title = "Survival Based on Family Size",x = 'Family Size', label = "Survived") +
    #geom_label(stat = "count",position = position_dodge(width = 1) ,hjust = "middle",size = 3, aes(label = ..count..)) +
    theme_few()

# single travellers died most    
# Family size >7 : all died
# family sise between 2 to 4 have better survival rate

# Discrete family size ####
complete_data$FsizeDiscrete[complete_data$Fsize == 1] = "Singleton"
complete_data$FsizeDiscrete[complete_data$Fsize <= 5 & complete_data$Fsize > 1] = "Small"
complete_data$FsizeDiscrete[complete_data$Fsize >5] = "Large"

complete_data$FsizeDiscrete = as.factor(complete_data$FsizeDiscrete)
mosaicplot(table(complete_data$FsizeDiscrete, complete_data$Survived), main='Survival by Family Size ', shade=TRUE)


# Is solo traveller ?
# if complete_data$SibSp == 0 && complete_data$Parch == 0, solo traveller : no siblings, no child nor parent onboard 
complete_data$Solo = "No"
complete_data$Solo[complete_data$SibSp == 0 & complete_data$Parch == 0] = "Yes"
complete_data$Solo = as.factor(complete_data$Solo)

ggplot(data = complete_data[1:891,], aes(x = Solo, fill = as.factor(Survived))) +
    geom_bar(stat = "Count", position = "dodge", color = "black") +
    labs(title = "Solo Traveller Survival",x = "Solo Traveller ?")+
    geom_label(stat = "count",position = position_dodge(width = 1), hjust = "center", aes(label = ..count..))

# glue("Solo Travellers Survival rate : {(NROW(complete_data[complete_data$Survived == 1 & complete_data$Solo == 'Yes',])/NROW(complete_data[complete_data$Solo == 'Yes',])) * 100} %")

# ageGroup ####
# infants, children and old people are more likely to survive
for(i in 1:nrow(complete_data)){
    if(complete_data$Age[i] <=4){
        complete_data$AgeGroup[i] = "Infant" 
    }else if(complete_data$Age[i] > 4 & complete_data$Age[i] <= 10){
        complete_data$AgeGroup[i] = "Child"
    }else if(complete_data$Age[i] > 10 & complete_data$Age[i] <= 18){
        complete_data$AgeGroup[i] = "Young" 
    }else if(complete_data$Age[i] > 18 & complete_data$Age[i] <= 50){
        complete_data$AgeGroup[i] = "Adults"
    }else{
        complete_data$AgeGroup[i] = "Old" 
    }
}

complete_data$AgeGroup = as.factor(complete_data$AgeGroup)

ggplot(complete_data[1:891,], aes(x = AgeGroup, fill = Survived)) +
    geom_bar(stat='count', position='stack', color = "black") +
    labs(title = "Survival By Age Group",x = 'Age Group') +
    theme_few() 

ggplot(complete_data[1:891,], aes(x = AgeGroup, fill = Survived)) +
    geom_bar(stat='count', position='dodge',color = "black") +
    labs(title = "Survival By Age Group",x = 'Age Group') +
    theme_few() +
    geom_label(stat = "count",position = position_dodge(width = 1), hjust = "center", aes(label = ..count..))


# mother and child : likely to survive ####
complete_data$Mother = "Not Mother"
complete_data$Mother[complete_data$Sex == "female" & complete_data$Parch > 0 & complete_data$Age > 18 & complete_data$Title != "Miss"] = "Mother" 
complete_data$Mother = as.factor(complete_data$Mother)

ggplot(complete_data[1:891,], aes(x = Mother, fill = Survived)) +
    geom_bar(stat='count', position='dodge',color = "black") +
    labs(title = "Survival for Mothers",x = 'Mother') +
    geom_label(stat = "count",position = position_dodge(width = 1), hjust = "center", aes(label = ..count..))+
    theme_few()

# survival by gender
ggplot(complete_data[1:891,], aes(Age, fill = Survived)) +
    geom_histogram(color = "black") +
    facet_grid(.~Sex) +
    theme_few() + ggtitle("Survival by Age and Gender")


# fare range ####
# Fare distribution
ggplot(data = complete_data[1:891,], aes(x = Survived, y = Fare, fill = Survived)) +
    geom_boxplot()+
    xlab("Fare Box plot")

ggplot(data = complete_data[1:891,], aes(x = Fare, fill = Survived)) +
    geom_density(alpha = 0.7)+
    labs(title = "Fare Density Plot",x = "Fare")

glimpse(complete_data)
# Looks fine
md.pattern(complete_data[1:891,])
# Segregate training and test set
# train_set = complete_data[1:891,] 
# test_set = complete_data[892:nrow(complete_data),]
# test_set$Survived = NULL # delete Survived column from test set

# check for Factor variables 
cbind(sapply(complete_data,function(x){is.factor(x)}))

# Data Preparation for XGBoost Algorithm
# Convert Factor variables into one hot encooded sparse matrix
# sparse_data = sparse.model.matrix(PassengerId ~ Pclass +Sex +Embarked + Title +FsizeDiscrete + Solo + AgeGroup + Mother, 
#                                   data = complete_data)[, -1]

# filter already numeric data 
xgb_data = complete_data[,c("Survived","Age","SibSp","Parch","Fare","Fsize")]
# One-hot ecoding 
# i tried the spare matrix for one hot encoding but couldnt figure out :(
# results of sparse.model.matrix also didnt look convincing so i manually encoded them :( (stupid me) 

# cutting the drama, back to one hot encoding
#pclass
xgb_data$Plass_1 = ifelse(complete_data$Pclass == 1, 1, 0)
xgb_data$Plass_2 = ifelse(complete_data$Pclass == 2, 1, 0)
xgb_data$Plass_3 = ifelse(complete_data$Pclass == 3, 1, 0)

# Sex 
xgb_data$Sex_Male = ifelse(complete_data$Sex == "male", 1, 0)

# Embarked
xgb_data$Embarked_C = ifelse(complete_data$Embarked == "C", 1, 0)
xgb_data$Embarked_Q = ifelse(complete_data$Embarked == "Q", 1, 0)
xgb_data$Embarked_S = ifelse(complete_data$Embarked == "S", 1, 0)

# Title
xgb_data$Title_Mr = ifelse(complete_data$Title == "Mr", 1, 0)
xgb_data$Title_Mrs = ifelse(complete_data$Title == "Mrs", 1, 0)
xgb_data$Title_Miss = ifelse(complete_data$Title == "Miss", 1, 0)
xgb_data$Title_Master = ifelse(complete_data$Title == "Master", 1, 0)
xgb_data$Title_Rare = ifelse(complete_data$Title == "Rare", 1, 0)

# FsizeDiscrete
xgb_data$FsizeDiscrete_Singleton = ifelse(complete_data$FsizeDiscrete == "Singleton", 1, 0)
xgb_data$FsizeDiscrete_Small = ifelse(complete_data$FsizeDiscrete == "Small", 1, 0)
xgb_data$FsizeDiscrete_Large = ifelse(complete_data$FsizeDiscrete == "Large", 1, 0)

# Solo
xgb_data$Solo_Yes = ifelse(complete_data$Solo == "Yes", 1, 0)


# Age Group
xgb_data$AgeGroup_Infant = ifelse(complete_data$AgeGroup == "Infant", 1, 0)
xgb_data$AgeGroup_Child = ifelse(complete_data$AgeGroup == "Child", 1, 0)
xgb_data$AgeGroup_Young = ifelse(complete_data$AgeGroup == "Young", 1, 0)
xgb_data$AgeGroup_Adult = ifelse(complete_data$AgeGroup == "Adults", 1, 0)
xgb_data$AgeGroup_Old = ifelse(complete_data$AgeGroup == "Old", 1, 0)

# mother
xgb_data$Mother_Yes = ifelse(complete_data$Mother == "Mother", 1, 0)

# separate Training and testing dataset
training_data = xgb_data[1:891,]
testing_data = xgb_data[892:1309,]
testing_data$Survived = NULL # removing dependent variable from testing dataset

# bst.cv <- xgb.cv(params=paramList, 
#                  data=as.matrix(train_new[, features, with=FALSE]), 
#                  label=as.matrix(train_new$Income), 
#                  folds=testIdxs, 
#                  early.stop.round=3, 
#                  eval_metric="rmse", 
#                  nrounds=200, 
#                  prediction=TRUE)
# 
# bst <- xgboost(params=paramList, 
#                data=as.matrix(train_new[, features, with=FALSE]), 
#                label=as.matrix(train_new$Income), 
#                nrounds=nrow(bst.cv[["dt"]])-3)

paramList <- list(eta = 0.1, 
                  gamma = 0, 
                  max.depth = 10, 
                  min_child_weight = 5, 
                  subsample = 1.0, 
                  colsample_bytree = 0.2)

# Basic XGBoost Model
xgb_fit = xgboost(data = as.matrix(training_data[,-which(colnames(training_data) == "Survived")]),
                  label = as.matrix(training_data$Survived), 
                  params = paramList,
                  missing = NA,
                  nrounds = 50000,
                  verbose = 1,
                  eval_metric = "error", 
                  early_stopping_rounds = 2000, # Stop if it doesnt improve for 1000 iterations
                  objective = "binary:logistic")

importance <- xgb.importance(feature_names = colnames(testing_data), model = xgb_fit)
# in built feature importance plot from xgboost package
xgb.plot.importance(importance)

ggplot(data = importance, aes(x = reorder(Feature, Gain), 
                              y = Gain,
                              fill = Gain))+ 
    geom_bar(stat = "identity", color = "black") +
    coord_flip()+
    labs(title = "Feature Importance By XGBoost", x = "Features", y = "Gain") + 
    theme_calc() 
# geom_text(aes(label = round(Gain, 2)), position = position_dodge(width = 0.5), hjust = "right")

# Execution Log 
evaluation_log = as.data.frame(xgb_fit$evaluation_log)
colnames(evaluation_log)
ggplot(evaluation_log, mapping = aes(x = iter)) + 
    geom_line(aes(y = train_error, color = "Traning Error"))+ 
    labs(title = "Execution Error log",  x = "Iteration", y = "Error") + 
    theme_classic()


xgb_pred = predict(xgb_fit, as.matrix(testing_data))

xgb_result = data.frame(PassengerId = complete_data$PassengerId[892:1309], Survived = xgb_pred)

xgb_result$Survived = ifelse(xgb_result$Survived > 0.5, 1,0)

write.csv(xgb_result, "xgb_sub.csv", row.names = F)




# Tuning : Keep nrounds very high : likes of 1M
# reference : https://www.kaggle.com/c/bnp-paribas-cardif-claims-management/discussion/19083#108783
# Depth  = 10 -> 8, if error increases, then Depth = 12,
#    error for 12 lower than 10, then depth = 15 for next time
#    error is lower for 8 than 10, then try depth = 5 next time and so on.

# Subsample : start with 1.0 then change to 0.8
#               if error is higher , try 0.9
#               if still higher, use 1.0

#tune min_child_weight similarly (1,3,5)
#then colsamplebytree 
# then play with eta: learning rate
#       eta = 0.05 and get the optimum nrounds when error starts to increase in the wathclist progress.


# this is initial model , i will tune incrementaly.

# 20% validation data for watchlist.
len = round(nrow(training_data) * 0.8,0)

dtrain = xgb.DMatrix(data = as.matrix(training_data[1:len, -which(colnames(training_data) == "Survived")]), 
                     label = as.matrix(training_data$Survived[1:len]))
dtest = xgb.DMatrix(data = as.matrix(training_data[(len+1):891, -which(colnames(training_data) == "Survived")]), 
                    label = as.matrix(training_data$Survived[(len+1):891]))

watch_list = list(train = dtrain, test = dtest)

xgb_train_fit = xgb.train(data = dtrain,
                    eta = 0.1, 
                    gamma = 0, 
                    max.depth = 10, 
                    min_child_weight = 5, 
                    subsample = 1.0, 
                    colsample_bytree = 0.2,
                    watchlist = watch_list,
                    missing = NA,
                    nrounds = 50000,
                    early_stopping_rounds = 2000,
                    verbose = 1,
                    eval_metric = "error",
                    objective = "binary:logistic")


importance2 <- xgb.importance(feature_names = colnames(testing_data), model = xgb_train_fit)
xgb.plot.importance(importance2)
ggplot(data = importance2, aes(x = reorder(Feature, Gain), 
                              y = Gain,
                              fill = Gain))+ 
    geom_bar(stat = "identity",color = "black") +
    coord_flip()+
    labs(title = "Feature Importance By XGBoost with Watchlist and Early Stopping", x = "Features", y = "Gain") + 
    theme_calc() 
    # geom_text(aes(label = round(Gain, 2)), position = position_dodge(width = 0.5), hjust = "right")

# Execution Log 
evaluation_log = as.data.frame(xgb_train_fit$evaluation_log)
ggplot(evaluation_log, mapping = aes(x = iter)) + 
    geom_line(aes(y = train_error, color = "Traning Error"))+ 
    geom_line(aes(y = test_error, color = "Testing Error"))+
    labs(title = "Execution Error log",  x = "Iteration", y = "Error") + 
    theme_classic()


xgb_pred2 = predict(xgb_train_fit, as.matrix(testing_data))
xgb_result2 = data.frame(PassengerId = complete_data$PassengerId[892:1309], Survived = xgb_pred2)
xgb_result2$Survived = ifelse(xgb_result2$Survived > 0.5, 1,0)
write.csv(xgb_result2, "xgb_sub2.csv", row.names = F)
# END ####