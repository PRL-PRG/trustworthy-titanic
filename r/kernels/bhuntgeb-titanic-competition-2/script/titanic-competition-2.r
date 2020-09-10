
library(ggplot2)
library(caret)
library(dplyr)
library(doSNOW)

train<-read.csv2(file="../input/train.csv", sep=",", colClasses = c(rep("character",12)))


train$Survived<-as.factor(train$Survived)       
train$Pclass<-as.factor(train$Pclass)           
train$Sex<-as.factor(train$Sex)         
train$Age<-as.numeric(train$Age)      
train$SibSp<-as.numeric(train$SibSp)      
train$Parch<-as.numeric(train$Parch)      
train$Ticket<-as.character(train$Ticket)    
train$Fare<-as.numeric(train$Fare)      
train$Embarked<-as.factor(train$Embarked)      
#train$MissingAge<-as.factor(train$MissingAge)
#train$FamilySize<-as.numeric(train$FamilySize)

summary(train)

g<-ggplot(train, aes(Age, color = Survived))
g+geom_density(na.rm = T)+
        geom_vline( aes(xintercept = mean(train$Age[train$Survived==0], na.rm=T), color="0"))+
        geom_vline( aes(xintercept = mean(train$Age[train$Survived==1], na.rm=T), color="1"))

# Parch >2 
g<-ggplot(train[train$Parch>2,], aes(Age, color = Survived))
g+geom_density( na.rm = T)

g<-ggplot(train, aes(Pclass, color = Survived))
g+geom_bar( fill="white")+facet_grid(. ~ Sex)

g<-ggplot(train, aes( Embarked, color = Survived))
g+geom_bar( fill="white")


# Add feature for tracking missing ages (not used in futher prediction)
train$MissingAge<- ifelse(is.na(train$Age), "Y", "N")
train$MissingAge<-as.factor(train$MissingAge)

# Add a feature for familiy size (not used in futher prediction)
train$FamilySize<-1 + as.numeric(train$SibSp) + as.numeric(train$Parch)

# Grouping by Ticket  (not used in futher prediction)
tickets_person_count<-train %>% group_by(Ticket) %>% summarize(person_count = n())

# Merging by Ticket (not used in futher prediction)
train <- merge(train, tickets_person_count, by.x = "Ticket", by.y = "Ticket", all = T)

# Calculate per Person Ticket-Fare (not used in futher prediction)
train <-train %>% mutate( Fare_pP = Fare/person_count)
train<-arrange(train, PassengerId)

# Impute missing embarked with mode
train$Embarked[train$Embarked==""]<-"S"

# select used features 
#features<- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked", "Fare", "MissingAge", "FamilySize")
features<- c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch" ,"Embarked", "Fare")
train<-train[,features]

# Tranform some factor variables to dummy vars
library(dummies)
train.dummy<-dummy.data.frame(train, names = c("Pclass","Sex", "Embarked"))

# Impute missing ages
pre.process<-preProcess(train.dummy[,-1], method = "bagImpute")
imputed.data<-predict(pre.process, train.dummy)

# Impute missing age value with normal distribued random variables
#age_mean<-mean(train.dummy$Age, na.rm = TRUE)
#age_sd<-sd(train.dummy$Age, na.rm = TRUE)
#num_age_isna_train<-sum(is.na(train.dummy$Age))
#train.dummy[((is.na(train.dummy$Age)) | (train.dummy$Age>105)),7]<- round(abs(rnorm(num_age_isna_train, mean =age_mean , sd = age_sd )))
#imputed.data<-train.dummy

#train$Age<-imputed.data[,6]

train<-imputed.data


# Split data
set.seed(54321)
indexes<- createDataPartition(train$Survived,
                              times = 1,
                              p=0.7,
                              list=FALSE)
titanic.train<-train[indexes,]
titanic.test<- train[-indexes,]

#exermine prop of surived
#prop.table(table(train$Survived))
#prop.table(table(titanic.train$Survived))
#prop.table(table(titanic.test$Survived))

# Setup caret
train.control<-trainControl(method="repeatedcv",
                            number = 10,
                            repeats = 3,
                            search = "grid")

# setup tune grid for xgbTree
#tune.grid<-expand.grid(eta=c(0.05,0.075, 0.1),
#                       nrounds= c(50,75,100),
#                       max_depth= 6:8,
#                       min_child_weight = c(2.0, 2.25, 2.5 ),
#                       colsample_bytree = c(0.3, 0.4, 0.5),
#                       gamma =0,
#                       subsample =1)

# setup tune grid for gbm
tune.grid<-expand.grid(n.trees = c( 200, 300,400, 500), 
                       interaction.depth = c(4:12),
                       shrinkage = 0.001,
                       n.minobsinnode = c(5:15))

# create doSNOW Cluster
cl<-makeCluster(16, type = "SOCK")

registerDoSNOW(cl)



# Train with caret ->whol train dataset and xgbTree
#caret.cv<-train(Survived ~.,
#                data=train,
#                method ="xgbTree",
#                tuneGrid = tune.grid,
#                trControl= train.control)

# Train with caret ->whol train dataset and gbm
caret.cv<-train(Survived ~.,
                data=train,
                method ="gbm",
                tuneGrid = tune.grid,
                trControl= train.control,
                bag.fraction = 0.5,
                verbose = FALSE)

caret.cv$bestTune
stopCluster(cl)

preds<-predict(caret.cv, titanic.test)

confusionMatrix(preds, titanic.test$Survived)


# Loading the test dataset
test<-read.csv2(file="../input/test.csv", sep=",", colClasses = c(rep("character",11)))

test$PassengerId<-as.numeric(test$PassengerId) 
test$Pclass<-as.factor(test$Pclass)     
test$Sex<-as.factor(test$Sex)       
test$Age<-as.numeric(test$Age)      
test$SibSp<-as.numeric(test$SibSp)      
test$Parch<-as.numeric(test$Parch)      
test$Ticket<-as.character(test$Ticket)    
test$Fare<-as.numeric(test$Fare)      
test$Embarked<-as.factor(test$Embarked)       


# create some additional features
# Add feature for tracking missing ages
test$MissingAge<- ifelse(is.na(test$Age), "Y", "N")
test$MissingAge<-as.factor(test$MissingAge)

# Add a feature for familiy size
test$FamilySize<-1 + as.numeric(test$SibSp) + as.numeric(test$Parch)
test$FamilySize<-as.numeric(test$FamilySize)

# Grouping by Ticket 
tickets_person_count_test<-test %>% group_by(Ticket) %>% summarize(person_count = n())

# Merging by Ticket
test <- merge(test, tickets_person_count_test, by.x = "Ticket", by.y = "Ticket", all = T)

# Calculate per Person Ticket-Fare
# Impute missing Fare
test[is.na(test$Fare) ,9]<-mean(test$Fare, na.rm = TRUE)

test <-test %>% mutate( Fare_pP = Fare/person_count)
test<-arrange(test, PassengerId)


# select used features
#features_test<- c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked", "Fare", "MissingAge", "FamilySize")
features_test<- c( "Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked", "Fare")
test<-test[,features_test]

# Tranform all features to dummy vars
test.dummy<-dummy.data.frame(test, names = c("Pclass","Sex", "Embarked"))



# Immute missing embarked with mode
test$Embarked[test$Embarked==""]<-"S"

# Impute missing ages
pre.process<-preProcess(train.dummy[,-1], method = "bagImpute")
imputed.data.test<-predict(pre.process, test.dummy)

#num_age_isna_test<-sum(is.na(test.dummy$Age))
#test.dummy[((is.na(test.dummy$Age)) | (test.dummy$Age>105)),6]<- round(abs(rnorm(num_age_isna_test, mean =age_mean , sd = age_sd )))
#imputed.data.test<-test.dummy

#test$Age<-imputed.data.test[,5]
test<-imputed.data.test

# Do the prediction with our trained model
preds2<-predict(caret.cv, test)

result<-data.frame(c(892:1309), as.character(preds2))
colnames(result)<-c("PassengerId", "Survived")
#write.csv(result, "../input/result.csv", row.names=FALSE, quote = FALSE)
result
