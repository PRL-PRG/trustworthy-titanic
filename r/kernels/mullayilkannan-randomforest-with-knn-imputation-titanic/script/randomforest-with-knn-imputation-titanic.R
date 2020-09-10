## Using R   random forest ,missing value imputation by knn
library(modelr)
library(broom)
library(mice)
library(DMwR)
 
#titanic <- knnImputation(titanic)
library(randomForest)
set.seed(12345)
train_set <- read.csv("../input/train.csv")
test_set <- read.csv("../input/test.csv")
test_set$Survived<-NA
dataset<-rbind(train_set,test_set)
summary(dataset)
dataset$Embarked[dataset$Embarked==""]<-"S" #replacing with the most frequent values
summary(dataset)
#relevant Attributes
relevant<-c("PassengerId","Pclass" , "Sex" , "Age" , "SibSp" ,"Parch" , "Fare" , "Embarked")

dataset1<-dataset[, relevant]
dataset1 <- knnImputation(dataset1)
dataset1$Survived=dataset$Survived # adding back the column survived
trainset=dataset1[1:nrow(train_set),]
trainset


summary(trainset)
validation_set=dataset1[-(1:nrow(train_set)),]
nrow(validation_set)
summary(validation_set)






smp_siz = floor(0.80*nrow(trainset))
print(smp_siz)
   
train_ind = sample(seq_len(nrow(trainset)),size = smp_siz)   
train =trainset[train_ind,]  
test=trainset[-train_ind,]  
   
fit<-randomForest(formula = as.factor(Survived) ~Sex + Age +  Fare+ Parch+Pclass+  SibSp , data = train, ntree = 2500, importance = TRUE) 
#fit<- randomForest(Survived ~ Age + Sex+Fare+Embarked+SibSp, data = train,ntree=100, proximity=T)
#"Pclass" , "Sex" , "Age" , "SibSp" ,"Parch" , "Fare" , "Embarked"
importance(fit)
                       
                         
test$PSurvived = predict(fit, test)
#test
 
 
CM<-table(test$Survived, test$PSurvived)
CM
accuracy = (sum(diag(CM)))/sum(CM)
print("accuracy=")
print(accuracy)
#fit1<-randomForest(formula = as.factor(Survived) ~  Sex + Age +  Fare+Embarked +   SibSp, data = trainset, ntree = 5000, importance = TRUE) 
#using the full dataset as training set

validation_set$Survived=predict(fit,validation_set)
#validation_set$Survived
  
my_solution=validation_set[c("PassengerId","Survived")]
#my_solution
write.csv(my_solution,'random_forest.csv',row.names=FALSE)
 