
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)
library(Matrix)
library(xgboost)

# The train and test data is stored in the ../input directory
temptrain <- read.csv("../input/train.csv")
temptest  <- read.csv("../input/test.csv")

train<-temptrain[,c("SibSp","Parch","Sex","Pclass", "Fare","Survived")]
test<-temptest[,c("SibSp","Parch","Sex", "Pclass", "Fare","PassengerId")]

train[,"Sex"]<-as.numeric(train[,"Sex"])
test[,"Sex"]<-as.numeric(test[,"Sex"])
train[,"Pclass"]<-as.numeric(train[,"Pclass"])
test[,"Pclass"]<-as.numeric(test[,"Pclass"])

train[is.na(train)]<-0
test[is.na(test)]<-0

#Silly way to force the dataframe into numeric. I spent so long frustrated with this!
train[[1,"SibSp"]]<-1.00
train[[1,"Parch"]]<-0.00

test[[1,"SibSp"]]<-0.00
test[[1,"Parch"]]<-0.00

str(train)
str(test)
summary(train)
summary(test)


param       = list("objective" = "multi:softmax", # multi class classification
	      "num_class"= 2 ,  		# Number of classes in the dependent variable.
              "eval_metric" = "mlogloss",  	 # evaluation metric 
              "nthread" = 8,   			 # number of threads to be used 
              "max_depth" = 16,    		 # maximum depth of tree 
              "eta" = 0.3,    			 # step size shrinkage 
              "gamma" = 0,    			 # minimum loss reduction 
              "subsample" = 0.7,    		 # part of data instances to grow tree 
              "colsample_bytree" = 1, 		 # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 12  		 # minimum sum of instance weight needed in a child 
              )

#Identify the Predictors and the dependent variable, aka label.
predictors = colnames(train[,-ncol(train)])
str(predictors)
#xgboost works only if the labels are numeric. Hence, convert the labels (Species) to numeric.
label = as.numeric(train[,"Survived"])
print(table (label))
#Alas, xgboost works only if the numeric labels start from 0. Hence, subtract 1 from the label.
print(table (label))

bst = xgboost(
		param=param,
		data =as.matrix(train[,predictors]),
		label = label,
		nrounds=1000)

# Make prediction on the testing data.

train.prediction = predict(bst, as.matrix(test))

output<-data.frame(test$PassengerId,train.prediction)

colnames(output)<-cbind("PassengerId","Survived")

write.csv(output, file = 'Rushton_Solution.csv', row.names = F)



