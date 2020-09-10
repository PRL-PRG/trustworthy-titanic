

# Importing the dataset
dataset_train = read.csv('../input/train.csv')
dataset_test = read.csv('../input/test.csv')

# Encoding the target feature as factor
dataset_train$Survived = factor(dataset_train$Survived, levels = c(0, 1))
#dataset_test$Survived = factor(dataset_test$Survived, levels = c(0, 1))
# Encoding categorical data
dataset_train$Sex = factor(dataset_train$Sex,
                         levels = c('female', 'male'),
                         labels = c(0, 1))
dataset_test$Sex = factor(dataset_test$Sex,
                           levels = c('female', 'male'),
                           labels = c(0, 1))
dataset_train$Age = ifelse(is.na(dataset_train$Age),
                     ave(dataset_train$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset_train$Age)
dataset_test$Age = ifelse(is.na(dataset_test$Age) ,
                          29.69912,
                           dataset_test$Age)

dataset_test$Fare = ifelse(is.na(dataset_test$Fare),
                           ave(dataset_train$Fare, FUN = function(x) mean(x, na.rm = TRUE)),
                          dataset_test$Fare)

dataset_train_short = dataset_train[,c(3,6,7,8,10,5,2)]
dataset_test_short = dataset_test[,c(2,5,6,7,9,4)]
#dataset_test_short$Survived = dataset_labelTest$Survived

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
#split = sample.split(dataset$Purchased, SplitRatio = 0.75)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)

# Feature Scaling
summary(dataset_train_short)
dataset_train_short[c(-6,-7)] = scale(dataset_train_short[c(-6,-7)])
dataset_test_short[c(-6)] = scale(dataset_test_short[c(-6)])
#test_set[-3] = scale(test_set[-3])

# Fitting Decision Tree Classification to the Training set
# install.packages('rpart')

# library(rpart)
# classifier = rpart(formula = Survived ~ .,
#                    data = dataset_train_short)
#install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = dataset_train_short[c(-6,-7)],
                          y = dataset_train_short$Survived,
                          ntree = 500)

# Predicting the Test set results
y_pred = predict(classifier, newdata = dataset_test_short, type = 'class')
y_pred_train = predict(classifier, newdata = dataset_train_short[-7], type = 'class')
# Making the Confusion Matrix
#cm  = table(dataset_test_short[, 7], y_pred)
cm_train = table(dataset_train_short[, 7], y_pred_train)
train_accuracy=(cm_train[1,1] + cm_train[2,2]) / (cm_train[1,1] + cm_train[2,2]+ cm_train[1,2] + cm_train[2,1])
#test_accuracy=(cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2]+ cm[1,2] + cm[2,1])

#creating output file for kaggle competition submission
outputFile = cbind(dataset_test$PassengerId, as.numeric(as.character(y_pred)))
colnames(outputFile) <- c("PassengerId","Survived")
write.csv(outputFile,'titanicPrediction1.csv',row.names=FALSE) #row.names=FALSE is to prevent indexes showing up as first column in file
