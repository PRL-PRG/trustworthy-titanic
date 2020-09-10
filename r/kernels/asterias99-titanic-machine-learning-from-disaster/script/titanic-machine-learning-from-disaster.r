
# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(lattice) # Requirement for 'caret'-package
library(caret) # Missclassification Error
library(mice) # Missing Value imputation

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

#### Import training and test dataset
train <- read.csv(file = '../input/train.csv')
test <- read.csv(file = '../input/test.csv')


str(train)

# Subset of data
trainImp <- train[,c(2, 3, 5, 6)]
testImp <- test[,c(2, 4, 5)]

# Make sure to add useNA argument.
table(trainImp$Age, useNA = "ifany")
# Percent of NA
print(paste0("Percent of NA ", mean(is.na(train$Age))))

attach(trainImp)
table(Pclass, Survived, useNA = "ifany")
prop.table(table(Pclass, Survived, useNA = "ifany"),1)
print("========================================")
table(Sex, Survived, useNA = "ifany")
prop.table(table(Sex, Survived, useNA = "ifany"),1)
detach(trainImp)

ggplot(data = trainImp, aes(x = Age, fill = as.factor(Survived))) + geom_histogram(alpha = .5) + theme_minimal()

trainLogistic <- glm(formula = Survived ~ Age + Sex + Pclass,
                     family = binomial(),
                     data = trainImp)
summary(trainLogistic)
trainPred <- (predict(object = trainLogistic, newdata = trainImp[ ,-1]) > 1) * 1 # Remove response-variable
confusionMatrix(data = table(trainPred, trainImp$Survived), dnn = c("Prediction", "Reference"))


set.seed(123)

trainMice <- mice(data = train[ ,c(2, 3, 5, 6, 7, 8, 10)], imputationMethod = 'rf', maxit = 5, m = 1, printFlag = FALSE)
testMice <- mice(data = test[ ,c(1, 2, 4, 5, 6, 7)], imputationMethod = 'rf', maxit = 5, m = 1, printFlag = FALSE)
trainMiceComplete <- complete(trainMice)
testMiceComplete <- complete(testMice)


trainLogisticMice <- glm(formula = Survived ~ Sex + Pclass + Age,
                         family = binomial(),
                         data = trainMiceComplete[,c(1, 2, 3, 4)])
summary(trainLogisticMice)
trainPred <- (predict(object = trainLogisticMice, newdata = trainImp[ ,-1]) > 1) * 1 # Remove response-variable
confusionMatrix(data = table(trainPred, trainImp$Survived), dnn = c("Prediction", "Reference"))

trainMiceComplete$AgeInterval <- cut(trainMiceComplete$Age,
                                    breaks = c(0, 20, 40, Inf),
                                    labels = c('Children','MiddleAge', 'Older'),
                                    right = FALSE)

testMiceComplete$AgeInterval <- cut(testMiceComplete$Age,
                                    breaks = c(0, 20, 40, Inf),
                                    labels = c('Children','MiddleAge', 'Older'),
                                    right = FALSE)

trainLogisticMice <- glm(formula = Survived ~ Sex + Pclass + AgeInterval,
                         family = binomial(),
                         data = trainMiceComplete[,c(1, 2, 3, 5, 8)])
summary(trainLogisticMice)
trainPred <- (predict(object = trainLogisticMice, newdata = trainMiceComplete) > 1) * 1 # Remove response-variable
confusionMatrix(data = table(trainPred, trainImp$Survived), dnn = c("Prediction", "Reference"))

testPred <- (predict(object = trainLogisticMice, newdata = testMiceComplete) > 1) * 1 # Predict test dataset
Submission <- data.frame("PassengerId" = testMiceComplete$PassengerId, "Survived" = testPred)

Since this is my first Kernel and first participation in a competition on Kaggle I feel like i am satisfied.
I will try and submit my result.

write.csv(Submission,"Submission_titanic.csv",row.names = FALSE)






















