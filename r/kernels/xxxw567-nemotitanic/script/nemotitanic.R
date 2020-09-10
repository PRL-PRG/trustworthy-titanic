library(randomForest)
library(mice)
library(MASS)
library(C50)
# Read the data
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
summary(train)

##############################################################
# 1.  Dataclean                                              #
##############################################################
# impute missing data
# PMM see http://www.ats.ucla.edu/stat/r/faq/R_pmm_mi.htm for more detail
imputed_train <- mice(train[,c("Survived","Pclass","Sex","Age",
                        "SibSp","Parch","Fare","Embarked")],
                        m=5,maxit=50,meth='pmm',seed=500)


##############################################################
# 2.  Models                                                 #
##############################################################
#1Decision trees C4.5 

cols <- c("Survived",'Pclass', 'Sex', 'Embarked')
train[cols] <- lapply(train[cols], as.factor)
titanicTree <- C5.0(Survived ~ ., data = train)
predicttree <- predict(object = titanicTree, 
                newdata = test, type = “class”)
predicttree
#2Random forests

#3Bayesian networks

#4Support vector machines

#5Neural networks

#6Logistic regression

#C4.5, CART, 朴素贝叶斯， K近邻， 支持向量， 最大期望, AdaBoost


##############################################################
# PLot part                                                  #
##############################################################
#Here we will plot the passenger survival by class
train$Survived <- factor(train$Survived, levels=c(1,0))
levels(train$Survived) <- c("Survived", "Died")
train$Pclass <- as.factor(train$Pclass)
levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

png("1_survival_by_class.png", width=800, height=600)
mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
           off=c(0), cex.axis=1.4)
dev.off()
