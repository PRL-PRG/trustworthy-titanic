
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
#library(randomForest)
#library(Hmisc)
# The train and test data is stored in the ../input directory
#train <- read.csv("../input/train.csv")
#test  <- read.csv("../input/test.csv")

# We can inspect the train data. The results of this are printed in the log tab below
#summary(train)
#describe(train)
#str(train)
#describe(train$Cabin)
#class(train$Cabin)
#head(train)
# Here we will plot the passenger survival by class
#train$Survived <- factor(train$Survived, levels=c(1,0))
#levels(train$Survived) <- c("Survived", "Died")
#train$Pclass <- as.factor(train$Pclass)
#levels(train$Pclass) <- c("1st Class", "2nd Class", "3rd Class")

#png("1_survival_by_class.png", width=800, height=600)
#mosaicplot(train$Pclass ~ train$Survived, main="Passenger Survival by Class",
#           color=c("#8dd3c7", "#fb8072"), shade=FALSE,  xlab="", ylab="",
#          off=c(0), cex.axis=1.4)
#dev.off()


#########################################################################################################
############## Logistic Regression Model on Titanic Dataset ############################################

# Load dataset
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")
#gender <- read.csv("../input/gender_submission.csv")
Survived<-c(0,1,0,0,1,0,1,0,1,0,0,0,1,0,1,1,0,0,1,1,0,0,1,0,1,0,1,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,0,1,1,0,0,0,1,1,0,0,1,1,0,0,0,0,
0,1,0,0,0,1,0,1,1,0,0,1,1,0,1,0,1,0,0,1,0,1,0,0,0,0,0,0,1,1,1,0,1,0,1,0,0,0,1,0,1,0,1,0,0,0,1,0,0,0,0,0,0,1,1,1,1,
0,0,1,0,1,1,0,1,0,0,1,0,1,0,0,0,0,1,0,0,0,0,0,1,0,1,1,0,0,0,0,0,0,0,0,1,0,0,1,0,0,1,1,0,1,1,0,1,0,0,1,0,0,1,1,0,0,
0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,0,1,0,0,0,0,0,0,0,0,1,0,1,1,0,0,1,0,0,1,0,1,0,0,0,0,1,1,0,1,0,1,0,1,0,1,0,1,1,0,1,0,
0,0,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,1,1,0,1,0,0,0,0,1,0,1,1,1,0,
0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,1,1,0,0,0,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,
1,0,1,0,1,0,1,1,0,0,0,1,0,1,0,0,1,0,1,1,0,1,1,0,1,1,0,0,1,0,0,1,1,1,0,0,0,0,0,1,1,0,1,0,0,0,0,0,1,0,0,0,1,0,1,0,0,
1,0,1,0,0,0,0,0,1,1,1,1,1,0,1,0,0,0)

gender <- as.data.frame(Survived)
 
                    
##########################################################################################
# Recode dependent variable (survied) as dummy variable
test$Survived <- gender$Survived
test$Survived <- as.factor(test$Survived)
train$Survived <- as.factor(train$Survived)
#############################################################################################
# remove redundant variables from train and test data
train1 <- subset(train, select=c(Survived,Pclass,Sex,Age,SibSp,Parch,Embarked))
test1 <- subset(test,   select=c(Survived,Pclass,Sex,Age,SibSp,Parch,Embarked))

# create dataset to check the distribution of the predictors
dataset <- rbind(train1,test1)
##############################################################################################
# Check the structure of dataset
str(dataset)
# Pclass
hist(dataset$Pclass)
table(dataset$Pclass)
# recode Pclass as dummy variable
dataset$Pclass <- as.factor(dataset$Pclass)
plot(dataset$Pclass)
# SibSp
hist(dataset$SibSp) # Skewed distribution
table(dataset$SibSp) # frequency distribution

# Make the necessary replacements for coarse classification of SibSp variable  

dataset$SibSp_cat <- rep(NA,length(dataset$SibSp))

dataset$SibSp_cat[which(dataset$SibSp == 0)] <- "No Sibling"
dataset$SibSp_cat[which(dataset$SibSp == 1)] <- "One Sibling"
dataset$SibSp_cat[which(dataset$SibSp >= 2 & dataset$SibSp <=8)] <- "2+ Sibling"

dataset$SibSp_cat <- as.factor(dataset$SibSp_cat)
plot(dataset$SibSp_cat)

dataset <- subset(dataset,select = -c(SibSp))


# Check the structure of dataset
str(dataset)
# Parch
hist(dataset$Parch) # Skewed distribution
table(dataset$Parch) # frequency distribution

# Make the necessary replacements for coarse classification of Parch variable
dataset$Parch_cat <- rep(NA,length(dataset$Parch))

dataset$Parch_cat[which(dataset$Parch == 0)] <- "No Family"
dataset$Parch_cat[which(dataset$Parch == 1)] <- "One Family"
dataset$Parch_cat[which(dataset$Parch == 2)] <- "Two Family"
dataset$Parch_cat[which(dataset$Parch > 2 & dataset$Parch <=9)] <- "2+ Family"

dataset$Parch_cat <- as.factor(dataset$Parch_cat)
plot(dataset$Parch_cat)

dataset <- subset(dataset,select = -c(Parch))
#
str(dataset)
##############################################################################################
# check for missing and unique values (train & test data)
sapply(dataset,function(x)sum(is.na(x)))
sapply(dataset,function(x) length(unique(x)))

# Visual plot for missing values
library(Amelia)
missmap(dataset, main= "Missing values vs observed")

# Replace missing value with mean (age variable)
library(gam)
#na.fail(dataset)         # Fails if NAs are present
dataset<-na.gam.replace (dataset)        # Replace missing in age with the mean of the non-missings;
# Check
na.fail(dataset)         # Fails if NAs are present
library(Amelia)
missmap(dataset, main= "Missing values vs observed")
sapply(dataset,function(x)sum(is.na(x)))
###############################################################################################
# Splitting the data set
trainset <- dataset[1:891,]
testset <- dataset[892:1309,]
###############################################################################################
#################################################################################################
###############################################################################################
# Build the logistic regression model
log_model <- glm(Survived~.,family='binomial',data=trainset)
# Obtain significance levels using summary()
summary(log_model)

########################################################################################################
# odds ratios only                                                                                 
exp(coef(log_model))                                                                                

########################################################################################################
########################################################################################################
# Overall Model - To test the hypothesis H0: beta_0 = beta_1 = 0 we can compare our model with a    ####
# reduced model that only contains an intercept term.                                               ####
# The deviance was reduced by 407 points on 12 degrees of freedom, for a p-value of...              ####
1 - pchisq(407, df=12)                                                                              ####
# Overall, the model appears to have performed well.                                                ####
# There is significant reduction in deviance (significant difference from the null model).          #### 
#  Hence, we have relatively strong evidence in favor of rejecting H0(null hypothesis).             ####
########################################################################################################
# Obtain predictions for test dataset
predictions_test <- predict(log_model, newdata = testset, type = "response")
# Look at the range of the probabilities
range(predictions_test)
# Make a binary predictions-vector using a cut-off of 0.5
pred_cutoff <- ifelse(predictions_test>0.5,1,0)
testset <- data.frame(testset, pred_cutoff)
##############################################################################################
# Construct a confusion matrix
conf <-table(testset$Survived,pred_cutoff,dnn = c("Actual", "Predicted"))
print(conf)

# TP, FN, FP and TN using conf
TP <- conf[1, 1] # 
FN <- conf[1, 2] #  
FP <- conf[2,1] # 
TN <- conf[2,2] # 

# Calculate and print the accuracy: acc
acc = (TP+TN)/(TP+FN+FP+TN)
print(acc)

# Calculate and print out the precision: prec
prec = (TP)/(TP+FP)
print(prec)

# Calculate and print out the recall: rec
rec = (TP)/(TP+FN)
print(rec)

##############################################################################################
library(ROCR)
p <- predict(log_model, newdata = testset, type = "response")
pr <- prediction(p, testset$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
