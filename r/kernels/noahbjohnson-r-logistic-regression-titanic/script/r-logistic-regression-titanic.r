
library(readr)
library(arm)
library(pROC)

data <- read_csv(file = "../train.csv") # Import training file with default settings
#training <- read_csv(file = "../input/train.csv") # Import training file (kaggle directory)
data <- data.frame(data)

# clean variables for machine readability
summary(data) # print summaries to decide what to do

# factoring
data$Sex <- factor(data$Sex) # Factor Sex as (male/female)
data$Embarked <- factor(data$Embarked) # Factor Embarked as (C/Q/S) C = Cherbourg, Q = Queenstown, S = Southampton
data$Pclass <- factor(data$Pclass) # Factor Pclass as (1/2/3) for passenger's cabin class
data$Survived <- factor(data$Survived) # Factor Survived as (1/0) 1 = TRUE

# remove useless variables
data$PassengerId <- NULL # same as key
data$Name <- NULL # they didn't load the lifeboats in alphabetic order
data$Cabin <- NULL # deciphering this gibberish is beyond the scope of this project
data$Ticket <- NULL

# Imputing missing values

nafrequency <- is.na(data) # check the frequency of missing values
summary(nafrequency) # print summaries to decide what to do

# impute missing Ages with regression
#agemodel <- lm(Age ~ (Pclass + Sex + SibSp + Parch + Fare + Embarked)^2,data=data)

#data$Age[is.na(data$Age)] <- agemodel$fitted.values

#median worked better
data$Age[is.na(data$Age)] <- median(na.omit(data$Age))
data$Embarked[is.na(data$Embarked)] <- 'S'

# Split off test set
## 75% of the sample size
smp_size <- floor(0.8 * nrow(data))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

training <- data[train_ind, ]
test <- data[-train_ind, ]

fit1 <- bayesglm(Survived ~ .^2,family='binomial',data=training,)

fitted.results <- predict(fit1,newdata=training)
rocmodel <- roc(training$Survived,fitted.results)
pROC::plot.roc(rocmodel,print.thres = TRUE,print.auc = TRUE,main = "ROC Curve")

test.results <- predict(fit1,newdata=test)
fitted.results.binary <- ifelse(test.results > -.5,1,0)
misClasificError <- mean(fitted.results.binary != test$Survived)
print(paste('Accuracy',1-misClasificError))






