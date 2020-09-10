# Titanic competition script using the Xgboost method

library(xgboost)
library(rpart)
library(Ckmeans.1d.dp)

set.seed(415)

train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

feature_eng <- function(train_df, test_df) {
        # Combining the train and test sets for purpose engineering
        test_df$Survived <- NA
        combi <- rbind(train_df, test_df) 
        
        #Features engineering
        combi$Name <- as.character(combi$Name)
        
        # The number of titles are reduced to reduce the noise in the data
        combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
        combi$Title <- sub(' ', '', combi$Title)
        #table(combi$Title)
        combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
        combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
        combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
        combi$Title <- factor(combi$Title)
        
        # Reuniting the families together
        combi$FamilySize <- combi$SibSp + combi$Parch + 1
        combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
        combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
        combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
        #table(combi$FamilyID)
        combi$FamilyID <- factor(combi$FamilyID)
        
        
        # Decision trees model to fill in the missing Age values
        Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data=combi[!is.na(combi$Age),], method="anova")
        combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age),])
        
        # Fill in the Embarked and Fare missing values
        #which(combi$Embarked == '')
        combi$Embarked[c(62,830)] = "S"
        combi$Embarked <- factor(combi$Embarked)
        #which(is.na(combi$Fare))
        combi$Fare[1044] <- median(combi$Fare, na.rm=TRUE)
        
        # Creating a new familyID2 variable that reduces the factor level of falilyID so that the random forest model
        # can be used
        combi$FamilyID2 <- combi$FamilyID
        combi$FamilyID2 <- as.character(combi$FamilyID2)
        combi$FamilyID2[combi$FamilySize <= 3] <- 'Small'
        combi$FamilyID2 <- factor(combi$FamilyID2)
        
        return(combi)
}

# Calling the engineering function
data <- feature_eng(train, test)

# Creating a dataframe containing only columns that interest us and converting the data into numerics in order to use xgboost
combi2 <- data[, -c(1,4,9, 11, 15,17)]
# Converting factors to numerics and making the variables start at 0 since this is a requirement of the xgboost package
combi2$Pclass <- as.numeric(combi2$Pclass)-1
combi2$Sex <- as.numeric(combi2$Sex) -1
combi2$Embarked <- as.numeric(combi2$Embarked) -1
combi2$Title <- as.numeric(combi2$Title) -1
combi2$FamilySize <- as.numeric(combi2$FamilySize) -1
combi2$FamilyID <- as.numeric(combi2$FamilyID) -1

# convert the new dataframe into a matrix 
combi2 <- as.matrix(combi2)

# Splitting back to train and test sets
train <- combi2[1:891,]
test <- combi2[892:1309,]

# Using the cross validation to estimate our error rate:
param <- list("objective" = "binary:logistic")

cv.nround <- 15
cv.nfold <- 3

xgboost_cv = xgb.cv(param=param, data = train[, -c(1)], label = train[, c(1)], nfold = cv.nfold, nrounds = cv.nround)
# [0]        train-error:0.130752+0.007009	test-error:0.178451+0.043771
# [1]	train-error:0.124018+0.005412	test-error:0.179573+0.032702
# [2]	train-error:0.120090+0.005412	test-error:0.178451+0.032119
# [3]	train-error:0.112795+0.001684	test-error:0.178451+0.032119
# [4]	train-error:0.112795+0.003367	test-error:0.177329+0.035367

# Fitting with the xgboost model
nround  = 15
fit_xgboost <- xgboost(param =param, data = train[, -c(1)], label = train[, c(1)], nrounds=nround)

# Get the feature real names
names <- dimnames(train)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = fit_xgboost)

# Plotting
xgb.plot.importance(importance_matrix)

# Prediction on test and train sets
pred_xgboost_test <- predict(fit_xgboost, test[, -c(1)])
pred_xgboost_train <- predict(fit_xgboost, train[, -c(1)])

# Since xgboost gives a survival probability prediction, we need to find the best cut-off:
proportion <- sapply(seq(.3,.7,.01),function(step) c(step,sum(ifelse(pred_xgboost_train<step,0,1)!=train[, c(1)])))
dim(proportion)
# Applying the best cut-off on the train set prediction for score checking
predict_xgboost_train <- ifelse(pred_xgboost_train<proportion[,which.min(proportion[2,])][1],0,1)
head(predict_xgboost_train)
score <- sum(train[, c(1)] == predict_xgboost_train)/nrow(train)
score

# Applying the best cut-off on the test set
predict_xgboost_test <- ifelse(pred_xgboost_test<proportion[,which.min(proportion[2,])][1],0,1)
test <- as.data.frame(test) # Conveting the matrix into a dataframe

# Creating the submitting file
submit <- data.frame(PassengerId = data[892:1309,c("PassengerId")], Survived = predict_xgboost_test)
write.csv(submit, file = "firstxgboost.csv", row.names = FALSE)
#0.76077 
