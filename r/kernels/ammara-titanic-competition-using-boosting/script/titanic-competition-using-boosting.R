# Titanic competition script using the gradient boosting method

library(gbm)
library(randomForest)
library(rpart)

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

# Splitting back to the train and test sets
data <- feature_eng(train, test)
train <- data[1:891,]
test <- data[892:1309,]

# Gradient boosting fitting and predicting
n.trees <- 5000
gbm_fit <- gbm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data=train, distribution = "bernoulli", interaction.depth = 3, n.minobsinnode = 10, n.trees = n.trees, shrinkage = 0.001, train.fraction = 0.8, verbose = TRUE)
gbm.perf(gbm_fit) # To see the variable importance
#summary(gbm_fit)
predict_gbm <- predict(gbm_fit, train, n.trees = gbm.perf(gbm_fit), type = "response") # Predicting on the train set to check for overfitting and best cut-off since probabilities are returned
predict_gbm2 <- predict(gbm_fit, test, n.trees = gbm.perf(gbm_fit), type = "response") # Predicting on the test set

# Since gbm gives a survival probability prediction, we need to find the best cut-off on the train set:
proportion <- sapply(seq(.3,.7,.01),function(step) c(step,sum(ifelse(predict_gbm<step,0,1)!=train$Survived)))
#dim(proportion)
predict_gbm_train <- ifelse(predict_gbm < proportion[,which.min(proportion[2,])][1],0,1) # Converting probabilities into 0 or 1 according to the best cut-off
head(predict_gbm_train)
score <- sum(train$Survived == predict_gbm_train)/nrow(train)
score

# Applying the best cut-off on the test set
predict_gbm_test <- ifelse(predict_gbm2<proportion[,which.min(proportion[2,])][1],0,1)
submit <- data.frame(PassengerId = test$PassengerId, Survived = predict_gbm_test)

# Creating the submitting file
write.csv(submit, file = "firstgbm.csv", row.names = FALSE)