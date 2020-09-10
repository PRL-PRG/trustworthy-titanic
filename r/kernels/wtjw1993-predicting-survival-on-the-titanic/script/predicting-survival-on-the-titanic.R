# script for predicting survival on the Titanic
# data processing and feature engineering was forked from Megan Risdal's Exploring Survival on the Titanic R kernel (https://www.kaggle.com/mrisdal/exploring-survival-on-the-titanic?scriptVersionId=924638)
# a variety of classifiers are attempted, including logistic regression, support vector machine, and random forest
# to-do:
# (1) random forest parameter tuning 
# (2) try bagging, boosting and radial kernel svm

# Load packages
library('dplyr')
library('mice')
library('randomForest')
library('e1071') # for support vector classifier

# Load data
train <- read.csv('../input/train.csv', stringsAsFactors = F)
test  <- read.csv('../input/test.csv', stringsAsFactors = F)
full  <- bind_rows(train, test) # bind training & test data

# Grab title from passenger names
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# The first character is the deck. For example:
strsplit(full$Cabin[2], NULL)[[1]]

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))

## Sensible value imputation

# Passengers 62 and 830 are missing Embarkment
full[c(62, 830), 'Embarked']

# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)

# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62, 830)] <- 'C'

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(42)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Replace Age variable from the mice model
full$Age <- mice_output$Age

# Create the column child, and indicate whether child or adult
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Finish by factorizing our two new factor variables
full$Child  <- factor(full$Child)
md.pattern(full)

# Split the data back into a train set and a test set
full$Survived <- as.factor(full$Survived)
train <- full[1:891,]
test <- full[892:1309,]
train$Deck <- NULL
test$Deck <- NULL
test$Survived <- NULL


## Classifier Predictions

# Logistic regression predictions (public score 0.77990)
logit_model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, family = binomial)
logit_probabilities <- predict(logit_model, test, type = "response")
logit_prediction <- data.frame(PassengerID = test$PassengerId, Survived = ifelse(logit_probabilities > 0.5, 1, 0))
write.csv(logit_prediction, file = 'logit_prediction.csv', row.names = FALSE)

# Support vector classifier w/ linear kernel & 10-fold CV (public score 0.78947)
svc_cost <- 10^seq(-2,2,1) # values of cost to be evaluated using CV
svc_linear_cv <- rep(0, length(svc_cost))
# 10-fold CV to determine optimal cost
for (i in 1:length(svc_cost)) {
  set.seed(2017)
  svc_linear <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, kernel = 'linear', cost = svc_cost[i], probability = TRUE, cross = 10)
  svc_linear_cv[i] <- svc_linear$tot.accuracy
}
# retrain svc using optimal cost from CV
svc_linear <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, kernel = 'linear', cost = svc_cost[which.max(svc_linear_cv)], probability = TRUE, cross = 10)
svc_linear_prob <- predict(svc_linear, test, probability = TRUE)
svc_linear_prob <- attr(svc_linear_prob,"probabilities")[,2]
svc_linear_pred <- data.frame(PassengerID = test$PassengerId, Survived = ifelse(svc_linear_prob > 0.5, 1, 0))
write.csv(svc_linear_pred, file = 'svc_linear_pred.csv', row.names = FALSE)

# Support vector classifier w/ polynomial kernel (public score 0.79425)
svc_poly_cv <- rep(0,9)
# 10-fold cross validation to determine optimal degree (varying between 2 and 9)
for (deg in 2:9) {
  set.seed(2017)
  svc_poly <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, kernel = 'polynomial', degree = deg, probability = TRUE, cross = 10)
  svc_poly_cv[deg] <- svc_poly$tot.accuracy
}
svc_poly_degree <- which.max(svc_poly_cv)
# 10-fold cross-validation to determine optimal cost
svc_poly_cv <- rep(0, length(svc_cost))
for (i in 1:length(svc_poly_cv)) {
  set.seed(2017)
  svc_poly <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, kernel = 'polynomial', degree = svc_poly_degree, cost = svc_cost[i], probability = TRUE, cross = 10)
  svc_poly_cv[i] <- svc_poly$tot.accuracy
}
# retrain svc using optimal cost and degree
svc_poly <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train, kernel = 'polynomial', degree = svc_poly_degree, cost = svc_cost[which.max(svc_poly_cv)], probability = TRUE, cross = 10)
svc_poly_prob <- predict(svc_poly, test, probability = TRUE)
svc_poly_prob <- attr(svc_poly_prob,"probabilities")[,2]
svc_poly_pred <- data.frame(PassengerID = test$PassengerId, Survived = ifelse(svc_poly_prob > 0.5, 1, 0))
write.csv(svc_poly_pred, file = 'svc_poly_pred.csv', row.names = FALSE)

# Random forest predictions (public score 0.79425)
set.seed(2017)
rf_model <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FsizeD + Child, data = train)
rf_probabilities <- predict(rf_model, test, type = "prob")[,2]
rf_prediction <- data.frame(PassengerID = test$PassengerId, Survived = ifelse(rf_probabilities > 0.5, 1, 0))
write.csv(rf_prediction, file = 'rf_prediction.csv', row.names = FALSE)