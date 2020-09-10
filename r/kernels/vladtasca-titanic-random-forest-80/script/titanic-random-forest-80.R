# Draft First Attempt -- Random Forest 

#Loading many unnecessary libraries

#vis
library('ggplot2') # visualization
library('ggthemes') # visualization
library('ggridges') # visualization
library('ggforce') # visualization
library('ggExtra') # visualization
library('GGally') # visualisation
library('scales') # visualization
library('grid') # visualisation
library('gridExtra') # visualisation
library('corrplot') # visualisation
library('VIM') # missing values

# wrangle
library('dplyr') # data manipulation
library('tidyr') # data manipulation
library('readr') # data input
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('modelr') # factor manipulation

# model
library('randomForest') # classification
library('xgboost') # classification
library('ROCR') # model validation
library('caret')
library('rpart.plot')
library('doSNOW')

#Load 'em babies up
train <- read_csv('../input/train.csv')
test <- read_csv('../input/test.csv')

#Combine 'em
test$Survived <- NA
combined <- rbind(train, test)
tr_idx <- seq(nrow(train)) #train indices, test indices are -tr_idx

#Fixing small error (16 y/o /w 13 y/o son)
combined$SibSp[combined$PassengerId==280] = 0
combined$Parch[combined$PassengerId==280] = 2
combined$SibSp[combined$PassengerId==1284] = 1
combined$Parch[combined$PassengerId==1284] = 1

# Who is missing?
colSums(is.na(combined))
#1xFare, 2xEmbarked, 263xAge

#####
#Replacing FARE
#####

trControl <- trainControl(method = 'repeatedcv',number = 10,repeats = 5) 
fareMiss <- which(is.na(combined$Fare)) #missing fare row

model_fare <- train(Fare ~ Pclass + Sex + Embarked + SibSp + Parch, data = combined %>% filter(!is.na(Fare)),trControl = trControl,method = 'rpart',na.action = na.pass,tuneLength = 5)

combined$Fare[fareMiss] = predict(model_fare, combined[fareMiss,]) #predict missing fare
combined$Fare <- as.factor(combined$Fare) #add fare factor column
rpart.plot(model_fare$finalModel)

#####
###Replacing EMBARKED
#####

which(is.na(combined$Embarked))
combined[62, "Embarked"] <- 'C'
combined[830, "Embarked"] <- 'C'

# New Variable: Title
combined$Title <- gsub('(.*, )|(\\..*)', '', combined$Name)
table(combined$Sex, combined$Title)

# Reassign titles
officer <- c('Capt', 'Col', 'Don', 'Dr', 'Major', 'Rev')
royalty <- c('Dona', 'Lady', 'the Countess','Sir', 'Jonkheer')

combined$Title[combined$Title == 'Mlle'] <- 'Miss' 
combined$Title[combined$Title == 'Ms'] <- 'Miss'
combined$Title[combined$Title == 'Mme'] <- 'Mrs' 
combined$Title[combined$Title %in% royalty] <- 'Royalty'
combined$Title[combined$Title %in% officer] <- 'Officer'

# New Variable: Family Size
combined$FamilySize <- combined$SibSp + combined$Parch + 1

ggplot(combined, aes(Pclass, fill=!is.na(Age))) + geom_bar(position="dodge") + labs(title="Passenger Has Age",fill="Has Age") #  We decide at this point to dismiss Age information for Pclass 3. Having to complete a large percentage of missing values may add more noise to the prediction system for a goal of better than 80% accuracy.

ggplot(combined[tr_idx,] %>% filter(Pclass!=3), aes(Age)) + geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density per Age for Pclass 1 and 2")

child <- 14
combined$Minor <- ifelse(combined$Age<child&combined$Pclass!=3, 1, 0)
combined$Minor <- ifelse(is.na(combined$Minor), 0, combined$Minor)


# Factorise and ready to model
combined$Survived <- as.factor(combined$Survived)
combined$Pclass <- as.factor(combined$Pclass)
combined$Sex <- as.factor(combined$Sex)
combined$Title <- as.factor(combined$Title)
combined$Minor <- as.factor(combined$Minor)
combined$Embarked <- as.factor(combined$Embarked)
combined$Fare <- as.double(combined$Fare)

glimpse(combined)

# Back into train and test
train <- combined[tr_idx,]
test <- combined[-tr_idx,]

# Keep wanted columns
train <- train[, c('Survived', 'Pclass', 'Sex', 'Fare', 'Embarked','Title', 'FamilySize', 'Minor')]
test <- test[, c('Survived', 'Pclass', 'Sex', 'Fare', 'Embarked','Title', 'FamilySize', 'Minor', 'PassengerId')]
#Partition
p_idx <- createDataPartition(train$Survived, p = 0.7, list = F)
p_train <- train[p_idx, ]
p_test <- train[-p_idx, ]
# Model
#cl <- makeCluster(5, type = 'SOCK')
#registerDoSNOW(cl)
glimpse(p_train)

model_rf <- randomForest(Survived ~ ., data=train, importance=TRUE, proximity=TRUE, do.trace=TRUE)
 
#stopCluster(cl)

summary(model_rf)

test$Survived <- predict(model_rf, test)

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)

write.csv(submit, 'submit.csv', row.names = F)

