
# Import needed libraries
library(dplyr)
library(plyr)
library(naniar)
library(corrplot)

# Import Data
train.df <- read.csv('../input/train.csv',na.strings=c(""))
test.df <- read.csv('../input/test.csv',na.strings=c(""))

test_actual.df<- read.csv('../input/gender_submission.csv',na.strings=c(""))
test.df$Survived <- test_actual.df$Survived

full <- rbind(train.df, test.df)
head(full)

# Data Exploration
summary(full)

## Unbalance rate of 'Survived'
mean(full$Survived)

## Count of 'Pclass'
count(full$Pclass)

## Missing value of 'Cabin'
count(is.na(full$Cabin))

## plots
boxplot(Age~Sex, data = full)
boxplot(Age~Embarked, data = full)
plot(Fare~Age, data = full)

# Data Preprocessing
## Delete 'Cabin' column
full <- subset(full, select = -11)

## Add 'Family' column
full$Family <- full$SibSp + full$Parch + 1

## Fill in Age Missing value
### fill in missing Age by average Age by Sex
female_average <- aggregate(Age~Sex, full, mean)$Age[1]
male_average <- aggregate(Age~Sex, full, mean)$Age[2]
full[is.na(full$Age), 'Age'] <- ifelse(full[is.na(full$Age), 'Sex'] == 'female', female_average, male_average)
### fill in missing Fare by average Fare
full[is.na(full$Fare), 'Fare'] <- summary(full$Fare)['Mean']
### fill in missing Embarked by majority Embarked
full[is.na(full$Embarked), 'Embarked'] <- 'S'

## Turn categorical variables into factors
col_names <- c('Pclass', 'Sex', 'Embarked', 'Survived')
full[col_names] <- lapply(full[col_names] , factor)

## Partition Data
train <- full[1:891,]
test <- full[892:1309,]

# Modeling
## Random Forest
library(randomForest)
rf_model <- randomForest(Survived ~ Pclass + Sex + Age + Family + Fare + Embarked, data = train)

## Logistic Regression
lg_model <- glm(Survived ~ Pclass + Sex + Age + Family + Fare + Embarked, data = train, family = "binomial")

# Evaluation
## Predictions
rf_pred <- predict(rf_model, subset(test, select = -c(1,2,4,9)),type="prob")
lg_pred <- predict(lg_model, subset(test, select = -c(1,2,4,9)), type = "response")
ensemble_pred <- (rf_pred[,2] + lg_pred)/2

## Accuracy
library(caret)
print('Benchmark:')
confusionMatrix(factor(rep('0', nrow(test)), levels = c('0','1')), test$Survived)$overall['Accuracy']
print('RF:')
confusionMatrix(factor(ifelse(rf_pred[,2] > 0.5, '1', '0')), factor(test$Survived))$overall['Accuracy']
print('LG:')
confusionMatrix(factor(ifelse(lg_pred > 0.5, '1', '0')), factor(test$Survived))$overall['Accuracy']
print('Enssemble:')
confusionMatrix(factor(ifelse(ensemble_pred > 0.5, '1', '0')), factor(test$Survived))$overall['Accuracy']

importance(rf_model)
summary(lg_model)

# Submission
submission <- test_actual.df
submission$Survived <- factor(ifelse(ensemble_pred > 0.5, '1', '0'))
write.csv(submission, 'submission.csv', row.names=FALSE)
