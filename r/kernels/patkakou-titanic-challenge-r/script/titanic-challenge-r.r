
df <- read.csv('../input/train-data/train_data.csv', header=T,na.strings=c(''))
df_test <- read.csv('../input/data-test/test_data.csv', header=T,na.strings=c(''))

# Let Change the "Pclass" and "Survived" attributes to factors.
df$Pclass <- as.factor(df$Pclass)
df$Survived <- as.factor(df$Survived)
str(df) #to check the type of Pclass & Survived

df_test$Pclass <- as.factor(df_test$Pclass)

# I will now check for missing values and look how many unique values there are for each variable 
paste('number of missing values for each variable')
sapply(df,function(x) sum(is.na(x)))
sapply(df_test,function(x) sum(is.na(x)))
       
paste('number of unique values for each variable')
sapply(df, function(x) length(unique(x)))
sapply(df_test, function(x) length(unique(x)))

library(Amelia) # let plot our dataset and highlight missing values
par(mfrow = c(2,1))
missmap(df, col = c('yellow','darkgreen'), main = 'Missing values vs observed')
missmap(df_test, col = c('yellow','darkgreen'), main = 'Missing values vs observed')

#extract the first letter of the "Cabin" variable
#install.packages("stringr", repos='http://cran.us.r-project.org')
library('stringr')
df$Cabin <- as.factor(str_sub(df$Cabin, 1, 1))
df_test$Cabin <- as.factor(str_sub(df_test$Cabin, 1, 1))

# Missing values in "Cabin" attribute
levels(df$Cabin) <- c(levels(df$Cabin),'U')
df$Cabin[which(is.na(df$Cabin))] <- 'U'

levels(df_test$Cabin) <- c(levels(df$Cabin),'T', 'U')
df_test$Cabin[which(is.na(df_test$Cabin))] <- 'U'



#5 titles contained missing value Mr, Mrs, Miss, Dr & Master
#assigning the mean age value for each title not containing a missing value

mean_mr <- mean(df$Age[grepl(' Mr\\.',df$Name) & !is.na(df$Age)])
df$Age[grepl(' Mr\\.',df$Name) & is.na(df$Age)] <- mean_mr

mean_mrs <- mean(df$Age[grepl(' Mrs\\.',df$Name) & !is.na(df$Age)])
df$Age[grepl(' Mrs\\.',df$Name) & is.na(df$Age)] <- mean_mrs

mean_miss <- mean(df$Age[grepl(' Miss\\.',df$Name) & !is.na(df$Age)])
df$Age[grepl(' Miss\\.',df$Name) & is.na(df$Age)] <- mean_miss

mean_dr <- mean(df$Age[grepl(' Dr\\.',df$Name) & !is.na(df$Age)])
df$Age[grepl(' Dr\\.',df$Name) & is.na(df$Age)] <- mean_dr

mean_master <- mean(df$Age[grepl(' Master\\.',df$Name) & !is.na(df$Age)])
df$Age[grepl(' Master\\.',df$Name) & is.na(df$Age)] <- mean_master

# Assigning the mode of 'Embarked' 
df$Embarked[which(is.na(df$Embarked))] <- 'S'
#contrasts(df$Embarked)For a better understanding of how R is going to deal with the categorical variables, 
#This function will show us how the variables have been dummyfied by R and how to interpret them in a model.

# Test dataset
#5 titles contained missing value Mr, Mrs, Miss, Dr & Master
#assigning the mean age value for each title not containing a missing value

mean_mr <- mean(df_test$Age[grepl(' Mr\\.',df_test$Name) & !is.na(df_test$Age)])
df_test$Age[grepl(' Mr\\.',df_test$Name) & is.na(df_test$Age)] <- mean_mr

mean_mrs <- mean(df_test$Age[grepl(' Mrs\\.',df_test$Name) & !is.na(df_test$Age)])
df_test$Age[grepl(' Mrs\\.',df_test$Name) & is.na(df_test$Age)] <- mean_mrs

mean_miss <- mean(df_test$Age[grepl(' Miss\\.',df_test$Name) & !is.na(df_test$Age)])
df_test$Age[grepl(' Miss\\.',df_test$Name) & is.na(df_test$Age)] <- mean_miss

mean_ms <- mean(df_test$Age[grepl(' Ms\\.',df_test$Name) & !is.na(df_test$Age)])
df_test$Age[grepl(' Ms\\.',df_test$Name) & is.na(df_test$Age)] <- mean_dr

mean_master <- mean(df_test$Age[grepl(' Master\\.',df_test$Name) & !is.na(df_test$Age)])
df_test$Age[grepl(' Master\\.',df_test$Name) & is.na(df_test$Age)] <- mean_master

df_test$Fare[which(is.na(df_test$Fare))] <- median(!is.na(df_test$Fare))

#Let create a new column "tilte"
df$Title <- c(rep(NA, nrow(df)))

df$Title[grep(' Mr\\.',df$Name)] <- 'Mr'
df$Title[grep(' Mrs\\.',df$Name)] <- 'Mrs'
df$Title[grep(' Miss\\.',df$Name)] <- 'Miss'
df$Title[grep(' Ms\\.',df$Name)] <- 'Mrs'
df$Title[grep(' Mme\\.',df$Name)] <- 'Mrs'
df$Title[grep(' Rev\\.',df$Name)] <- 'Officer'
df$Title[grep(' Master\\.',df$Name)] <- 'Master'
df$Title[grep(' Mlle\\.',df$Name)] <- 'Miss'
df$Title[grep(' Dr\\.',df$Name)] <- 'Officer'
df$Title[grep(' Don\\.',df$Name)] <- 'Royalty'
df$Title[grep(' Dona\\.',df$Name)] <- 'Royalty'
df$Title[grep(' Lady\\.',df$Name)] <- 'Royalty'
df$Title[grep(' Col\\.',df$Name)] <- 'Officer'
df$Title[grep(' Capt\\.',df$Name)] <- 'Officer'
df$Title[grep(' Major\\.',df$Name)] <- 'Officer'
df$Title[grep(' the Countess\\.',df$Name)] <- 'Royalty'
df$Title[grep(' Jonkheer\\.',df$Name)] <- 'Royalty'
df$Title[grep(' Sir\\.',df$Name)] <- 'Royalty'


df$Title <- as.factor(df$Title)
plot(df$Title)

df_test$Title <- c(rep(NA, nrow(df_test)))

df_test$Title[grep(' Mr\\.',df_test$Name)] <- 'Mr'
df_test$Title[grep(' Mrs\\.',df_test$Name)] <- 'Mrs'
df_test$Title[grep(' Miss\\.',df_test$Name)] <- 'Miss'
df_test$Title[grep(' Ms\\.',df_test$Name)] <- 'Mrs'
df_test$Title[grep(' Mme\\.',df_test$Name)] <- 'Mrs'
df_test$Title[grep(' Rev\\.',df_test$Name)] <- 'Officer'
df_test$Title[grep(' Master\\.',df_test$Name)] <- 'Master'
df_test$Title[grep(' Mlle\\.',df_test$Name)] <- 'Miss'
df_test$Title[grep(' Dr\\.',df_test$Name)] <- 'Officer'
df_test$Title[grep(' Don\\.',df_test$Name)] <- 'Royalty'
df_test$Title[grep(' Dona\\.',df_test$Name)] <- 'Royalty'
df_test$Title[grep(' Lady\\.',df_test$Name)] <- 'Royalty'
df_test$Title[grep(' Col\\.',df_test$Name)] <- 'Officer'
df_test$Title[grep(' Capt\\.',df_test$Name)] <- 'Officer'
df_test$Title[grep(' Major\\.',df_test$Name)] <- 'Officer'
df_test$Title[grep(' the Countess\\.',df_test$Name)] <- 'Royalty'
df_test$Title[grep(' Jonkheer\\.',df_test$Name)] <- 'Royalty'
df_test$Title[grep(' Sir\\.',df_test$Name)] <- 'Royalty'

df_test$Title <- as.factor(df_test$Title)
plot(df_test$Title)

df['FamSize'] = df['SibSp'] + df['Parch'] + 1
df_test['FamSize'] = df_test['SibSp'] + df_test['Parch'] + 1



# Using the subset() function to subset the original dataset selecting the relevant columns only.
# I removed the following columns: PassengerId, Name, Ticket, Cabin
data <- subset(df,select=c(2,3,5,6,9,10, 11, 12, 13,14))
data_test <- subset(df_test,select=c(2,4,5,8,9,10,11,12,13))

# Let's split the data dataset into dataing and test set.
set.seed(100000)
data_index <- sample(nrow(data), floor(nrow(data)*0.7))
train.set <- data[data_index,]
test.set <- data[-data_index,]

# Logistic Regression Model
log_r <- glm(Survived~., data = train.set, family = binomial(link='logit'))
summary(log_r)

# Performance Evaluation: Confusion Matrix
predicted <- predict(log_r, test.set[-1], type="response")
predicted_V1 <- ifelse(predicted>=0.55, 1, 0)
confusionMatrix_V1 <- table(actual = test.set$Survived, predicted = predicted_V1)
confusionMatrix_V1
accuracy <- sum(diag(confusionMatrix_V1))/nrow(test.set)
print(paste('Accuracy =',accuracy))

#p <- predict(log_r, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
#install.packages("ROCR")
library(ROCR)
pr <- prediction(predicted, test.set$Survived)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
print(paste('AUC =',auc))


survived_test <- predict(log_r,newdata= data_test,type = 'response')
survived <- ifelse(survived_test>=0.55, 1, 0)
submission = data.frame(cbind(df_test['PassengerId'], Survived = survived))
write.csv(submission,'my_solution_LogReg.csv', row.names = FALSE)
