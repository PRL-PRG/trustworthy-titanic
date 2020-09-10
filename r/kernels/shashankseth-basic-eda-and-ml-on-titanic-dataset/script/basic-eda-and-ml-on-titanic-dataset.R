# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr) # Data manipulation

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

#system("ls ../input")

#### Load the titanic dataset
train <- read.csv('../input/train.csv')
test  <- read.csv('../input/test.csv')

## Check Data
str(train)
str(test)

full <- bind_rows(train,test) # Combine the train & test data set
str(full)

## Unique values per column
lapply(full, function(x) length(unique(x)))

## Boxplot of Age column to check for Outliers
boxplot(full$Age)

## Creating User function to treat outliers in Age column
remove_outliers <- function(x, na.rm = TRUE) {
qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H <- 1.5 * IQR(x, na.rm = T)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
  
## Replace the outliers in Age column with NA 
  a <- remove_outliers(full$Age)
  sum(is.na(a))
  full$Age <- a
  
## Impute the missing values in Age column with Mean value
mean_full <- mean(full$Age, na.rm = T)
full[is.na(full$Age),"Age"] <- mean_full

## Check for the Missing value for Embarked Column
   ## Passenger 62 and 830 in training data set are missing Embarkment
   full[c(62,830),'Embarked']
   full[c(62,830),'Fare']
   
   ## Exclude missing passenger Ids
   fare_emb <- full %>% filter(PassengerId != 62 & PassengerId != 830)
   
   ## GGPLOT to visualize Embarkment, Median Fare & Passenger Class
   ggplot(fare_emb, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot()
   
   ## Impute the missing embarked with 'C' since the Fare of Missing Embarked people is 80$ which is close to Median Fare value of Embarked 'C' people
      full$Embarked[c(62,830)] <- 'C'
      length(unique(full$Embarked))

## Create a new variable Family Size
full$FamSize <- full$SibSp + full$Parch + 1

## GGPLOT to visualize relation between Family Size & Survival
ggplot(full, aes(x = FamSize, fill = factor(Survived))) + geom_bar()

## Create a new variable based on Family Size
full$FamSizeD[full$FamSize == 1] <- 1   # Small
full$FamSizeD[full$FamSize > 1 & full$FamSize <= 4] <- 2  # Medium
full$FamSizeD[full$FamSize > 4] <- 3  # Large

## Convert the variables into Factors
full$Pclass <- as.factor(full$Pclass)
full$Survived <- as.factor(full$Survived)
full$Embarked <- as.factor(full$Embarked)
full$FamSizeD <- as.factor(full$FamSizeD)

## Divide the train and test data set
train_set <- full[1:891, ]
str(train_set)
test_set <- full[-c(1:891), ]
str(test_set)

## Keep 20% of the data from train_set for validation purpose
set.seed(123)
div <- sample(nrow(train_set), nrow(train_set) * 0.8)

train_set1 <- train_set[div, ]
str(train_set1)

valid_set <- train_set[-div, ]
str(valid_set)

prop.table(table(train_set1$Survived))
prop.table(table(valid_set$Survived))

## Build the Model 
   ## Logistic Regression Model
      lr_model1 <- glm(Survived ~ Pclass + Sex + Age + FamSizeD + Embarked, data = train_set1, family = binomial)
      summary(lr_model1)
      
   ## Prediction on training data set (train_set1)
      train_pred <- predict(lr_model1, newdata = train_set1, type = "response")
      table(train_set1$Survived, train_pred>0.5)
      
   ## Accuracy on training data set (train_set1)
      train_acc <- ((371+198)/(371+62+81+198)) * 100
      train_acc
  
   ## Prediction on validation data set (valid_set)
     valid_pred <- predict(lr_model1, newdata = valid_set, type = "response")
     table(valid_set$Survived, valid_pred>0.5)
   
   ## Accuracy on validaton data set (valid_set)
      valid_acc <- ((106+43)/(106+10+20+43)) * 100
      valid_acc
      
## Prediction on test data set (test_set) & write the result
      test_pred <- predict(lr_model1, newdata = test_set, type = "response")
      test_set$Survived <- as.factor(ifelse(test_pred > 0.5, 1, 0))
      table(test_set$Survived)  
      
      ## Save the output to data frame
      output <- data.frame(PassengerId = test_set$PassengerId, Survived = test_set$Survived)
      
      ## Write the output back to CSV file
      write.csv(output, file = "lr_logistic_reg.csv", row.names = F)
