## ----message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Loading the necessary libraries
library(ggplot2)
library(lattice)
library(caret)
library(ranger)
library(dplyr)
library(e1071)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Reading the train and the test datasets.
train <- read.csv("../input/train.csv",stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Checking the structure of the dataset
str(train)

## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Examining the summary of the dataset
summary(train)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Converting Survived to a factor 
train$Survived <- factor(train$Survived)

#Converting Pclass to a factor
train$Pclass <- factor(train$Pclass)

#Converting Sex to a factor
train$Sex  <- factor(train$Sex)

#Converting SibSp to a factor
train$SibSp <- factor(train$SibSp)

#Converting Parch to a factor
train$Parch <- factor(train$Parch)

#Converting Embarked to a factor
train$Embarked <- factor(train$Embarked, ordered = FALSE)



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(train$Name)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
convert_name <- function(name) {
  
  if (grepl("\\(.*\\)", name)) {           # women: take name from inside parentheses
    gsub("^.*\\((.*)\\)$", "\\1", name)
  } else {                                # men: take name before comma and after title
    gsub("^(.*),\\s[a-zA-Z\\.]*\\s(.*)$", "\\2 \\1", name)
  }
}
#grepl() searches for pattern and is gives a logical result. 
#gsub(pattern, replacement, string) is used to replace every occurence of pattern in the string with the replacement.  

###The pattern is as :
#  * ^        denotes starting of pattern
#  * .*       denotes occurence of any character zero or more times.
#  * \\(      denotes that we are actually looking for '(' in the string. Names of females of the dataset are inside paranthesis.
#  * (.*)     denotes a back-reference.
#  * \\)      denotes we actually want to look for ) in the string.
#  * \\1      denotes first back-reference. For every occurence of (.*), there is a back-reference 
#  * \\s      matches a space
# * [a-zA-z] This sprecifies character ranges. All characters in a-z and A-Z are matched.
#  * $        denotes end of string.


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
pass_names <- train$Name

clean_pass_names <- vapply(pass_names, FUN = convert_name,
                           FUN.VALUE = character(1), USE.NAMES = FALSE)

train$Name <-  clean_pass_names

#The function is applied to pass_names (The vector that contains all the names of the train dataset) via vapply so as to use convert_names for all names in the dataset.

head(train$Name)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>% 
    ggplot(aes(x = Pclass, fill = Survived)) + 
          geom_bar()


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tab <- table(train$Pclass, train$Survived)
prop.table(tab,1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>%
      ggplot(aes(x = Sex, fill = Survived)) + 
            geom_bar(stat = "count", position = "fill")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tab <- table(train$Sex, train$Survived)
prop.table(tab,1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>%
      ggplot(aes(x = Age, fill = Survived)) + 
            geom_histogram()



## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train %>%
  filter(Embarked %in% c("S","C","Q")) %>%
  ggplot() +
  geom_bar(aes(Embarked, fill = Pclass), position = "dodge") +
  facet_grid(~ Survived)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tab <- table(train$Embarked, train$Survived)
prop.table(tab,1)


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sum(is.na(train$Age))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#An example using median imputation
#train(x,y, preProcess = "medianImpute")


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Choosing independent columns 
x <- train[,c("Age","Pclass","Sex","Embarked")]
#Choosing the dependent column
y <- train$Survived


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Set a random seed
set.seed(123)

#the method "ranger" here is a fast alternative of randomForest.
#trainControl is used to define cross-validation.
model<- train(x = x,y = y,preProcess = "medianImpute", method = "ranger", trControl = trainControl(method = "cv", number = 10))


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

model


## --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Predict using the test set
prediction <- predict(model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rfSolution.csv', row.names = F)

