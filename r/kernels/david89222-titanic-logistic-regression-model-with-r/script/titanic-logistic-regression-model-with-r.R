## import package
library(tidyverse) # metapackage with lots of helpful functions

## import data
train <- read_csv("../input/train.csv")
test <- read_csv("../input/test.csv")

## Descriptive statistics
head(train)
glimpse(train)
summary(train)

## which variable contains NAs? 
# Fix Age/Embarked variables # remove Cabin (not informative)
for (i in 1:dim(train)[2]) {
  cat(colnames(train)[i],": " ,mean(is.na(train[, i])), "%", "\n")
}

## remove Cabin
train <- train %>% select(-Cabin)

## factor # set score
train$Survived <- factor(train$Survived)
train$Pclass <- factor(train$Pclass)
train$Sex <- factor(train$Sex, labels = c(0, 1)) # female = 0; male = 1 # I'm not Sexism!

## Make Name variable more informative
name_split <- str_split(train$Name, pattern = ", ", simplify = F)
name_split <- sapply(name_split, "[", 2)
name_split <- str_split(name_split, pattern = "\\.", simplify = F)
name_split <- sapply(name_split, '[', 1)
train$Title <- name_split

## levels of Title: Mr, Miss, Mrs, Others
for (i in 1:length(train$Title)) {
  if (!train$Title[i] %in% c("Mr", "Miss", "Mrs")) {
    train$Title[i] = "Others"
  }
}

## mapping categorical variables
train$Title <- factor(train$Title, levels = c("Mr", "Miss", "Mrs", "Others"), labels = c("0", "1", "2", "3"))

## Deal with NAs in "Age" with the help of "title"
train %>%
  group_by(Title) %>%
  summarise("Age_median" = median(Age, na.rm = T))

for (i in 1:length(train$Age)) {
  if (is.na(train$Age[i])) {
    if (train$Title[i] == "0") {train$Age[i] = 30}
    else if (train$Title[i] == "1") {train$Age[i] = 21}
    else if (train$Title[i] == "2") {train$Age[i] = 35} 
    else {train$Age[i] = 9}
  }
}

## Deal with NAs in Embarked
for (i in 1:dim(train)[1]) {
  if (is.na(train$Embarked[i])) {
    train$Embarked[i] = "S"
    }
}

## Add new feature: Travel alone?
for (i in 1:dim(train)[1]) {
  if (train$SibSp[i] > 0 || train$Parch[i] > 0) {
    train$Alone[i] = 1
  } else {
    train$Alone[i] = 0
  }
}

## mapping categorical variables
# S:0; C:1; Q:2
train$Alone <- factor(train$Alone)
train$Embarked <- factor(train$Embarked, levels = c("S", "C", "Q"), labels = c("0", "1", "2"))
train$Pclass <- factor(train$Pclass, levels = c(1, 2, 3), labels = c("0", "1", "2"))


## Select useful features
train <- train %>% select(Survived, Pclass, Age, Sex, Fare, Embarked, Title, Alone)

## Add one more feature: kid
# Add one more feature
for (i in 1:dim(train)[1]) {
  if (train$Age[i] <= 16) {
    train$kid[i] = 1
  } else {
    train$kid[i] = 0
  }
}

## Build logistic regression model
logit.mod <- glm(Survived ~ Pclass + Sex + Age + Embarked + kid , data = train, family = "binomial")
summary(logit.mod)

## Qualify the model
prediction_train <- predict(logit.mod, type = "response")
tab <- table(train$Survived, prediction_train > 0.5)

TP = tab[2,2] # true positives
TN = tab[1,1] # true negatives
FP = tab[1,2] # false positives
FN = tab[2,1] # false negatives

sensitivity = TP/(TP+FN)
specificity = TN/(TN+FN)
accuracy = (TN + TP)/(TN + TP + FP + FN)
accuracy

# --------------------------------------------------------

## transformation of testing data 

test <- test %>% select(-Cabin)

test$Pclass <- factor(test$Pclass)
test$Sex <- factor(test$Sex, labels = c(0, 1)) 

name_split <- str_split(test$Name, pattern = ", ", simplify = F)
name_split <- sapply(name_split, "[", 2)
name_split <- str_split(name_split, pattern = "\\.", simplify = F)
name_split <- sapply(name_split, '[', 1)
test$Title <- name_split

for (i in 1:length(test$Title)) {
  if (!test$Title[i] %in% c("Mr", "Miss", "Mrs")) {
    test$Title[i] = "Others"
  }
}

test$Title <- factor(test$Title, levels = c("Mr", "Miss", "Mrs", "Others"), labels = c("0", "1", "2", "3"))

for (i in 1:length(test$Age)) {
  if (is.na(test$Age[i])) {
    if (test$Title[i] == "0") {test$Age[i] = 30}
    else if (test$Title[i] == "1") {test$Age[i] = 21}
    else if (test$Title[i] == "2") {test$Age[i] = 35} 
    else {test$Age[i] = 9}
  }
}

for (i in 1:dim(test)[1]) {
  if (is.na(test$Embarked[i])) {
    test$Embarked[i] = "S"
    }
}

for (i in 1:dim(test)[1]) {
  if (test$SibSp[i] > 0 || test$Parch[i] > 0) {
    test$Alone[i] = 1
  } else {
    test$Alone[i] = 0
  }
}

test$Alone <- factor(test$Alone)
test$Embarked <- factor(test$Embarked, levels = c("S", "C", "Q"), labels = c("0", "1", "2"))
test$Pclass <- factor(test$Pclass, levels = c(1, 2, 3), labels = c("0", "1", "2"))


for (i in 1:dim(test)[1]) {
  if (test$Age[i] <= 16) {
    test$kid[i] = 1
  } else {
    test$kid[i] = 0
  }
}

test <- test %>% select(PassengerId, Pclass, Age, Sex, Fare, Embarked, Title, Alone, kid)


# -----------------------------------------------

# prediction

## prediction in testing data
prediction <- predict(logit.mod, newdata=test, type = "response")
solution <- data.frame(PassengerID = test$PassengerId, Survived = round(prediction, 0))
write.csv(solution, file = '../logitmod.csv', row.names = F)





