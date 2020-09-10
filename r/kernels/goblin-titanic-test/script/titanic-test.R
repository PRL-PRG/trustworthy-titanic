library(rpart)

train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

medianFare <- median(train$Fare, na.rm = TRUE)

addTitle <- function(data) {
  data$Title <- ""
  data$Title[regexpr('Mme\\.|Mlle\\.', data$Name) > -1] <- "Mlle"
  data$Title[regexpr('Capt\\.|Don\\.|Major\\.|Sir\\.', data$Name) > -1] <- "Sir"
  data$Title[regexpr('Dona\\.|Lady\\.|Mrs\\.', data$Name) > -1] <- "Lady"
  data$Title[regexpr('Miss\\.', data$Name) > -1] <- "Miss"
  data$Title <- as.factor(data$Title)
  
  return(data)
}

train <- addTitle(train)
test <- addTitle(test)

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title,
                       data = train[!is.na(train$Age),], method = "anova")

addFeatures <- function(data) {
  data$Embarked[data$Embarked==""] <- "S"
  
  data$Fare[is.na(data$Fare)] <- medianFare
  
  data$Age[is.na(data$Age)] <- predict(predicted_age, data[is.na(data$Age),])
  
  data$Sex      <- as.factor(data$Sex)
  data$Embarked <- as.factor(data$Embarked)
  
  return(data)
}

my_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title, data = addFeatures(train), method = "class")

my_prediction <- predict(my_tree, newdata = addFeatures(test), type = "class")

my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)
