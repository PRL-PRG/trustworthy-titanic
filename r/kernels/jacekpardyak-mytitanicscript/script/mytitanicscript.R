
# This R script will run on our backend. You can write arbitrary code here!

# Many standard libraries are already installed, such as randomForest
library(randomForest)

# The train and test data is stored in the ../input directory
train <- read.csv("../input/train.csv")
test  <- read.csv("../input/test.csv")

test$Survived <- NA
train.test <- rbind(train, test)
train.test$Survived <- as.factor(train.test$Survived)

# fill missing Fare
train.test[is.na(train.test$Fare),"Fare"] <- 
  median(train.test[train.test$Pclass == 3,"Fare"], na.rm = T)

summary(train.test)
# substitude Cabin with number of cabins
train.test$Cabin <- as.character(train.test$Cabin)
train.test$Cabin <- sapply(train.test$Cabin, function(x) length(unlist(strsplit(x, split = " "))))

# bin the age every 5 years
train.test$Age <- cut(train.test$Age, pretty(train.test$Age,n = 16))

summary(train.test)

# age.data to build model predicting age
age.data <- train.test[!is.na(train.test$Age),]
# no.age.data without age which should be predicted
no.age.data <- train.test[is.na(train.test$Age),]

names(age.data)
# formula used to build the model for age
formula <- as.formula(paste("Age", 
      paste("Pclass", "Sex", "SibSp", "Parch", "Fare", "Cabin", "Embarked" , sep = "+"),
      sep = "~"))

# build model for Age
require(randomForest)
fit <- randomForest(formula = formula, data = age.data, na.action = na.omit)
no.age.data$Age <- predict(fit, newdata = no.age.data, type = "class")

# merge and split the data
data <- rbind(age.data, no.age.data)
data.train <- data[!is.na(data$Survived),]
data.test  <- data[ is.na(data$Survived),]

# formula used to build the model for Survived
formula <- as.formula(paste("Survived", 
                            paste("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Cabin", "Embarked" , sep = "+"),
                            sep = "~"))
# build model for Survived
require(randomForest)
fit <- randomForest(formula = formula, data = data.train, na.action = na.omit)
data.test$Survived <- predict(fit, newdata = data.test, type = "class")

submission <- data.frame(data.test[,c("PassengerId", "Survived")])
write.csv(submission, file="submission.csv", row.names = F)

