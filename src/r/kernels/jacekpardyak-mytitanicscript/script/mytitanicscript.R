library(randomForest)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
test$Survived <- NA
train.test <- rbind(train, test)
train.test$Survived <- as.factor(train.test$Survived)
train.test[is.na(train.test$Fare), "Fare"] <- median(train.test[train.test$Pclass == 3, "Fare"], na.rm = T)
summary(train.test)
train.test$Cabin <- as.character(train.test$Cabin)
train.test$Cabin <- sapply(train.test$Cabin, function(x) length(unlist(strsplit(x, split = " "))))
train.test$Age <- cut(train.test$Age, pretty(train.test$Age, n = 16))
summary(train.test)
age.data <- train.test[!is.na(train.test$Age), ]
no.age.data <- train.test[is.na(train.test$Age), ]
names(age.data)
formula <- as.formula(paste("Age", paste("Pclass", "Sex", "SibSp", "Parch", "Fare", "Cabin", "Embarked", sep = "+"), sep = "~"))
require(randomForest)
fit <- randomForest(formula = formula, data = age.data, na.action = na.omit)
no.age.data$Age <- predict(fit, newdata = no.age.data, type = "class")
data <- rbind(age.data, no.age.data)
data.train <- data[!is.na(data$Survived), ]
data.test <- data[is.na(data$Survived), ]
formula <- as.formula(paste("Survived", paste("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Cabin", "Embarked", sep = "+"), sep = "~"))
require(randomForest)
fit <- randomForest(formula = formula, data = data.train, na.action = na.omit)
data.test$Survived <- predict(fit, newdata = data.test, type = "class")
submission <- data.frame(data.test[, c("PassengerId", "Survived")])
write.csv(submission, file = "submission.csv", row.names = F)
