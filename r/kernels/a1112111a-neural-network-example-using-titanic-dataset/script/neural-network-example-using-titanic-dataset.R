library(dplyr)
library(neuralnet)
library(randomForest)
full <- read.csv("../input/train.csv", stringsAsFactors = F)
full2 <- read.csv("../input/test.csv", stringsAsFactors = F)
sapply(full, function(x) sum(is.na(x)))
sapply(full2, function(x) sum(is.na(x)))
variables <- c("Survived", "Pclass", "Sex", "Age", "Embarked", "Fare", "Parch", "SibSp")
full <- full[variables]
variables2 <- c("Pclass", "Sex", "Age", "Embarked", "Fare", "Parch", "SibSp")
full2 <- full2[variables2]
sapply(full, function(x) sum(is.na(x)))
sapply(full2, function(x) sum(is.na(x)))
full$Age[is.na(full$Age)] <- mean(full$Age, na.rm = T)
full$Fare[is.na(full$Fare)] <- mean(full$Fare, na.rm = T)
full2$Age[is.na(full2$Age)] <- mean(full2$Age, na.rm = T)
full2$Fare[is.na(full2$Fare)] <- mean(full2$Fare, na.rm = T)
count(full, Embarked)
count(full2, Embarked)
full$Embarked[is.na(full$Embarked)] <- "S"
full2$Embarked[is.na(full2$Embarked)] <- "S"
sapply(full, function(x) sum(is.na(x)))
sapply(full2, function(x) sum(is.na(x)))
full$Embarked <- as.factor(full$Embarked)
full$Sex <- as.factor(full$Sex)
full2$Embarked <- as.factor(full2$Embarked)
full2$Sex <- as.factor(full2$Sex)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + Embarked + Fare + Parch + SibSp, data = full)
importance <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[, "MeanDecreaseGini"], 2))
chosen_variables <- c("Survived", "Pclass", "Sex", "Age", "Fare", "SibSp")
full_prepped <- full[chosen_variables]
chosen_variables2 <- c("Pclass", "Sex", "Age", "Fare", "SibSp")
full_prepped2 <- full2[chosen_variables2]
train <- full_prepped[1:800, ]
test <- full_prepped[801:891, ]
m <- model.matrix(~Survived + Pclass + Sex + Age + Fare + SibSp, data = full_prepped)
nn <- neuralnet(Survived ~ Pclass + Sexmale + Age + Fare + SibSp, data = m, hidden = 2, threshold = 0.01, linear.output = F)
m1 <- model.matrix(~+Pclass + Sex + Age + Fare + SibSp, data = full_prepped2)
res <- neuralnet::compute(nn, m1[, c("Pclass", "Sexmale", "Age", "Fare", "SibSp")])
pred_train = round(res$net.result)
print(pred_train)
