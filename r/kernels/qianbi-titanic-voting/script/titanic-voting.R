library(dplyr)
library(mice)
library(nnet)
library(randomForest)
library(rpart)
library(e1071)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE,
                  na.strings = "")
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE,
                 na.strings = "")
full <- bind_rows(train, test)
full$Title <- gsub("^.*, |\\. .*$", "", full$Name)
Mr <- c("Col", "Don", "Jonkheer", "Sir")
Mrs <- c("Dona", "Lady", "Mme", "the Countess")
Miss <- c("Mlle", "Ms")
full$Title[full$Title %in% Mr] <- "Mr"
full$Title[full$Title %in% Mrs] <- "Mrs"
full$Title[full$Title %in% Miss] <- "Miss"
full$Fsize <- full$SibSp + full$Parch + 1
full$FsizeD[full$Fsize == 1] <- "Singleton"
full$FsizeD[full$Fsize >1 & full$Fsize < 5] <- "Small"
full$FsizeD[full$Fsize > 4] <- "Large"
factor_var <- c("Survived", "Pclass", "Sex", "Embarked",
                "Title", "FsizeD")
full[factor_var] <- lapply(full[factor_var], factor)
res <- data.frame(PassengerId = test[, 1], Survived = 0)

for (i in 1 : 5){
  full_mice <- full
  mice_mod <- mice(full_mice[, c(3, 5 : 8, 10, 12)], method = "rf")
  mice_output <- complete(mice_mod)
  full_mice[, c(6, 10, 12)] <- mice_output[, c(3, 6, 7)]
  full_mice$Embarked <- factor(full_mice$Embarked)
  full_mice$Child <- factor(ifelse(full_mice$Age <= 12, 1, 0))
  full_mice <- full_mice[, -c(9, 11)]
  train <- full_mice[1 : nrow(train), ]
  test <- full_mice[(nrow(train) + 1) : nrow(full_mice), -2]
  #logistic
  logistic_mod <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
               Embarked + Title + FsizeD + Child, family = binomial, data = train)
  res$Survived <- res$Survived + round(predict(logistic_mod, test, type = "response"), 0)
  #nnet
  nn_mod <- nnet(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                   Embarked + Title + FsizeD + Child, data = train, size = 10, decay = 0.01)
  res$Survived <- res$Survived + as.numeric(predict(nn_mod, test, type = "class"))
  #randomForest
  rf_mod <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                           Embarked + Title + FsizeD + Child, data = train)
  res$Survived <- res$Survived + as.numeric(predict(rf_mod, test))
  #SVM
  svm_mod <- svm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                   Embarked + Title + FsizeD + Child, data = train)
  res$Survived <- res$Survived + as.numeric(predict(svm_mod, test))
  #rpart
  rpart_mod <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                 Embarked + Title + FsizeD + Child, data = train, method = "class")
  rpart_predict <- predict(rpart_mod, newdata = test)
  res$Survived <- res$Survived + ifelse(rpart_predict[, 1] < rpart_predict[, 2], 1, 0)
}

res$Survived <- ifelse(res$Survived > 12, 1, 0)
write.csv(res, file = "Titanic_voting.csv", row.names = FALSE)
