library(gbm)
library(randomForest)
library(rpart)
set.seed(415)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
feature_eng <- function(train_df, test_df) {
    test_df$Survived <- NA
    combi <- rbind(train_df, test_df)
    combi$Name <- as.character(combi$Name)
    combi$Title <- sapply(combi$Name, FUN = function(x) {
        strsplit(x, split = "[,.]")[[1]][2]
    })
    combi$Title <- sub(" ", "", combi$Title)
    combi$Title[combi$Title %in% c("Mme", "Mlle")] <- "Mlle"
    combi$Title[combi$Title %in% c("Capt", "Don", "Major", "Sir")] <- "Sir"
    combi$Title[combi$Title %in% c("Dona", "Lady", "the Countess", "Jonkheer")] <- "Lady"
    combi$Title <- factor(combi$Title)
    combi$FamilySize <- combi$SibSp + combi$Parch + 1
    combi$Surname <- sapply(combi$Name, FUN = function(x) {
        strsplit(x, split = "[,.]")[[1]][1]
    })
    combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep = "")
    combi$FamilyID[combi$FamilySize <= 2] <- "Small"
    combi$FamilyID <- factor(combi$FamilyID)
    Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize, data = combi[!is.na(combi$Age), ], method = "anova")
    combi$Age[is.na(combi$Age)] <- predict(Agefit, combi[is.na(combi$Age), ])
    combi$Embarked[c(62, 830)] = "S"
    combi$Embarked <- factor(combi$Embarked)
    combi$Fare[1044] <- median(combi$Fare, na.rm = TRUE)
    combi$FamilyID2 <- combi$FamilyID
    combi$FamilyID2 <- as.character(combi$FamilyID2)
    combi$FamilyID2[combi$FamilySize <= 3] <- "Small"
    combi$FamilyID2 <- factor(combi$FamilyID2)
    return(combi)
}
data <- feature_eng(train, test)
train <- data[1:891, ]
test <- data[892:1309, ]
n.trees <- 5000
gbm_fit <- gbm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID, data = train, distribution = "bernoulli", interaction.depth = 3, n.minobsinnode = 10, n.trees = n.trees, shrinkage = 0.001, train.fraction = 0.8, verbose = TRUE)
gbm.perf(gbm_fit)
predict_gbm <- predict(gbm_fit, train, n.trees = gbm.perf(gbm_fit), type = "response")
predict_gbm2 <- predict(gbm_fit, test, n.trees = gbm.perf(gbm_fit), type = "response")
proportion <- sapply(seq(0.3, 0.7, 0.01), function(step) c(step, sum(ifelse(predict_gbm < step, 0, 1) != train$Survived)))
predict_gbm_train <- ifelse(predict_gbm < proportion[, which.min(proportion[2, ])][1], 0, 1)
head(predict_gbm_train)
score <- sum(train$Survived == predict_gbm_train)/nrow(train)
score
predict_gbm_test <- ifelse(predict_gbm2 < proportion[, which.min(proportion[2, ])][1], 0, 1)
submit <- data.frame(PassengerId = test$PassengerId, Survived = predict_gbm_test)
write.csv(submit, file = "firstgbm.csv", row.names = FALSE)
