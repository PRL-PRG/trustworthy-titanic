library("stringr")
library("rpart")
library("ggplot2")
library("dplyr")
library("randomForest")
library("caret")
library("purrr")
library("mice")
library("tidyr")
train_raw <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test_raw <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
test_raw$Survived <- NA
all <- rbind(train_raw, test_raw)
all[all == ""] <- NA
all$Pclass <- as.factor(all$Pclass)
all$Sex <- as.factor(all$Sex)
Family_size <- all$SibSp + all$Parch + 1
table(Family_size)
all$Family[Family_size == 1] <- "Single"
all$Family[Family_size > 1 & Family_size <= 4] <- "Small"
all$Family[Family_size >= 5] <- "Big"
all$Title <- gsub(pattern = ".*,\\s|\\..*", "", all$Name)
names(table(all$Title)[table(all$Title) < 20])
all$Title[all$Title %in% c("Capt", "Don", "Major", "Sir", "Col", "Jonkheer", "Rev", "Dr")] <- "Mr"
all$Title[all$Title %in% c("Mlle", "Mme", "Ms")] <- "Miss"
all$Title[all$Title %in% c("the Countess", "Lady", "Dona")] <- "Mrs"
fac_var <- c("PassengerId", "Name", "Ticket", "Cabin", "Embarked", "Title", "Family", "Survived")
all[, fac_var] <- all[, fac_var] %>% map_df(as.factor)
apply(all, 2, function(x) {
    sum(is.na(x))
})
all_rf <- all[, -c(1, 2, 4, 7, 8, 9, 11)] %>% mice(method = "rf") %>% mice::complete()
all_cart <- all[, -c(1, 2, 4, 7, 8, 9, 11)] %>% mice(method = "cart") %>% mice::complete()
age_orig <- all$Age
age_rf <- all_rf$Age
age_cart <- all_cart$Age
age1 <- as.data.frame(cbind(age_orig, age_cart, age_rf))
names(age1) <- c("Original", "CART", "RF")
age <- gather(age1, key = origin, value = age, Original, CART, RF)
ggplot(age, aes(x = age, y = ..density.., fill = origin)) + stat_bin(alpha = 0.5, position = position_dodge(1))
all[, c("Age", "Fare", "Embarked")] <- all_rf[, c("Age", "Fare", "Embarked")]
train <- all[1:nrow(train_raw), ]
test <- all[-(1:nrow(train_raw)), ]
set.seed(16)
control_rf <- trainControl(method = "cv", number = 10, verboseIter = TRUE, savePredictions = "final")
model_rf <- train(Survived ~ Pclass + Sex + Age + Family + Embarked + Title, train, method = "rf", trControl = control_rf)
train_rf_pred <- model_rf$pred[, c("pred", "obs")]
class_assess <- function(x) {
    pred_table <- table(x$obs, x$pred)
    tp <- pred_table[1, 1]
    fp <- pred_table[1, 2]
    fn <- pred_table[2, 1]
    tn <- pred_table[2, 2]
    Accuracy <- sum(diag(pred_table))/sum(pred_table)
    Precision <- tp/(tp + fp)
    Recall <- tp/(tp + fn)
    F1_Score <- 2 * Precision * Recall/(Precision + Recall)
    return(data.frame(Accuracy = Accuracy, Precision = Precision, Recall = Recall, F1_Score = F1_Score))
}
RF <- class_assess(train_rf_pred)
control_svm <- trainControl(method = "cv", number = 10, verboseIter = TRUE, savePredictions = "final")
model_svm <- train(Survived ~ Pclass + Sex + Age + Family + Embarked + Title, train, method = "svmLinear", trControl = control_svm)
train_svm_pred <- model_svm$pred[, c("pred", "obs")]
SVM <- class_assess(train_svm_pred)
control_knn <- trainControl(method = "cv", number = 10, verboseIter = TRUE, savePredictions = "final")
model_knn <- train(Survived ~ Pclass + Sex + Age + Family + Embarked + Title, train, method = "kknn", trControl = control_knn)
train_knn_pred <- model_knn$pred[, c("pred", "obs")]
KNN <- class_assess(train_knn_pred)
control_nnet <- trainControl(method = "cv", number = 10, verboseIter = TRUE, savePredictions = "final")
model_nnet <- train(Survived ~ Pclass + Sex + Age + Family + Embarked + Title, train, method = "nnet", trControl = control_nnet)
train_nnet_pred <- model_nnet$pred[, c("pred", "obs")]
NNET <- class_assess(train_nnet_pred)
comparison <- rbind(RF = RF, SVM = SVM, KNN = KNN, NNET = NNET)
comparison
test_pred <- predict.train(model_nnet, test)
titanic_prediction <- data.frame(PassengerId = test$PassengerId, Survived = test_pred)
write.csv(titanic_prediction, file = "Titanic_prediction.csv", row.names = FALSE)
