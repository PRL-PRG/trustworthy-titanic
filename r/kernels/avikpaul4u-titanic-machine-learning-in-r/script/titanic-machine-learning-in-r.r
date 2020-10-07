library(dplyr)
library(ROCR)
library(caret)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
list.files(path = "../input")
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
train_df <- train
test_df <- test
test$Survived <- ""
fullset <- rbind(train, test)
head(fullset)
str(fullset)
fullset$Cabin <- NULL
train_df$Survived <- as.factor(train_df$Survived)
train_df$Pclass <- as.factor(train_df$Pclass)
ggplot(train_df, aes(x = Pclass, fill = Survived)) + geom_bar(width = 0.3)
ggplot(train_df, aes(x = Sex, fill = Survived)) + geom_bar(width = 0.3)
ggplot(train_df, aes(Sex)) + facet_wrap(~Pclass) + geom_bar(aes(y = (..count..)/sum(..count..), fill = Survived), stat = "count") + geom_text(aes(label = scales::percent(round((..count..)/sum(..count..), 2)), y = ((..count..)/sum(..count..))), stat = "count", vjust = -0.25) + ggtitle("Class") + labs(y = "percent")
Imbalance_Check <- aggregate(PassengerId ~ Survived, train_df, length)
colnames(Imbalance_Check)[2] <- "No_of_passengers"
Imbalance_Check$Contribution <- (Imbalance_Check$No_of_passengers/sum(Imbalance_Check$No_of_passengers)) * 100
Imbalance_Check
summary(fullset)
fullset$Embarked[fullset$Embarked == ""] <- "S"
summary(fullset$Embarked)
Means_Age <- summarise(group_by(fullset, Pclass, Sex, Embarked), Mean_Age = mean(Age, na.rm = T))
Means_Age$key <- paste0(Means_Age$Pclass, "_", Means_Age$Sex, "_", Means_Age$Embarked)
head(Means_Age)
fullset$key <- paste0(fullset$Pclass, "_", fullset$Sex, "_", fullset$Embarked)
fullset <- merge(x = fullset, y = Means_Age[, c("key", "Mean_Age")], by = "key", all.x = T)
fullset$Age <- ifelse(is.na(fullset$Age), fullset$Mean_Age, fullset$Age)
fullset$Mean_Age <- NULL
fullset$key <- NULL
summary(fullset$Age)
fullset$Fare <- ifelse(is.na(fullset$Fare), mean(fullset$Fare, na.rm = T), fullset$Fare)
fullset$Title <- sapply(as.character(fullset$Name), FUN = function(x) {
    strsplit(x, "[,.]")[[1]][2]
})
fullset$Title <- sub(" ", "", fullset$Title)
fullset$Title <- as.factor(fullset$Title)
summary(fullset$Title)
fullset$Title <- as.character(fullset$Title)
fullset$Title[fullset$Title %in% c("Mlle", "Ms")] <- "Miss"
fullset$Title[fullset$Title == "Mme"] <- "Mrs"
fullset$Title[fullset$Title %in% c("Don", "Sir", "Jonkheer", "Rev", "Dr")] <- "Sir"
fullset$Title[fullset$Title %in% c("Dona", "Lady", "the Countess")] <- "Lady"
fullset$Title[fullset$Title %in% c("Capt", "Col", "Major")] <- "Officer"
fullset$Title <- as.factor(fullset$Title)
summary(fullset$Title)
fullset$FamSize <- fullset$SibSp + fullset$Parch
table(fullset$FamSize)
fullset$FamGroup[fullset$FamSize == 0] <- "Individual"
fullset$FamGroup[fullset$FamSize < 5 & fullset$FamSize > 0] <- "small"
fullset$FamGroup[fullset$FamSize > 4] <- "large"
fullset$FamGroup <- as.factor(fullset$FamGroup)
length(unique(fullset$Ticket))
length(unique(fullset$PassengerId))
Ticket_Count <- data.frame(table(fullset$Ticket))
head(Ticket_Count)
fullset <- merge(fullset, Ticket_Count, by.x = "Ticket", by.y = "Var1", all.x = T)
fullset$Ticket_Size[fullset$Freq == 1] <- "Single"
fullset$Ticket_Size[fullset$Freq >= 2 & fullset$Freq < 5] <- "Small"
fullset$Ticket_Size[fullset$Freq >= 5] <- "Big"
fullset$Ticket_Size <- as.factor(fullset$Ticket_Size)
fullset$isMinor[fullset$Age < 18] <- "Minor"
fullset$isMinor[fullset$Age >= 18] <- "Adult"
fullset$isMinor <- as.factor(fullset$isMinor)
head(fullset)
str(fullset)
fullset$Survived <- as.factor(fullset$Survived)
fullset$Pclass <- as.factor(fullset$Pclass)
train_df <- subset(fullset, !(fullset$Survived == ""))
str(train_df$Survived)
x <- data.frame(Survived = droplevels(train_df$Survived))
train_df$Survived <- NULL
train_df <- cbind(train_df, x)
str(train_df$Survived)
train_val <- sample_frac(train_df, size = 0.8)
test_val <- subset(train_df, !(train_df$PassengerId %in% train_val$PassengerId))
mod <- glm(Survived ~ Pclass + Title + FamGroup + Sex + isMinor + Ticket_Size + Embarked, data = train_val, family = "binomial")
summary(mod)
predict_train <- predict(mod, train_val, type = "response")
prob_train <- ifelse(predict_train > 0.5, 1, 0)
confusion_matrix_train <- table(prob_train, train_val$Survived)
print(confusion_matrix_train)
Accuracy_train <- sum(diag(confusion_matrix_train))/sum(confusion_matrix_train)
print(Accuracy_train * 100)
pred1 <- prediction(predict(mod), train_val$Survived)
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1)
predict_test <- predict(mod, test_val, type = "response")
prob_test <- ifelse(predict_test > 0.5, 1, 0)
confusion_matrix_test <- table(prob_test, test_val$Survived)
Accuracy_test <- sum(diag(confusion_matrix_test))/sum(confusion_matrix_test)
print(Accuracy_test * 100)
mymod <- rpart(Survived ~ Pclass + Title + FamGroup + Sex + isMinor + Ticket_Size + Embarked, data = train_val, method = "class")
rpart.plot(mymod, fallen.leaves = F, extra = 3)
predict_train_dt = predict(mymod, data = train_val, type = "class")
confusionMatrix(predict_train_dt, train_val$Survived)
prediction_test_dt <- predict(mymod, test_val, type = "class")
confusionMatrix(prediction_test_dt, test_val$Survived)
set.seed(1234)
mod_rf <- randomForest(Survived ~ Pclass + Title + FamGroup + Sex + isMinor + Ticket_Size + Embarked, data = train_val[, c("Survived", "Pclass", "Title", "FamGroup", "Sex", "isMinor", "Ticket_Size", "Embarked")], importance = TRUE, ntree = 1000, mtry = 2)
print(mod_rf)
varImpPlot(mod_rf)
set.seed(1234)
mod_rf <- randomForest(Survived ~ Pclass + Title + FamGroup + Sex + isMinor + Ticket_Size, data = train_val[, c("Survived", "Pclass", "Title", "FamGroup", "Sex", "isMinor", "Ticket_Size")], importance = TRUE, ntree = 1000, mtry = 2)
print(mod_rf)
set.seed(1234)
mod_rf_val <- randomForest(Survived ~ Pclass + Title + FamGroup + Sex + isMinor + Ticket_Size + Embarked, data = test_val[, c("Survived", "Pclass", "Title", "FamGroup", "Sex", "isMinor", "Ticket_Size", "Embarked")], importance = TRUE, ntree = 1000, mtry = 2)
print(mod_rf_val)
test_df <- subset(fullset, (fullset$Survived == ""))
prediction_final <- predict(mod_rf, test_df)
submission <- data.frame(PassengerID = test_df$PassengerId, Survived = prediction_final)
write.csv(submission, file = "Submission.csv", row.names = F)
