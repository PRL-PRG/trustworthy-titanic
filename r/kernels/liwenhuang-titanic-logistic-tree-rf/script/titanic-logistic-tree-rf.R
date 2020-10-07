knitr::opts_chunk$set(echo = TRUE)
library(data.table)
library(ggplot2)
library(dplyr)
train <- fread("../input/train.csv")
test <- fread("../input/test.csv")
submission <- fread("../input/gender_submission.csv")
full <- bind_rows(train, test)
str(train)
head(train)
sapply(full, function(x) sum(is.na(x) | x == ""))
train$Female <- train$Sex == "female"
ggplot(full, aes(group = as.factor(Pclass), y = Fare, color = as.factor(Pclass))) + geom_boxplot() + coord_cartesian(xlim = c(-0.5, 0.5), ylim = c(-50, 300))
aggregate(Fare ~ Pclass, data = train, mean)
full[full$Embarked == ""]
ggplot(data = full, aes(x = Embarked, y = Fare, fill = factor(Pclass))) + geom_boxplot() + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", lwd = 1)
full$Embarked[full$Embarked == ""] <- "C"
full[is.na(full$Fare), ]
full$Fare[which(is.na(full$Fare))] <- 13.67555
cor(train[, -c("PassengerId", "Sex", "Name", "Ticket", "Cabin", "Embarked")])
ggplot(data = train, aes(x = Survived, fill = Sex)) + geom_bar(width = 0.5, position = "dodge")
ggplot(data = train, aes(x = Survived, fill = as.factor(Pclass))) + geom_bar(width = 0.5, position = "dodge")
cor(train[, -c("PassengerId", "Sex", "Name", "Ticket", "Cabin", "Embarked")], use = "complete.obs")
cor(full[, -c("PassengerId", "Sex", "Name", "Ticket", "Cabin", "Embarked")], use = "complete.obs")
train_logit1 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, family = "binomial")
summary(train_logit1)
hist(train$Age, breaks = 50, col = "lightblue", freq = F)
lines(density(train$Age, na.rm = T), col = "red")
lines(density(train$Age, adjust = 4, na.rm = T), lty = "dotted", col = "darkgreen", lwd = 2)
hist(test$Age, breaks = 50, col = "lightblue", freq = F)
lines(density(test$Age, na.rm = T), col = "red")
lines(density(test$Age, adjust = 4, na.rm = T), lty = "dotted", col = "darkgreen", lwd = 2)
hist(full$Age, breaks = 50, col = "lightblue", freq = F)
lines(density(full$Age, na.rm = T), col = "red")
lines(density(full$Age, adjust = 4, na.rm = T), lty = "dotted", col = "darkgreen", lwd = 2)
summary(train$Age, rm.na = T)
summary(test$Age, rm.na = T)
summary(full$Age, rm.na = T)
Age_missing <- full[is.na(full$Age)]
table(full$Pclass)
table(full$Pclass)/dim(full)[1]
table(Age_missing$Pclass)
table(Age_missing$Pclass)/dim(Age_missing)[1]
table(full$Sex)
table(full$Sex)/dim(full)[1]
table(Age_missing$Sex)
table(Age_missing$Sex)/dim(Age_missing)[1]
ggplot(data = full, aes(Age, fill = as.factor(Pclass))) + geom_histogram(aes(y = ..density..), alpha = 0.7, position = "identity", binwidth = 2, na.rm = T) + geom_density(alpha = 0.2, linetype = "dotted", na.rm = T)
summary(full$Age[full$Pclass == 1])
summary(full$Age[full$Pclass == 2])
summary(full$Age[full$Pclass == 3])
ggplot(data = full, aes(Age, fill = Sex)) + geom_histogram(aes(y = ..density..), alpha = 0.7, position = "identity", binwidth = 2, na.rm = T) + geom_density(alpha = 0.2, linetype = "dotted", na.rm = T)
library(mice)
mice_mod <- mice(data = full[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Cabin", "Embarked")], method = "rf")
mice_output <- complete(mice_mod)
par(mfrow = c(1, 2))
hist(full$Age, breaks = 50, col = "darkblue", freq = F, main = "Age: Original Data")
lines(density(full$Age, na.rm = T), col = "red")
lines(density(full$Age, adjust = 4, na.rm = T), lty = "dotted", col = "darkgreen", lwd = 2)
hist(mice_output$Age, breaks = 50, col = "lightblue", freq = F, main = "Age: MICE output")
lines(density(mice_output$Age, na.rm = T), col = "red")
lines(density(mice_output$Age, adjust = 4, na.rm = T), lty = "dotted", col = "darkgreen", lwd = 2)
par(mfrow = c(1, 2))
ggplot(data = full, aes(Age, fill = as.factor(Pclass))) + geom_histogram(aes(y = ..density..), alpha = 0.7, position = "identity", binwidth = 2, na.rm = T) + geom_density(alpha = 0.2, linetype = "dotted", na.rm = T) + ggtitle("Age: Original Data")
ggplot(data = mice_output, aes(Age, fill = as.factor(Pclass))) + geom_histogram(aes(y = ..density..), alpha = 0.7, position = "identity", binwidth = 2, na.rm = T) + geom_density(alpha = 0.2, linetype = "dotted", na.rm = T) + ggtitle("Age: MICE output")
full$Age <- mice_output$Age
sum(is.na(full$Age))
full$Cabin[1:30]
train_logit2 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = full[!is.na(full$Survived), ], family = "binomial")
summary(train_logit1)
summary(train_logit2)
anova(train_logit1, test = "Chisq")
anova(train_logit2, test = "Chisq")
train_predict1 <- predict(train_logit1, data = full[1:891, ], type = "response")
train_predict2 <- predict(train_logit2, data = full[1:891, ], type = "response")
train_1 <- cbind(train, train_predict1)
train_2 <- cbind(train, train_predict2)
ggplot(data = train, aes(train_predict2, fill = as.factor(Survived))) + geom_histogram(alpha = 0.5, position = "identity", binwidth = 0.01) + ggtitle("MICE outcome")
library(ROCR)
result2 <- prediction(train_predict2, train$Survived)
evaluation2 <- performance(result2, "acc")
plot(evaluation2)
abline(v = 0.6, col = "red")
abline(h = 0.81, col = "red")
max_index2 <- which.max(slot(evaluation2, "y.values")[[1]])
max_index2
acc2 <- slot(evaluation2, "y.values")[[1]][max_index2]
cut_off2 <- slot(evaluation2, "x.values")[[1]][max_index2]
print(c(Accuracy = acc2, Cutoff = cut_off2))
train_outcome2 <- ifelse(train_predict2 > 0.6121, 1, 0)
conf_table2 <- table(train$Survived, train_outcome2)
conf_table2
conf_table2[2, 2]/sum(conf_table2[, 2])
conf_table2[2, 1]/sum(conf_table2[, 1])
roc2 <- performance(result2, "tpr", "fpr")
plot(roc2, colorize = T)
abline(a = 0, b = 1, col = "grey")
auc2 <- performance(result2, "auc") %>% slot(., "y.values") %>% unlist() %>% round(., 4)
auc2
plot(roc2, colorize = T)
abline(a = 0, b = 1, col = "grey")
legend(0.65, 0.2, auc2, title = "AUC2")
library(pscl)
pR2(train_logit2)
full$Name[1:20]
Title <- gsub("(.*,\\s)|(\\..*)", "", full$Name)
table(Title, full$Sex)
table(Title[1:891], train$Survived)
Title[Title == "Mlle" | Title == "Mme" | Title == "Ms"] <- "Miss"
Title[Title == "Don" | Title == "Dona" | Title == "Master" | Title == "Lady" | Title == "Sir" | Title == "Jonkheer" | Title == "the Countess"] <- "Upper"
Title[Title == "Capt" | Title == "Col" | Title == "Major" | Title == "Rev" | Title == "Dr"] <- "Service"
tb_ts <- table(Title[1:891], train$Survived)
tb_ts
chisq.test(tb_ts)
table(full$SibSp, full$Survived)
table(full$Parch, full$Survived)
Fam_size <- full$Parch + full$SibSp
tb_fams <- table(Fam_size, full$Survived)
tb_fams
Fam_sizeD <- vector(mode = "numeric", length = length(Fam_size))
Fam_sizeD[Fam_size == 0] <- "Singleton"
Fam_sizeD[Fam_size > 0 & Fam_size < 5] <- "Small"
Fam_sizeD[Fam_size >= 5] <- "Big"
tb_fams <- table(Fam_sizeD, full$Survived)
tb_fams
chisq.test(tb_fams)
library(tibble)
full$Title <- Title
full$Fam_sizeD <- Fam_sizeD
train_logit3 <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Fam_sizeD + Title, family = "binomial", data = full[1:891, ])
summary(train_logit3)
train_predict3 <- predict(train_logit3, full[1:891], type = "response")
result3 <- prediction(train_predict3, train$Survived)
evaluation3 <- performance(result3, "acc")
plot(evaluation3)
abline(v = 0.6, col = "red")
abline(h = 0.81, col = "red")
max_index3 <- which.max(slot(evaluation3, "y.values")[[1]])
max_index3
acc3 <- slot(evaluation3, "y.values")[[1]][max_index3]
cut_off3 <- slot(evaluation3, "x.values")[[1]][max_index3]
print(c(Accuracy = acc3, Cutoff = cut_off3))
train_outcome3 <- ifelse(train_predict3 > 0.6291, 1, 0)
conf_table3 <- table(train$Survived, train_outcome3)
conf_table3
conf_table3[2, 2]/sum(conf_table3[, 2])
conf_table3[2, 1]/sum(conf_table3[, 1])
auc3 <- performance(result3, "auc") %>% slot(., "y.values") %>% unlist() %>% round(., 4)
auc3
roc3 <- performance(result3, "tpr", "fpr")
plot(roc3, colorize = T)
abline(a = 0, b = 1, col = "grey")
legend(0.85, 0.2, auc3, title = "AUC3")
auc2
pR2(train_logit3)
pR2(train_logit2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
train_tree <- rpart(formula = Survived ~ as.factor(Pclass) + Sex + Age + SibSp + Parch + Fare + Embarked + Fam_sizeD + Title, data = full[1:891, ], method = "class")
fancyRpartPlot(train_tree)
tree_predict <- predict(train_tree, data = full[1:891, ], type = "class")
conf_table4 <- table(train$Survived, tree_predict)
conf_table4
conf_table4[2, 2]/sum(conf_table4[, 2])
conf_table4[2, 1]/sum(conf_table4[, 1])
library(randomForest)
library(dplyr)
full = full %>% mutate_if(is.character, as.factor)
set.seed(111)
train_rf <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Fam_sizeD + Title, data = full[1:891, ], importance = T)
train_rf$confusion
varImpPlot(train_rf)
plot(train_rf, ylim = c(0, 0.36))
legend("topright", colnames(train_rf$err.rate), col = 1:3, fill = 1:3)
test_predict <- predict(train_rf, full[892:1309, ])
solution <- data.frame(PassengerID = test$PassengerId, Survived = test_predict)
write.csv(solution, file = "rf_Solution.csv", row.names = F)
