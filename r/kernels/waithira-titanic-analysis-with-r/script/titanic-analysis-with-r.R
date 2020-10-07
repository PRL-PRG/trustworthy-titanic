knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(dplyr)
library(GGally)
library(rpart)
library(rpart.plot)
library(randomForest)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
full <- bind_rows(train, test)
LT = dim(train)[1]
str(full)
colSums(is.na(full))
colSums(full == "")
full$Embarked[full$Embarked == ""] = "C"
apply(full, 2, function(x) length(unique(x)))
cols <- c("Survived", "Pclass", "Sex", "Embarked")
for (i in cols) {
    full[, i] <- as.factor(full[, i])
}
str(full)
ggplot(data = full[1:LT, ], aes(x = Sex, fill = Survived)) + geom_bar()
ggplot(data = full[1:LT, ], aes(x = Embarked, fill = Survived)) + geom_bar(position = "fill") + ylab("Frequency")
t <- table(full[1:LT, ]$Embarked, full[1:LT, ]$Survived)
for (i in 1:dim(t)[1]) {
    t[i, ] <- t[i, ]/sum(t[i, ]) * 100
}
t
ggplot(data = full[1:LT, ], aes(x = Pclass, fill = Survived)) + geom_bar(position = "fill") + ylab("Frequency")
ggplot(data = full[1:LT, ], aes(x = Embarked, fill = Survived)) + geom_bar(position = "fill") + facet_wrap(~Pclass)
ggplot(data = full[1:LT, ], aes(x = SibSp, fill = Survived)) + geom_bar()
ggplot(data = full[1:LT, ], aes(x = Parch, fill = Survived)) + geom_bar()
full$FamilySize <- full$SibSp + full$Parch + 1
full1 <- full[1:LT, ]
ggplot(data = full1[!is.na(full[1:LT, ]$FamilySize), ], aes(x = FamilySize, fill = Survived)) + geom_histogram(binwidth = 1, position = "fill") + ylab("Frequency")
ggplot(data = full1[!(is.na(full[1:LT, ]$Age)), ], aes(x = Age, fill = Survived)) + geom_histogram(binwidth = 3)
ggplot(data = full1[!is.na(full[1:LT, ]$Age), ], aes(x = Age, fill = Survived)) + geom_histogram(binwidth = 3, position = "fill") + ylab("Frequency")
ggplot(data = full[1:LT, ], aes(x = Fare, fill = Survived)) + geom_histogram(binwidth = 20, position = "fill")
full$Fare[is.na(full$Fare)] <- mean(full$Fare, na.rm = T)
sum(is.na(full$Age))
full$Age[is.na(full$Age)] <- mean(full$Age, na.rm = T)
sum(is.na(full$Age))
full$Title <- gsub("(.*, )|(\\..*)", "", full$Name)
full$Title[full$Title == "Mlle"] <- "Miss"
full$Title[full$Title == "Ms"] <- "Miss"
full$Title[full$Title == "Mme"] <- "Mrs"
full$Title[full$Title == "Lady"] <- "Miss"
full$Title[full$Title == "Dona"] <- "Miss"
officer <- c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Rev", "Sir", "the Countess")
full$Title[full$Title %in% officer] <- "Officer"
full$Title <- as.factor(full$Title)
ggplot(data = full[1:LT, ], aes(x = Title, fill = Survived)) + geom_bar(position = "fill") + ylab("Frequency")
train_im <- full[1:LT, c("Survived", "Pclass", "Sex", "Age", "Fare", "SibSp", "Parch", "Title")]
ind <- sample(1:dim(train_im)[1], 500)
train1 <- train_im[ind, ]
train2 <- train_im[-ind, ]
model <- glm(Survived ~ ., family = binomial(link = "logit"), data = train1)
summary(model)
pred.train <- predict(model, train2)
pred.train <- ifelse(pred.train > 0.5, 1, 0)
mean(pred.train == train2$Survived)
t1 <- table(pred.train, train2$Survived)
presicion <- t1[1, 1]/(sum(t1[1, ]))
recall <- t1[1, 1]/(sum(t1[, 1]))
presicion
recall
F1 <- 2 * presicion * recall/(presicion + recall)
F1
test_im <- full[LT + 1:1309, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Title")]
pred.test <- predict(model, test_im)[1:418]
pred.test <- ifelse(pred.test > 0.5, 1, 0)
res <- data.frame(test$PassengerId, pred.test)
names(res) <- c("PassengerId", "Survived")
write.csv(res, file = "res.csv", row.names = F)
model_dt <- rpart(Survived ~ ., data = train1, method = "class")
rpart.plot(model_dt)
pred.train.dt <- predict(model_dt, train2, type = "class")
mean(pred.train.dt == train2$Survived)
t2 <- table(pred.train.dt, train2$Survived)
presicion_dt <- t2[1, 1]/(sum(t2[1, ]))
recall_dt <- t2[1, 1]/(sum(t2[, 1]))
presicion_dt
recall_dt
F1_dt <- 2 * presicion_dt * recall_dt/(presicion_dt + recall_dt)
F1_dt
pred.test.dt <- predict(model_dt, test_im, type = "class")[1:418]
res_dt <- data.frame(test$PassengerId, pred.test.dt)
names(res_dt) <- c("PassengerId", "Survived")
write.csv(res_dt, file = "res_dt.csv", row.names = F)
model_rf <- randomForest(Survived ~ ., data = train1)
plot(model_rf)
pred.train.rf <- predict(model_rf, train2)
mean(pred.train.rf == train2$Survived)
t1 <- table(pred.train.rf, train2$Survived)
presicion <- t1[1, 1]/(sum(t1[1, ]))
recall <- t1[1, 1]/(sum(t1[, 1]))
presicion
recall
F1 <- 2 * presicion * recall/(presicion + recall)
F1
pred.test.rf <- predict(model_rf, test_im)[1:418]
res_rf <- data.frame(test$PassengerId, pred.test.rf)
names(res_rf) <- c("PassengerId", "Survived")
write.csv(res_rf, file = "res_rf.csv", row.names = F)
