library(randomForest)
tit_train <- read.csv("../input/train.csv", header = TRUE)
tit_test <- read.csv("../input/test.csv", header = TRUE)
head(tit_train)
names.split <- strsplit(as.character(tit_train$Name), "[,.]")
test.names.split <- strsplit(as.character(tit_test$Name), "[,.]")
title <- rep(NA, length(names.split))
test.title <- rep(NA, length(test.names.split))
for (i in 1:length(names.split)) {
    title[i] <- trimws(names.split[[i]][2])
}
for (i in 1:length(test.names.split)) {
    test.title[i] <- trimws(test.names.split[[i]][2])
}
table(title)
table(test.title)
respect <- c("Capt", "Col", "Don", "Dona", "Dr", "Jonkheer", "Lady", "Major", "Rev", "Sir", "the Countess")
tit_train$Title <- title
tit_test$Title <- test.title
tit_train$Title[which(tit_train$Title == "Mlle")] <- "Ms"
tit_test$Title[which(tit_test$Title == "Mlle")] <- "Ms"
tit_train$Title[which(tit_train$Title == "Mme")] <- "Mrs"
tit_test$Title[which(tit_test$Title == "Mme")] <- "Mrs"
tit_train$Title[which(tit_train$Title == "Don")] <- "Sir"
tit_test$Title[which(tit_test$Title == "Don")] <- "Sir"
tit_train$Title[which(tit_train$Title == "Dona")] <- "Lady"
tit_test$Title[which(tit_test$Title == "Dona")] <- "Lady"
tit_test$Title[which(tit_test$Title %in% respect)] <- "Resp"
tit_train$Title[which(tit_train$Title %in% respect)] <- "Resp"
tit_train$Title <- factor(tit_train$Title)
tit_test$Title <- factor(tit_test$Title)
levels(tit_test$Title) <- levels(tit_train$Title)
table(Survived = tit_train$Survived, Sex = tit_train$Sex)
barplot(table(tit_train$Survived, tit_train$Sex), legend = c("Died", "Survived"), xlab = "Sex", ylab = "Frequency", main = "Female vs Male Survival Rate", col = c("darkred", "lightblue"))
table(Survived = tit_train$Survived[tit_train$Sex == "female"], title[tit_train$Sex == "female"])
tit_train$Sex <- ifelse(tit_train$Sex == "male", 1, 0)
tit_test$Sex <- ifelse(tit_test$Sex == "male", 1, 0)
marrWom <- rep(0, nrow(tit_train))
marrWom.test <- rep(0, nrow(tit_test))
marrWom <- ifelse(title == "Mrs", 1, 0)
marrWom.test <- ifelse(test.title == "Mrs", 1, 0)
tit_train$marrWom <- marrWom
tit_test$marrWom <- marrWom.test
isBoy <- rep(0, nrow(tit_train))
isBoy.test <- rep(0, nrow(tit_test))
isBoy <- ifelse(title == "Master", 1, 0)
isBoy.test <- ifelse(test.title == "Master", 1, 0)
tit_train$isBoy <- isBoy
tit_test$isBoy <- isBoy.test
barplot(table(tit_train$Parch[title == "Miss"], tit_train$Age[title == "Miss"]), legend = c("0 Parents/Children", "1 Parent/Child", "2 Parents/Children"), xlab = "Age", ylab = "Frequency", main = "Ages of those titled 'Miss'")
barplot(table(tit_train$SibSp[title == "Miss"], tit_train$Age[title == "Miss"]), legend = c("0 Siblings/Spouses", "1 Sib/Spouse", "2 Sib/Spouse", "3 Sib/Spouse", "4 Sib/Spouse", "5 Sib/Spouse", "8 Sib/Spouse"), col = c("darkslategrey", "darkslategray4", "darkslategray3", "darkslategray1", "goldenrod4", "goldenrod3", "goldenrod1"), xlab = "Age", ylab = "Frequency", main = "Ages of those titled 'Miss'")
mean(tit_train$Age[title == "Miss" | title == "Ms"], na.rm = TRUE)
sqrt(var(c(tit_train$Age[title == "Miss" | title == "Ms"], tit_test$Age[test.title == "Miss" | test.title == "Ms"]), na.rm = TRUE))
youngFem <- rep(0, nrow(tit_train))
youngFem.test <- rep(0, nrow(tit_test))
miss <- c(which(title == "Miss"), which(title == "Ms"))
miss.test <- c(which(test.title == "Miss"), which(test.title == "Ms"))
for (i in miss) {
    if (is.na(tit_train$Age[i])) {
        if (tit_train$SibSp[i] >= 1 || tit_train$Parch[i] >= 1) 
            youngFem[i] <- 1
    }
    else if (tit_train$Age[i] <= 25) 
        youngFem[i] <- 1
}
for (i in miss.test) {
    if (is.na(tit_test$Age[i])) {
        if (tit_test$SibSp[i] >= 1 || tit_test$Parch[i] >= 1) 
            youngFem.test[i] <- 1
    }
    else if (tit_test$Age[i] <= 25) 
        youngFem.test[i] <- 1
}
tit_train$youngFem <- youngFem
tit_test$youngFem <- youngFem.test
tit_train$famSize <- tit_train$SibSp + tit_train$Parch + 1
tit_test$famSize <- tit_test$SibSp + tit_test$Parch + 1
missing.Fare <- which(is.na(tit_test$Fare))
tit_test[missing.Fare, ]
cor(tit_train$Fare, tit_train$Pclass)
tit_test$Fare[missing.Fare] <- mean(tit_train$Fare[tit_train$Pclass == 3])
par(mfrow = c(2, 2))
plot(density(tit_train$Fare), main = "Train Fare")
plot(density(log(tit_train$Fare)), main = "Train Log(Fare)")
plot(density(tit_test$Fare), main = "Test Fare")
plot(density(log(tit_test$Fare)), main = "Test Log(Fare)")
dev.off()
tit_train$Fare <- log(1 + tit_train$Fare)
tit_test$Fare <- log(1 + tit_test$Fare)
levels(tit_train$Embarked)
levels(tit_test$Embarked)
missing.Embarked <- which(tit_train$Embarked == "")
tit_train$Embarked[missing.Embarked] <- "S"
tit_train[missing.Embarked, ]
table(tit_train$Survived, tit_train$Embarked)
tit_train$Embarked.S <- ifelse(tit_train$Embarked == "S", 1, 0)
tit_train$Embarked.Q <- ifelse(tit_train$Embarked == "Q", 1, 0)
tit_train$Embarked.C <- ifelse(tit_train$Embarked == "C", 1, 0)
tit_test$Embarked.S <- ifelse(tit_test$Embarked == "S", 1, 0)
tit_test$Embarked.Q <- ifelse(tit_test$Embarked == "Q", 1, 0)
tit_test$Embarked.C <- ifelse(tit_test$Embarked == "C", 1, 0)
tit_train$cab <- ifelse(tit_train$Cabin == "" | tit_train$Cabin == "T", "U", substr(tit_train$Cabin, 1, 1))
tit_test$cab <- ifelse(tit_test$Cabin == "" | tit_test$Cabin == "T", "U", substr(tit_test$Cabin, 1, 1))
table(tit_train$Survived, tit_train$cab)
tit_train$cab.A <- ifelse(tit_train$cab == "A", 1, 0)
tit_train$cab.B <- ifelse(tit_train$cab == "B", 1, 0)
tit_train$cab.C <- ifelse(tit_train$cab == "C", 1, 0)
tit_train$cab.D <- ifelse(tit_train$cab == "D", 1, 0)
tit_train$cab.E <- ifelse(tit_train$cab == "E", 1, 0)
tit_train$cab.F <- ifelse(tit_train$cab == "F", 1, 0)
tit_train$cab.G <- ifelse(tit_train$cab == "G", 1, 0)
tit_train$cab.U <- ifelse(tit_train$cab == "U", 1, 0)
tit_test$cab.A <- ifelse(tit_test$cab == "A", 1, 0)
tit_test$cab.B <- ifelse(tit_test$cab == "B", 1, 0)
tit_test$cab.C <- ifelse(tit_test$cab == "C", 1, 0)
tit_test$cab.D <- ifelse(tit_test$cab == "D", 1, 0)
tit_test$cab.E <- ifelse(tit_test$cab == "E", 1, 0)
tit_test$cab.F <- ifelse(tit_test$cab == "F", 1, 0)
tit_test$cab.G <- ifelse(tit_test$cab == "G", 1, 0)
tit_test$cab.U <- ifelse(tit_test$cab == "U", 1, 0)
attach(tit_train)
K = 5
folds <- sample(1:K, nrow(tit_train), replace = TRUE)
error <- rep(0, 5)
for (i in 1:K) {
    log.fit <- glm(factor(Survived) ~ Pclass + Sex, data = tit_train, subset = which(folds != i), family = binomial)
    log.train.probs <- predict(log.fit, newdata = tit_train[folds == i, ], type = "response")
    log.train.preds <- ifelse(log.train.probs >= 0.5, 1, 0)
    error[i] <- mean(log.train.preds != Survived[folds == i])
}
print(paste("Error rate: ", mean(error)))
print(paste("Success rate: ", 1 - mean(error)))
set.seed(1)
rf.fit <- randomForest(factor(Survived) ~ Pclass + Sex + famSize + isBoy + youngFem + marrWom + Embarked.S + Embarked.C + Embarked.Q, data = tit_train, mtry = 2, ntree = 15000, nodesize = 1, importance = FALSE)
rf.pred.train <- predict(rf.fit, newdata = tit_train, type = "class")
print(paste("Success rate on training data: ", mean(rf.pred.train == tit_train$Survived)))
rf.pred.out <- predict(rf.fit, newdata = tit_test, type = "class")
results <- data.frame(PassengerId = 892:1309, Survived = rf.pred.out)
write.csv(results, "submission.csv", row.names = FALSE)
