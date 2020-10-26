library(dplyr)
library(plyr)
library(randomForest)
library(bit64)
library(Amelia)
library(e1071)
options(scipen = 999)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
str(train)
str(test)
sapply(train, summary)
sapply(test, summary)
test$Survived <- NA
missmap(train)
missmap(test)
both <- rbind(train, test)
sapply(both, summary)
aggregate(Age ~ Sex, both, mean)
for (i in 1:nrow(both)) {
    if (both$Sex[i] == "male" && is.na(both$Age[i]) == TRUE) {
        both$Age[i] <- 30.58
    }
    else if (both$Sex[i] == "female" && is.na(both$Age[i]) == TRUE) {
        both$Age[i] <- 28.68
    }
    else {
        both$Age[i] <- both$Age[i]
    }
}
aggregate(Fare ~ Pclass + Embarked, both, range)
table(both$Embarked)
both$Embarked[c(62, 830)] <- "S"
aggregate(Fare ~ Pclass, both, median)
both$Fare[c(1044)] <- 8.05
t = count(both, "Cabin")
t
1014/1309
both$Ticket <- as.character(both$Ticket)
both$Name <- as.character(both$Name)
missmap(both)
both$IsChild <- ifelse(both$Age < 18, 1, 0)
both$Fsize <- both$SibSp + both$Parch + 1
table(both$Fsize)
both$Fcat <- "Large"
both$Fcat[both$Fsize == 1] <- "Single"
both$Fcat[both$Fsize > 1 & both$Fsize <= 4] <- "Small"
head(both$Name)
both$Title <- sapply(both$Name, FUN = function(x) {
    strsplit(x, split = "[,.]")[[1]][2]
})
both$Title <- gsub(" ", "", both$Title)
table(both$Title)
lowcnt_title <- c("Dona", "Lady", "Capt", "Col", "Don", "Dr", "Major", "Rev", "Sir", "Jonkheer", "theCountess")
both$Title[both$Title %in% c("Ms", "Mlle")] <- "Miss"
both$Title[both$Title == "Mme"] <- "Mrs"
both$Title[both$Title %in% lowcnt_title] <- "Others"
table(both$Title)
both$Fare2 <- "30+"
both$Fare2[both$Fare < 30 & both$Fare >= 20] <- "20-30"
both$Fare2[both$Fare < 20 & both$Fare >= 10] <- "10-20"
both$Fare2[both$Fare < 10] <- "<10"
both$Age2 <- "Above 50"
both$Age2[both$Age <= 50 & both$Age > 40] <- "41-50"
both$Age2[both$Age <= 40 & both$Age > 30] <- "31-40"
both$Age2[both$Age <= 30 & both$Age > 20] <- "21-30"
both$Age2[both$Age <= 20 & both$Age > 10] <- "11-20"
both$Age2[both$Age <= 10] <- "Within 10"
both$Title <- as.factor(both$Title)
both$Fcat <- as.factor(both$Fcat)
both$SibSp <- as.factor(both$SibSp)
both$Parch <- as.factor(both$Parch)
both$Pclass <- as.factor(both$Pclass)
both$Fare2 <- as.factor(both$Fare2)
both$IsChild <- as.factor(both$IsChild)
both$Age2 <- as.factor(both$Age2)
data_train <- both[1:891, ]
data_test <- both[892:1309, ]
table(data_train$Survived)
prop.table(table(data_train$Survived))
data_test$Survived <- 0
table(data_train$Sex, data_train$Survived)
prop.table(table(data_train$Sex, data_train$Survived), 1)
t = count(data_train, c("Survived", "IsChild", "Sex"))
t
boxplot(data_train$Survived ~ data_train$Embarked)
table(data_train$Embarked, data_train$Survived)
prop.table(table(data_train$Embarked, data_train$Survived), 1)
boxplot(data_train$Survived ~ data_train$Pclass)
table(data_train$Pclass, data_train$Survived)
prop.table(table(data_train$Pclass, data_train$Survived), 1)
aggregate(Survived ~ Pclass + Embarked, data_train, sum)
aggregate(Survived ~ Pclass + Sex + Embarked, data_train, sum)
data_test$Survived <- ifelse(data_test$Sex == "female", 1, 0)
data_train$Survived <- as.factor(data_train$Survived)
data_test$Survived <- as.factor(data_test$Survived)
data_1 <- data_train[, c("Survived", "Pclass", "Sex", "SibSp", "Parch", "Fare2", "Embarked", "Fcat", "Title", "Age2")]
data_test1 <- data_test[, c(2, 3, 5, 7, 8, 12, 15, 16, 17, 18)]
nb_model <- naiveBayes(Survived ~ ., data = data_1)
nb_model
nb_test_predict <- predict(nb_model, data_test1[, -1])
table(pred = nb_test_predict, true = data_test1$Survived)
mean(nb_test_predict == data_test1$Survived)
svm_model <- svm(Survived ~ ., data = data_1)
svm_model
svm_test_predict <- predict(svm_model, data_test1[, -1])
table(pred = svm_test_predict, true = data_test1$Survived)
mean(svm_test_predict == data_test1$Survived)
MyOutput <- data.frame(PassengerId = data_test$PassengerId, Survived = svm_test_predict)
write.csv(MyOutput, "MyOutput.csv", row.names = FALSE)
