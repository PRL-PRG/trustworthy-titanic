library(dplyr)
library(ggplot2)
library(rpart)
library(randomForest)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
summary(train)
nrow(train)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
summary(test)
nrow(test)
train %>% summarise_all(funs(sum(is.na(.))))
test %>% summarise_all(funs(sum(is.na(.))))
sapply(train, function(x) length(which(x == "")))
sapply(test, function(x) length(which(x == "")))
sum(is.na(train$Age))
sum(is.na(test$Age))
train$Age[which(is.na(train$Age))] <- median(train$Age, na.rm = T)
test$Age[which(is.na(test$Age))] <- median(test$Age, na.rm = T)
sum(duplicated(train$PassengerId))
sum(duplicated(test$PassengerId))
train$Age <- gsub(".00", "", train$Age)
train$Survived <- factor(train$Survived)
train$Sex <- factor(train$Sex)
train$SibSp <- factor(train$SibSp)
train$Pclass <- factor(train$Pclass)
train$Embarked <- factor(train$Embarked)
train$Age_Category <- sapply(train$Age, function(x) {
    if (x >= 0 & x <= 12) {
        return("Child")
    }
    else if (x > 12 & x <= 24) {
        return("Very Young")
    }
    else if (x > 24 & x <= 36) {
        return("Young")
    }
    else if (x > 36 & x <= 48) {
        return("Middle Aged")
    }
    else if (x > 48) {
        return("Old")
    }
    else {
        return("NA")
    }
})
test$Age <- gsub(".00", "", test$Age)
test$Sex <- factor(test$Sex)
test$SibSp <- factor(test$SibSp)
test$Pclass <- factor(test$Pclass)
test$Embarked <- factor(test$Embarked)
test$Age_Category <- sapply(test$Age, function(x) {
    if (x >= 0 & x <= 12) {
        return("Child")
    }
    else if (x > 12 & x <= 24) {
        return("Very Young")
    }
    else if (x > 24 & x <= 36) {
        return("Young")
    }
    else if (x > 36 & x <= 48) {
        return("Middle Aged")
    }
    else if (x > 48) {
        return("Old")
    }
    else {
        return("NA")
    }
})
ncol(train)
summary(train$Fare)
ggplot(train, aes(y = train$Fare, x = train$Pclass)) + geom_boxplot(fill = "steelblue") + labs(x = "Class", y = "Fare", title = "Class wise Fare distribution")
prop.table(table(train$Survived))
ggplot(train, aes(x = train$Survived)) + geom_bar(fill = "orange") + labs(x = "Survived", y = "Passenger Count") + scale_y_continuous("Survived", breaks = seq(0, 500, by = 50)) + geom_text(aes(y = prop.table(..count..) * 100 + 0.5, label = paste0(prop.table(..count..) * 100, "%")), stat = "count", position = position_dodge(0.9), size = 4, vjust = -0.9)
ggplot(train, aes(x = train$Sex, fill = train$Survived)) + geom_bar(position = "fill")
ggplot(train, aes(x = train$Pclass, fill = train$Survived)) + geom_bar(position = "fill")
train$Age <- as.numeric(train$Age)
summary(train$Age)
data <- subset(train, train$Survived == 1)
ggplot(data, aes(x = data$Sex, fill = data$Pclass)) + geom_bar(position = "dodge")
ggplot(train, aes(x = train$Age_Category, fill = train$Survived)) + geom_bar(position = "fill")
Adults <- subset(train, Age > 12)
ggplot(Adults, aes(x = Adults$Sex, fill = Adults$Survived)) + geom_bar(position = "fill")
str(train$SibSp)
ggplot(train, aes(x = train$SibSp, fill = train$Survived)) + geom_bar(position = "fill")
ggplot(train, aes(x = factor(train$Parch), fill = train$Survived)) + geom_bar(position = "fill")
fit <- rpart(Survived ~ Age_Category + Sex + Pclass + SibSp, data = train)
print(predict(fit, head(train)))
print(head(train$Survived))
val <- as.data.frame(predict(fit, test))
colnames(val) <- c("NotSurvived", "Survived")
val$score <- ifelse(val$NotSurvived > val$Survived, 0, 1)
new <- data.frame(PassengerId = test$PassengerId, Survived = val$score)
write.csv(new, "predict_survival.csv", row.names = F)
nrow(new)
