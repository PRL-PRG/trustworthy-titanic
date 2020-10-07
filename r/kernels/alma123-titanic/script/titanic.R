library(ggplot2)
library(randomForest)
library(ROCR)
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
test$Survived <- NA
Data <- rbind(train, test)
Data$LName <- gsub(",.*$", "", Data$Name)
Data$Name1 <- gsub(".*,\\s", "", Data$Name)
Data$Title <- gsub("\\s.*$", "", Data$Name1)
Data$Cabin2 <- substr(Data$Cabin, 1, 1)
Data$Ticket2 <- gsub("\\s?[0-9]\\s?", "", Data$Ticket)
Data$Ticket2 <- gsub("\\.", "", Data$Ticket2)
Data$Ticket2 <- gsub("\\/", "", Data$Ticket2)
Data$Ticket2 <- toupper(Data$Ticket2)
Data$Title[Data$Title == "the"] <- "Countess"
Data$Title <- ifelse(Data$Title == "Mlle.", "Miss.", Data$Title)
Data$Title <- ifelse(Data$Title == "Ms.", "Miss.", Data$Title)
Data$Title <- ifelse(Data$Title == "Mme.", "Mrs.", Data$Title)
Data$Title2 <- Data$Title
Data$Title2 <- ifelse(!Data$Title == "Miss." & !Data$Title == "Mrs." & !Data$Title == "Mr." & !Data$Title == "Master.", "Other", Data$Title2)
Data$Name2 <- gsub("^.*[[:punct:]]\\s", "", Data$Name1)
Data$FName <- gsub("\\s.*", "", Data$Name2)
Data$Name3 <- paste(Data$FName, Data$LName)
Data$Parch2 <- ifelse(Data$Parch > 5, 5, Data$Parch)
Data$SibSp2 <- ifelse(Data$SibSp > 5, 5, Data$SibSp)
Noble <- c("Don.", "Lady.", "Sir.", "Countess", "Jonkheer.")
Data$Noble <- ifelse(Data$Title %in% Noble, 1, 0)
a <- aggregate(Data$PassengerId, by = list(Data$LName), FUN = c("length"))
names(a) <- c("LName", "Family")
Data <- merge(Data, a, by = "LName", all = T)
fitage <- lm(Age ~ Pclass + Title2 + SibSp2, data = Data)
summary(fitage)
x <- Data[is.na(Data$Age), ]
x$gAge <- round(predict(fitage, x, se.fit = TRUE)[[1]], 3)
x <- x[c("PassengerId", "gAge")]
Data <- merge(Data, x, by = "PassengerId", all = T)
Data$aAge <- ifelse(is.na(Data$Age), Data$gAge, Data$Age)
Data$AgeCat <- ifelse(Data$aAge <= 5, 1, 0)
Data$AgeCat <- ifelse(Data$aAge > 5 & Data$aAge <= 12, 2, Data$AgeCat)
Data$AgeCat <- ifelse(Data$aAge > 12 & Data$aAge <= 19, 3, Data$AgeCat)
Data$AgeCat <- ifelse(Data$aAge > 19, 4, Data$AgeCat)
Data$Cabin2[Data$Cabin2 == ""] = "Z"
Data$Ticket2[Data$Ticket2 == ""] = "Z"
Data$Embarked[Data$Embarked == ""] = "S"
Data <- Data[c("PassengerId", "Name", "Survived", "Pclass", "Sex", "SibSp", "SibSp2", "Parch", "Parch2", "Embarked", "Title", "Title2", "Cabin", "Cabin2", "Ticket", "Ticket2", "Noble", "Family", "Age", "aAge", "Fare")]
Data$Ticket2 <- ifelse(Data$Ticket2 %in% c("STONOQ", "AQ", "LP"), "Z", Data$Ticket2)
Data[4:18] <- lapply(Data[4:18], as.factor)
train <- Data[!is.na(Data$Survived), ]
test <- Data[is.na(Data$Survived), ]
test$Survived <- NULL
set.seed(1)
extractFeatures <- function(data) {
    features <- c("Pclass", "Title2", "Cabin2", "Ticket2", "aAge", "Family", "Sex", "Parch2", "SibSp2", "Fare", "Embarked")
    fea <- data[, features]
    fea$Sex <- as.factor(fea$Sex)
    fea$Embarked <- as.factor(fea$Embarked)
    fea$Title2 <- as.factor(fea$Title2)
    fea$Family <- as.factor(fea$Family)
    fea$Cabin2 <- as.factor(fea$Cabin2)
    fea$SibSp2 <- as.factor(fea$SibSp2)
    fea$Parch2 <- as.factor(fea$Parch2)
    fea$Ticket2 <- as.factor(fea$Ticket2)
    fea$Fare[is.na(fea$Fare)] <- 8.05
    return(fea)
}
rf <- randomForest(extractFeatures(train), as.factor(train$Survived), mtry = 4, ntree = 1000, keep.forest = TRUE, importance = TRUE)
Prob <- predict(rf, extractFeatures(train), type = "prob")[, 2]
pred <- prediction(Prob, train$Survived)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve for Random Forest", col = 2, lwd = 2)
abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")
auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc
train2 <- train
train2$Ans <- predict(rf, extractFeatures(train2))
train2$Ans <- as.numeric(as.character(train2$Ans))
train2$Res <- abs(train2$Ans - train2$Survived)
mean(train2$Res)
imp <- importance(rf, type = 1)
featureImportance <- data.frame(Feature = row.names(imp), Importance = imp[, 1])
p <- ggplot(featureImportance, aes(x = reorder(Feature, Importance), y = Importance)) + geom_bar(stat = "identity", fill = "#53cfff") + coord_flip() + theme_light(base_size = 20) + xlab("") + ylab("Importance") + ggtitle("Random Forest Feature Importance\n") + theme(plot.title = element_text(size = 18))
p
submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predict(rf, extractFeatures(test))
write.csv(submission, file = "1_random_forest_r_submission.csv", row.names = FALSE)
