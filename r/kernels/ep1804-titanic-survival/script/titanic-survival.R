library(ggplot2)
library(ggthemes)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
d0 <- merge(train, test, all = T)
for (i in 1:ncol(d0)) {
    if (is.character(d0[, i])) {
        d0[d0[, i] == "", i] <- NA
    }
}
summary(d0)
apply(d0, 2, function(x) {
    sum(is.na(x))
})
d0$PassengerId <- NULL
unique(d0$Pclass)
d0$Pclass <- factor(d0$Pclass)
extract_title <- function(name) {
    title <- gsub("(.*, )|(\\..*)", "", name)
    title[title == "Mlle"] <- "Miss"
    title[title == "Ms"] <- "Miss"
    title[title == "Mme"] <- "Mrs"
    tb <- table(title)
    title[title %in% names(tb)[tb < 10]] <- "Rare"
    cat(noquote(paste("Titles summary:")))
    print(table(d0$Sex, title))
    title
}
d0$Title <- factor(extract_title(d0$Name))
d0$Name <- NULL
d0$Sex <- factor(d0$Sex)
d0$FamilySize <- d0$SibSp + d0$Parch + 1
d0$Cabin <- NULL
d0$Ticket <- NULL
d0$Embarked <- factor(d0$Embarked)
d0$Survived <- factor(d0$Survived)
tr <- head(d0, nrow(train))
cv <- tail(d0, nrow(test))
cv$Survived <- NULL
library(mice)
mice_fit <- mice(tr, method = "rf")
tr <- complete(mice_fit)
mice_fit <- mice(cv, method = "rf")
cv <- complete(mice_fit)
apply(tr, 2, function(x) {
    sum(is.na(x))
})
apply(cv, 2, function(x) {
    sum(is.na(x))
})
library(randomForest)
fit <- randomForest(Survived ~ ., data = tr)
plot(fit)
legend("topright", colnames(fit$err.rate), col = 1:3, fill = 1:3)
imp <- importance(fit)
imp <- as.data.frame(imp)
p <- ggplot(imp, aes(x = reorder(rownames(imp), MeanDecreaseGini), y = MeanDecreaseGini)) + geom_bar(stat = "identity") + xlab("Features") + ggtitle("Random Forest Feature Importance\n") + coord_flip() + theme_few()
ggsave("feature_importance.png", p)
library(caret)
library(ranger)
fit <- train(Survived ~ ., data = tr, method = "ranger", trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE))
plot(fit, main = "Random Forest Parameter Tuning by CV")
pred <- data.frame(PassengerId = test$PassengerId, Survived = predict(fit, cv))
write.csv(pred, file = "pred.csv", quote = F, row.names = F)
