library(dplyr)
library(Amelia)
library(ggplot2)
library(scales)
library(caTools)
library(car)
library(ROCR)
library(e1071)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)
titanic_train = read.csv("../input/train.csv")
titanic_test = read.csv("../input/test.csv")
titanic <- bind_rows(titanic_train, titanic_test)
str(titanic)
colSums(is.na(titanic) | titanic == "")
missmap(titanic, main = "Titanic Data - Missings Map", col = c("yellow", "black"), legend = FALSE)
filter(titanic, is.na(Fare) == TRUE | Fare == "")
ggplot(filter(titanic, Pclass == 3 & Embarked == "S"), aes(Fare)) + geom_density(fill = "blue", alpha = 0.5) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "darkblue", linetype = "dashed", size = 2) + geom_vline(aes(xintercept = mean(Fare, na.rm = T)), colour = "red", linetype = "dashed", size = 2) + ggtitle("Fare distribution of third class passengers \n embarked from Southampton port") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
titanic$Fare[is.na(titanic$Fare) == TRUE] = median(filter(titanic, Pclass == 3 & Embarked == "S")$Fare, na.rm = TRUE)
colSums(is.na(titanic) | titanic == "")
filter(titanic, is.na(Embarked) == TRUE | Embarked == "")
table(filter(titanic, Pclass == 1)$Embarked)
ggplot(filter(titanic, is.na(Embarked) == FALSE & Embarked != "" & Pclass == 1), aes(Embarked, Fare)) + geom_boxplot(aes(colour = Embarked)) + geom_hline(aes(yintercept = 80), colour = "red", linetype = "dashed", size = 2) + ggtitle("Fare distribution of first class passengers") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
titanic$Embarked[titanic$Embarked == ""] = "C"
colSums(is.na(titanic) | titanic == "")
ggplot(titanic, aes(Pclass, Age)) + geom_boxplot(aes(fill = factor(Pclass)), alpha = 0.5) + ggtitle("Age distribution based on Pclass")
impute.age <- function(age, class) {
    vector <- age
    for (i in 1:length(age)) {
        if (is.na(age[i])) {
            if (class[i] == 1) {
                vector[i] <- round(mean(filter(titanic, Pclass == 1)$Age, na.rm = TRUE), 0)
            }
            else if (class[i] == 2) {
                vector[i] <- round(mean(filter(titanic, Pclass == 2)$Age, na.rm = TRUE), 0)
            }
            else {
                vector[i] <- round(mean(filter(titanic, Pclass == 3)$Age, na.rm = TRUE), 0)
            }
        }
        else {
            vector[i] <- age[i]
        }
    }
    return(vector)
}
imputed.age <- impute.age(titanic$Age, titanic$Pclass)
titanic$Age <- imputed.age
colSums(is.na(titanic) | titanic == "")
head(titanic$Name)
titanic$Title <- gsub("^.*, (.*?)\\..*$", "\\1", titanic$Name)
table(titanic$Sex, titanic$Title)
titanic$Title[titanic$Title == "Mlle" | titanic$Title == "Ms"] <- "Miss"
titanic$Title[titanic$Title == "Mme"] <- "Mrs"
Other <- c("Dona", "Dr", "Lady", "the Countess", "Capt", "Col", "Don", "Jonkheer", "Major", "Rev", "Sir")
titanic$Title[titanic$Title %in% Other] <- "Other"
table(titanic$Sex, titanic$Title)
FamilySize <- titanic$SibSp + titanic$Parch + 1
table(FamilySize)
titanic$FamilySize <- sapply(1:nrow(titanic), function(x) ifelse(FamilySize[x] == 1, "Single", ifelse(FamilySize[x] > 4, "Large", "Small")))
table(titanic$FamilySize)
titanic$Survived = factor(titanic$Survived)
titanic$Pclass = factor(titanic$Pclass)
titanic$Sex = factor(titanic$Sex)
titanic$Embarked = factor(titanic$Embarked)
titanic$Title = factor(titanic$Title)
titanic$FamilySize = factor(titanic$FamilySize, levels = c("Single", "Small", "Large"))
str(titanic)
ggplot(filter(titanic, is.na(Survived) == FALSE), aes(Pclass, fill = Survived)) + geom_bar(aes(y = (..count..)/sum(..count..)), alpha = 0.9, position = "dodge") + scale_fill_brewer(palette = "Dark2", direction = -1) + scale_y_continuous(labels = percent, breaks = seq(0, 0.6, 0.05)) + ylab("Percentage") + ggtitle("Survival Rate based on Pclass") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplot(filter(titanic, is.na(Survived) == FALSE), aes(Sex, fill = Survived)) + geom_bar(aes(y = (..count..)/sum(..count..)), alpha = 0.9) + facet_wrap(~Pclass) + scale_fill_brewer(palette = "Dark2", direction = -1) + scale_y_continuous(labels = percent, breaks = seq(0, 0.4, 0.05)) + ylab("Percentage") + ggtitle("Survival Rate based on Pclass and Sex") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(filter(titanic, is.na(Survived) == FALSE), aes(Pclass, Age)) + geom_violin(aes(fill = Survived), alpha = 0.9) + facet_wrap(~Survived) + scale_fill_brewer(palette = "Dark2", direction = -1) + ggtitle("Survival Rate based on Pclass and Age") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
mosaicplot(~Title + Survived, data = titanic, main = "Survival Rate based on Title", shade = TRUE)
ggplot(filter(titanic, is.na(Survived) == FALSE), aes(Title)) + geom_bar(aes(fill = Survived), alpha = 0.9, position = "fill") + facet_wrap(~Pclass) + scale_fill_brewer(palette = "Set1") + scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1)) + ylab("Percentage") + ggtitle("Survival Rate based on Pclass and Title") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
mosaicplot(~FamilySize + Survived, data = titanic, main = "Survival Rate based on FamilySize", shade = TRUE)
ggplot(filter(titanic, is.na(Survived) == FALSE), aes(Title)) + geom_bar(aes(fill = Survived), alpha = 0.9, position = "fill") + facet_wrap(~FamilySize) + scale_fill_brewer(palette = "Set1") + scale_y_continuous(labels = percent, breaks = seq(0, 1, 0.1)) + ylab("Percentage") + ggtitle("Survival Rate based on FamilySize and Title") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(filter(titanic, is.na(Survived) == FALSE), aes(Embarked, Fare)) + geom_boxplot(aes(fill = Survived), alpha = 0.9) + facet_wrap(~Survived) + scale_fill_manual(values = c("#56B4E9", "#CC79A7")) + ggtitle("Survival Rate based on Embarked and Fare") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
train_original <- titanic[1:891, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Title", "FamilySize")]
test_original <- titanic[892:1309, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked", "Title", "FamilySize")]
set.seed(789)
split = sample.split(train_original$Survived, SplitRatio = 0.8)
train = subset(train_original, split == TRUE)
test = subset(train_original, split == FALSE)
cor(train[, unlist(lapply(train, is.numeric))])
ps = chisq.test(train$Pclass, train$Sex)$p.value
pe = chisq.test(train$Pclass, train$Embarked)$p.value
pt = chisq.test(train$Pclass, train$Title)$p.value
pf = chisq.test(train$Pclass, train$FamilySize)$p.value
se = chisq.test(train$Sex, train$Embarked)$p.value
st = chisq.test(train$Sex, train$Title)$p.value
sf = chisq.test(train$Sex, train$FamilySize)$p.value
et = chisq.test(train$Embarked, train$Title)$p.value
ef = chisq.test(train$Embarked, train$FamilySize)$p.value
tf = chisq.test(train$Title, train$FamilySize)$p.value
cormatrix = matrix(c(0, ps, pe, pt, pf, ps, 0, se, st, sf, pe, se, 0, et, ef, pt, st, et, 0, tf, pf, sf, ef, tf, 0), 5, 5, byrow = TRUE)
row.names(cormatrix) = colnames(cormatrix) = c("Pclass", "Sex", "Embarked", "Title", "FamilySize")
cormatrix
classifier = glm(Survived ~ ., family = binomial(link = "logit"), data = train)
classifier <- step(classifier)
summary(classifier)
vif(classifier)
classifier = glm(Survived ~ . - Sex, family = binomial(link = "logit"), data = train)
classifier <- step(classifier)
summary(classifier)
vif(classifier)
durbinWatsonTest(classifier)
prob_pred = predict(classifier, type = "response", newdata = test)
y_pred = ifelse(prob_pred > 0.5, 1, 0)
table(test$Survived, y_pred > 0.5)
error <- mean(test$Survived != y_pred)
paste("Accuracy", round(1 - error, 4))
fitpred = prediction(prob_pred, test$Survived)
fitperf = performance(fitpred, "tpr", "fpr")
plot(fitperf, col = "green", lwd = 2, main = "ROC Curve")
abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")
paste("Age variance: ", var(train$Age), ", SibSp variance: ", var(train$SibSp), ", Parch variance: ", var(train$Parch), ", Fare variance: ", var(train$Fare))
standardized.train = cbind(select(train, Survived, Pclass, Sex, SibSp, Parch, Embarked, Title, FamilySize), Age = scale(train$Age), Fare = scale(train$Fare))
paste("Age variance: ", var(standardized.train$Age), ", Fare variance: ", var(standardized.train$Fare))
standardized.test = cbind(select(test, Survived, Pclass, Sex, SibSp, Parch, Embarked, Title, FamilySize), Age = scale(test$Age), Fare = scale(test$Fare))
paste("Age variance: ", var(standardized.test$Age), ", Fare variance: ", var(standardized.test$Fare))
classifier = svm(Survived ~ ., data = standardized.train, type = "C-classification", kernel = "linear")
y_pred = predict(classifier, newdata = standardized.test[, -which(names(standardized.test) == "Survived")])
table(test$Survived, y_pred)
error <- mean(test$Survived != y_pred)
paste("Accuracy", round(1 - error, 4))
classifier = svm(Survived ~ ., data = standardized.train, type = "C-classification", kernel = "radial")
y_pred = predict(classifier, newdata = standardized.test[, -which(names(standardized.test) == "Survived")])
table(test$Survived, y_pred)
error <- mean(test$Survived != y_pred)
paste("Accuracy", round(1 - error, 4))
tune.results <- tune(svm, Survived ~ ., data = standardized.train, kernel = "radial", ranges = list(cost = 2^(-2:2), gamma = 2^(-6:-2)))
summary(tune.results)
classifier = svm(Survived ~ ., data = standardized.train, type = "C-classification", kernel = "radial", cost = 4, gamma = 0.125)
y_pred = predict(classifier, newdata = standardized.test[, -which(names(standardized.test) == "Survived")])
table(test$Survived, y_pred)
error <- mean(test$Survived != y_pred)
paste("Accuracy", round(1 - error, 4))
classifier = rpart(Survived ~ ., data = train, method = "class")
rpart.plot(classifier, extra = 4)
y_pred = predict(classifier, newdata = test[, -which(names(test) == "Survived")], type = "class")
table(test$Survived, y_pred)
error <- mean(test$Survived != y_pred)
paste("Accuracy", round(1 - error, 4))
set.seed(789)
folds = createMultiFolds(train$Survived, k = 10, times = 5)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Survived ~ ., data = train, method = "rpart", trControl = control)
rpart.plot(classifier_cv$finalModel, extra = 4)
y_pred = predict(classifier_cv, newdata = test[, -which(names(test) == "Survived")])
table(test$Survived, y_pred)
error <- mean(test$Survived != y_pred)
paste("Accuracy", round(1 - error, 4))
set.seed(432)
classifier = randomForest(Survived ~ ., data = train)
plot(classifier)
y_pred = predict(classifier, newdata = test[, -which(names(test) == "Survived")])
table(test$Survived, y_pred)
error <- mean(test$Survived != y_pred)
paste("Accuracy", round(1 - error, 4))
set.seed(651)
folds = createMultiFolds(train$Survived, k = 10)
control <- trainControl(method = "repeatedcv", index = folds)
classifier_cv <- train(Survived ~ ., data = train, method = "rf", trControl = control)
y_pred = predict(classifier_cv, newdata = test[, -which(names(test) == "Survived")])
table(test$Survived, y_pred)
error <- mean(test$Survived != y_pred)
paste("Accuracy", round(1 - error, 4))
gini = as.data.frame(importance(classifier))
gini = data.frame(Feature = row.names(gini), MeanGini = round(gini[, "MeanDecreaseGini"], 2))
gini = gini[order(-gini[, "MeanGini"]), ]
ggplot(gini, aes(reorder(Feature, MeanGini), MeanGini, group = 1)) + geom_point(color = "red", shape = 17, size = 2) + geom_line(color = "blue", size = 1) + scale_y_continuous(breaks = seq(0, 60, 10)) + xlab("Feature") + ggtitle("Mean Gini Index of Features") + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
classifier = naiveBayes(Survived ~ ., data = train)
y_pred = predict(classifier, newdata = test[, -which(names(test) == "Survived")])
table(test$Survived, y_pred)
error <- mean(test$Survived != y_pred)
paste("Accuracy", round(1 - error, 4))
y_pred = predict(classifier, newdata = test_original)
results <- data.frame(PassengerID = titanic[892:1309, "PassengerId"], Survived = y_pred)
write.csv(results, file = "PredictingTitanicSurvival.csv", row.names = FALSE, quote = FALSE)
