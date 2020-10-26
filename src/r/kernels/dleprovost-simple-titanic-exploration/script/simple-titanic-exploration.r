quiet <- function(x) {
    suppressWarnings(suppressMessages(x))
}
quiet(library(plyr))
quiet(library(dplyr))
quiet(library(ggplot2))
quiet(library(Rmisc))
quiet(library(corrplot))
quiet(library(rpart))
quiet(library(rpart.plot))
quiet(library(randomForest))
quiet(library(e1071))
quiet(library(caret))
titanic_train <- read.csv(file = "../input/train.csv", head = TRUE)
titanic_test <- read.csv(file = "../input/test.csv", head = TRUE)
suppressWarnings(titanic <- bind_rows(titanic_train, titanic_test))
str(titanic)
summary(titanic)
mSurv <- mean(titanic_train$Survived)
titanic_train <- within(titanic_train, {
    Survived[Survived == 1] <- "Yes"
    Survived[Survived == 0] <- "No"
})
add_mSurv <- function(x) {
    return(x + geom_hline(aes(yintercept = mSurv, linetype = factor(mSurv)), show.legend = TRUE) + scale_linetype_manual(name = "Survival", values = "dashed", label = "average") + guides(fill = guide_legend(override.aes = list(linetype = "blank"))))
}
g1 <- ggplot(titanic_train) + geom_bar(aes(Pclass, fill = Survived)) + labs(x = "Passenger's class", y = "Count")
g2 <- add_mSurv(ggplot(titanic_train) + geom_bar(aes(Pclass, fill = Survived), position = "fill") + labs(x = "Passenger's class", y = "Part"))
multiplot(g1, g2, cols = 2)
g1 <- ggplot(titanic_train) + geom_bar(aes(Sex, fill = Survived))
g2 <- add_mSurv(ggplot(titanic_train) + geom_bar(aes(Sex, fill = Survived), position = "fill"))
multiplot(g1, g2, cols = 2)
g1 <- ggplot(titanic_train) + geom_histogram(aes(Age, fill = Survived), binwidth = 5, na.rm = TRUE)
g2 <- add_mSurv(ggplot(titanic_train) + geom_histogram(aes(Age, fill = Survived), binwidth = 5, na.rm = TRUE, position = "fill"))
multiplot(g1, g2, cols = 2)
g1 <- ggplot(titanic_train) + geom_histogram(aes(Fare, fill = Survived), binwidth = 20, na.rm = TRUE)
g2 <- add_mSurv(ggplot(titanic_train) + geom_histogram(aes(Fare, fill = Survived), binwidth = 20, na.rm = TRUE, position = "fill"))
multiplot(g1, g2, cols = 2)
g1 <- ggplot(titanic_train) + geom_bar(aes(Embarked, fill = Survived))
g2 <- add_mSurv(ggplot(titanic_train) + geom_bar(aes(Embarked, fill = Survived), position = "fill"))
multiplot(g1, g2, cols = 2)
ggplot(titanic_train) + geom_bar(aes(Pclass, fill = Survived)) + facet_wrap(~Embarked)
g1 <- ggplot(titanic_train) + geom_bar(aes(Parch, fill = Survived))
g2 <- add_mSurv(ggplot(titanic_train) + geom_bar(aes(Parch, fill = Survived), position = "fill"))
multiplot(g1, g2, cols = 2)
ggplot(filter(titanic_train, Parch > 0)) + geom_histogram(aes(Age, fill = Survived), binwidth = 5, na.rm = TRUE) + facet_grid(Parch ~ Sex)
g1 <- ggplot(titanic_train) + geom_bar(aes(SibSp, fill = Survived))
g2 <- add_mSurv(ggplot(titanic_train) + geom_bar(aes(SibSp, fill = Survived), position = "fill"))
multiplot(g1, g2, cols = 2)
ggplot(filter(titanic_train, SibSp > 0)) + geom_histogram(aes(Age, fill = Survived), binwidth = 5, na.rm = TRUE) + facet_grid(SibSp ~ Sex)
titanic_train$FamilyOnBoard <- titanic_train$Parch + titanic_train$SibSp
g1 <- ggplot(titanic_train) + geom_bar(aes(FamilyOnBoard, fill = Survived))
g2 <- add_mSurv(ggplot(titanic_train) + geom_bar(aes(FamilyOnBoard, fill = Survived), position = "fill"))
multiplot(g1, g2, cols = 2)
titanic_train$Family[(titanic_train$Parch + titanic_train$SibSp) == 0] <- "single"
titanic_train$Family[(titanic_train$Parch + titanic_train$SibSp) > 0 & (titanic_train$Parch + titanic_train$SibSp) < 4] <- "small"
titanic_train$Family[(titanic_train$Parch + titanic_train$SibSp) > 3] <- "large"
titanic_train$Family = as.factor(titanic_train$Family)
titanic_train$Family = factor(titanic_train$Family, levels = c("single", "small", "large"))
g1 <- ggplot(titanic_train) + geom_bar(aes(Family, fill = Survived))
g2 <- add_mSurv(ggplot(titanic_train) + geom_bar(aes(Family, fill = Survived), position = "fill"))
multiplot(g1, g2, cols = 2)
filter(titanic, Ticket == "PC 17599")$Fare
filter(titanic, Ticket == "19950")$Fare
filter(titanic, Ticket == "3101295")$Fare
cor(filter(titanic, !is.na(Fare))[, c("Pclass", "Fare")])
pplOnTicket <- setNames(aggregate(titanic$Ticket, list(titanic$Ticket), length), c("Ticket", "PplOnTicket"))
titanic <- left_join(titanic, pplOnTicket, by = "Ticket")
titanic["FarePerson"] <- titanic$Fare/titanic$PplOnTicket
cor(filter(titanic, !is.na(FarePerson))[, c("Pclass", "FarePerson")])
titanic$Family[(titanic$Parch + titanic$SibSp) == 0] <- "single"
titanic$Family[(titanic$Parch + titanic$SibSp) > 0 & (titanic$Parch + titanic$SibSp) < 4] <- "small"
titanic$Family[(titanic$Parch + titanic$SibSp) > 3] <- "large"
titanic$Family = factor(titanic$Family, levels = c("single", "small", "large"))
sapply(titanic, function(x) sum(is.na(x) | x == ""))
filter(titanic, is.na(Fare))
median(filter(titanic, Family == "single", Pclass == 3, Embarked == "S")$Fare, na.rm = TRUE)
titanic$Fare[1044] <- median(filter(titanic, Family == "single", Pclass == 3, Embarked == "S")$Fare, na.rm = TRUE)
titanic$FarePerson[1044] <- titanic$Fare[1044]
filter(titanic, Embarked == "")
ggplot(filter(titanic, Embarked != "")) + geom_boxplot(aes(Embarked, FarePerson, color = Embarked), na.rm = TRUE) + geom_hline(yintercept = 40, colour = "darkred")
titanic$Embarked[titanic$Embarked == ""] <- "C"
predicted_ages <- rpart(Age ~ Pclass + Sex + Family + FarePerson + Embarked, data = filter(titanic, !is.na(Age)))
titanic$Age[is.na(titanic$Age)] <- predict(predicted_ages, filter(titanic, is.na(Age)))
corr <- select(titanic, -Ticket, -Fare, -Cabin, -Name, -Family)
corr$Sex <- as.numeric(revalue(corr$Sex, c(male = 1, female = 2)))
corr$Embarked <- as.numeric(revalue(corr$Embarked, c(S = 1, C = 2, Q = 3)))
corr$Survived <- as.numeric(corr$Survived)
corr$Pclass <- as.numeric(corr$Pclass)
corr$PplOnTicket <- as.numeric(corr$PplOnTicket)
corr$FamilySize <- as.numeric(corr$SibSp + corr$Parch + 1)
corr <- select(corr, -SibSp, -Parch)
corrplot(cor(corr), method = "circle")
corrplot(cor(filter(subset(corr, select = -PassengerId), !is.na(Survived))), method = "circle")
titanic <- select(titanic, -SibSp, -Parch, -Ticket, -Fare, -Cabin, -Name)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Age <- as.numeric(titanic$Age)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$PplOnTicket <- as.integer(titanic$PplOnTicket)
titanic$FarePerson <- as.numeric(titanic$FarePerson)
titanic$Family <- as.factor(titanic$Family)
titanic_train <- filter(titanic, !is.na(Survived))
titanic_test <- filter(titanic, is.na(Survived))
titanic_formula <- (Survived ~ Pclass + Sex + Age + Embarked + PplOnTicket + FarePerson + Family)
spec <- c(train = 0.7, test = 0.3)
g <- sample(cut(seq(nrow(titanic_train)), nrow(titanic_train) * cumsum(c(0, spec)), labels = names(spec)))
my_titanic <- split(titanic_train, g)
my_titanic_tree <- rpart(titanic_formula, data = my_titanic$train, control = rpart.control(minsplit = 5, cp = 0))
plotcp(my_titanic_tree)
my_titanic_tree_optimal <- prune(my_titanic_tree, cp = my_titanic_tree$cptable[which.min(my_titanic_tree$cptable[, 4]), 1])
prp(my_titanic_tree_optimal, extra = 1)
isCorrect <- predict(my_titanic_tree_optimal, newdata = my_titanic$test, type = "class") == my_titanic$test$Survived
dt_score <- sum(isCorrect)/length(isCorrect)
dt_score
titanic_tree <- rpart(titanic_formula, data = titanic_train, control = rpart.control(minsplit = 5, cp = 0))
plotcp(titanic_tree)
titanic_tree_optimal <- prune(titanic_tree, cp = titanic_tree$cptable[which.min(titanic_tree$cptable[, 4]), 1])
prp(titanic_tree_optimal, extra = 1)
set.seed(19150415)
my_titanic_rf <- randomForest(titanic_formula, data = my_titanic$train)
plot(my_titanic_rf, ylim = c(0, 0.4))
legend("topright", colnames(my_titanic_rf$err.rate), col = 1:3, fill = 1:3)
my_titanic_rf_importance <- importance(my_titanic_rf)
vars_importance <- data.frame(Variables = row.names(my_titanic_rf_importance), importance = round(my_titanic_rf_importance[, "MeanDecreaseGini"], 2))
ggplot(vars_importance, aes(fill = importance)) + geom_bar(aes(reorder(Variables, importance), importance), stat = "identity") + coord_flip()
isCorrect <- predict(my_titanic_rf, newdata = my_titanic$test, type = "class") == my_titanic$test$Survived
rf_score <- sum(isCorrect)/length(isCorrect)
rf_score
set.seed(19150415)
titanic_rf <- randomForest(titanic_formula, data = titanic_train)
plot(titanic_rf)
titanic_rf_importance <- importance(titanic_rf)
vars_importance <- data.frame(Variables = row.names(titanic_rf_importance), importance = round(titanic_rf_importance[, "MeanDecreaseGini"], 2))
ggplot(vars_importance, aes(fill = importance)) + geom_bar(aes(reorder(Variables, importance), importance), stat = "identity") + coord_flip()
svm_model <- svm(titanic_formula, data = my_titanic$train)
isCorrect <- predict(svm_model, newdata = my_titanic$test, type = "class") == my_titanic$test$Survived
svm_score <- sum(isCorrect)/length(isCorrect)
svm_score
data.frame(Method = c("Decision Tree", "Random Forest", "Support Vector Machine"), Score = c(dt_score, rf_score, svm_score))
score <- function(model, vData) {
    isCorrect <- predict(model, newdata = vData, type = "class") == vData$Survived
    score <- sum(isCorrect)/length(isCorrect)
    return(score)
}
getScores <- function(formula, tData, vData) {
    tree <- rpart(formula, data = tData, control = rpart.control(minsplit = 5, cp = 0))
    tree_model <- prune(tree, cp = tree$cptable[which.min(tree$cptable[, 4]), 1])
    tree_score <- score(tree_model, vData)
    rf_model <- randomForest(formula, data = tData)
    rf_score <- score(rf_model, vData)
    svm_model <- svm(formula, data = tData)
    svm_score <- score(svm_model, vData)
    return(data.frame(DecisionTree = tree_score, RandomForest = rf_score, SVM = svm_score))
}
validation <- data.frame()
for (n in seq(2, 10, by = 2)) {
    folds <- split(titanic_train, cut(sample(1:nrow(titanic_train)), 10))
    x <- data.frame()
    for (i in 1:length(folds)) {
        test <- ldply(folds[i], data.frame)
        train <- ldply(folds[-i], data.frame)
        x <- rbind(x, getScores(titanic_formula, train, test))
    }
    validation <- rbind(validation, data.frame(DecisionTree = mean(x$DecisionTree), RandomForest = mean(x$RandomForest), SVM = mean(x$SVM)))
}
sa <- stack(as.data.frame(validation))
sa$x <- rep(seq(2, 2 * nrow(validation), by = 2), ncol(validation))
qplot(x, values, data = sa, colour = ind, geom = "line", xlab = "cross validation folds number", ylab = "score") + labs(colour = "Methods") + theme(legend.position = "bottom")
prediction <- predict(titanic_rf, titanic_test)
summary(prediction)/length(prediction)
solution <- data.frame(PassengerID = titanic_test$PassengerId, Survived = prediction)
write.csv(solution, file = "solution.csv", row.names = FALSE)
