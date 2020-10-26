library(ggplot2)
library(lattice)
library(caret)
library(ranger)
library(dplyr)
library(e1071)
train <- read.csv("../input/train.csv", stringsAsFactors = FALSE)
test <- read.csv("../input/test.csv", stringsAsFactors = FALSE)
str(train)
summary(train)
train$Survived <- factor(train$Survived)
train$Pclass <- factor(train$Pclass)
train$Sex <- factor(train$Sex)
train$SibSp <- factor(train$SibSp)
train$Parch <- factor(train$Parch)
train$Embarked <- factor(train$Embarked, ordered = FALSE)
head(train$Name)
convert_name <- function(name) {
    if (grepl("\\(.*\\)", name)) {
        gsub("^.*\\((.*)\\)$", "\\1", name)
    }
    else {
        gsub("^(.*),\\s[a-zA-Z\\.]*\\s(.*)$", "\\2 \\1", name)
    }
}
pass_names <- train$Name
clean_pass_names <- vapply(pass_names, FUN = convert_name, FUN.VALUE = character(1), USE.NAMES = FALSE)
train$Name <- clean_pass_names
head(train$Name)
train %>% ggplot(aes(x = Pclass, fill = Survived)) + geom_bar()
tab <- table(train$Pclass, train$Survived)
prop.table(tab, 1)
train %>% ggplot(aes(x = Sex, fill = Survived)) + geom_bar(stat = "count", position = "fill")
tab <- table(train$Sex, train$Survived)
prop.table(tab, 1)
train %>% ggplot(aes(x = Age, fill = Survived)) + geom_histogram()
train %>% filter(Embarked %in% c("S", "C", "Q")) %>% ggplot() + geom_bar(aes(Embarked, fill = Pclass), position = "dodge") + facet_grid(~Survived)
tab <- table(train$Embarked, train$Survived)
prop.table(tab, 1)
sum(is.na(train$Age))
x <- train[, c("Age", "Pclass", "Sex", "Embarked")]
y <- train$Survived
set.seed(123)
model <- train(x = x, y = y, preProcess = "medianImpute", method = "ranger", trControl = trainControl(method = "cv", number = 10))
model
prediction <- predict(model, test)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = "rfSolution.csv", row.names = F)
