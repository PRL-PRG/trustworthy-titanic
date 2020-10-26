library(ggplot2)
library(readr)
system("ls ../input")
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
sapply(train, function(x) sum(is.na(x)))
sapply(train, function(x) summary(is.na(x)))
train1 <- train
train$Survived <- NULL
combo <- rbind(train, test)
library(ggplot2)
ggplot(data = combo, aes(x = factor(Pclass), y = Age, fill = factor(Pclass))) + geom_bar(stat = "identity", position = position_dodge())
ggplot(combo, aes(Age, fill = factor(Pclass))) + geom_bar(binwidth = 1, position = position_dodge())
mean_class <- function(class) {
    classvec <- subset(combo, Pclass == class)
    mean_age <- mean(classvec$Age, na.rm = TRUE)
    return(mean_age)
}
l_age <- length(combo$Age)
library(dplyr)
i <- 1
for (i in 1:l_age) {
    if (is.na(combo$Age[i]) == TRUE) {
        class_value <- combo$Pclass[i]
        combo$Age[i] <- mean_class(class_value)
    }
}
ggplot(combo, aes(Fare, fill = factor(Pclass))) + geom_bar(stat = "identity", position = position_dodge(), binwidth = 5)
fare_na_index <- which(is.na(combo$Fare))
df <- subset(combo, Pclass = 3)
vec <- df$Fare
mean(vec, na.rm = TRUE)
combo$Fare[fare_na_index] <- mean(vec, na.rm = TRUE)
cabin_no <- function(string) {
    return(length(strsplit(string, " ")[[1]]))
}
l_cabin <- length(combo$Cabin)
combo$Cabin <- as.character(combo$Cabin)
l <- 0
for (l in 1:l_cabin) {
    cabin_string <- combo$Cabin[l]
    if (cabin_string == " ") {
        combo$Cabin[l] <- 0
    }
    else {
        cabin_count <- cabin_no(cabin_string)
        combo$Cabin[l] <- cabin_count
    }
}
combo$Cabin <- as.numeric(combo$Cabin)
combo$Sex <- factor(x = combo$Sex, labels = c(1, 2))
combo$Embarked <- replace(combo$Embarked, combo$Embarked == "", "S")
combo$Embarked <- factor(x = combo$Embarked, labels = c(1, 2, 3))
names(combo)
combo$family <- combo$SibSp + combo$Parch
combo$SibSp <- NULL
combo$Parch <- NULL
combo$Name <- NULL
combo$Ticket <- NULL
names(combo)
nrow(train)
nrow(test)
train <- combo[1:891, ]
test <- combo[892:1309, ]
train$Survived <- train1$Survived
names(train)
classifier <- glm(formula = Survived ~ Pclass + Sex + Age + Fare + Cabin + Embarked + family, family = binomial, data = train)
summary(classifier)
classifier <- glm(formula = Survived ~ Pclass + Sex + Age + family, family = binomial, data = train)
summary(classifier)
sur_pred <- predict(classifier, type = "response", newdata = test)
y_pred <- ifelse(sur_pred > 0.5, 1, 0)
test$Survival <- y_pred
submit <- data.frame(test$PassengerId, test$Survival)
names(submit) <- c("PassengerId", "Survived")
write.csv(submit, file = "survival.csv", row.names = FALSE)
