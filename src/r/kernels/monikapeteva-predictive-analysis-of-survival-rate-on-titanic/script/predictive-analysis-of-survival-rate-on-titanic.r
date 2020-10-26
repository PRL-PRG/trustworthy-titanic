library("dplyr")
library("ggplot2")
library("ggthemes")
options(warn = -1)
train <- read.csv("../input/train.csv", stringsAsFactors = F)
test <- read.csv("../input/test.csv", stringsAsFactors = F)
test$Survived <- NA
full <- rbind(train, test)
head(full)
str(full)
age <- full$Age
n = length(age)
set.seed(123)
age_na_rows = which(is.na(age))
age[age_na_rows] = sample(na.omit(full$Age), length(age_na_rows))
par(mfrow = c(1, 2))
hist(full$Age, freq = F, main = "Before Replacement", col = "lightblue", ylim = c(0, 0.04), xlab = "age")
hist(age, freq = F, main = "After Replacement", col = "darkblue", ylim = c(0, 0.04))
cabin_counts = strsplit(full$Cabin, " ")
cabin = sapply(cabin_counts, length)
table(cabin)
full$PassengerId[is.na(full$Fare)]
full[1044, ]
ggplot(full[full$Pclass == "3" & full$Embarked == "S", ], aes(x = Fare)) + geom_density(fill = "#99d6ff", alpha = 0.4) + geom_vline(aes(xintercept = median(Fare, na.rm = T)), colour = "red", linetype = "dashed", lwd = 1)
full$Fare[1044] <- median(full[full$Pclass == "3" & full$Embarked == "S", ]$Fare, na.rm = TRUE)
embarked <- full$Embarked
embarked[which(embarked == "")] = "S"
table(embarked)
d <- data.frame(Age = age[1:891], Survived = train$Survived)
ggplot(d, aes(Age, fill = factor(Survived))) + geom_histogram()
cuts <- cut(d$Age, hist(d$Age, 10, plot = F)$breaks)
rate <- tapply(d$Survived, cuts, mean)
d2 <- data.frame(age = names(rate), rate)
barplot(d2$rate, xlab = "age", ylab = "survival rate")
ggplot(train, aes(Sex, fill = factor(Survived))) + geom_histogram(stat = "count")
tapply(train$Survived, train$Sex, mean)
get_titles = function() {
    names = as.character(full$Name)
    split_before_title = strsplit(names, ",")
    part_with_title = sapply(split_before_title, "[[", -1)
    split_after_title = strsplit(part_with_title, "[.]")
    part_with_title = sapply(split_after_title, "[[", 1)
    titles = sapply(part_with_title, trimws)
    return(titles)
}
title = get_titles()
d <- data.frame(title = title[1:891], Survived = train$Survived)
ggplot(d, aes(title, fill = factor(Survived))) + geom_histogram(stat = "count")
table(title)
tapply(d$Survived, d$title, mean)
title[title != "Mr" & title != "Miss" & title != "Mrs" & title != "Master"] <- "Rare"
table(title)
age <- full$Age
age_title = as.data.frame(list(Age = age, Title = title))
set.seed(123)
get_age = function(title_to_get) {
    ages = na.omit(age_title[which(age_title$Title == title_to_get), "Age"])
    cat(title_to_get, "age range:", min(ages), "-", max(ages), "\n")
    age_na_rows = which(is.na(age_title$Age) & age_title$Title == title_to_get)
    age[age_na_rows] = sample(ages, length(age_na_rows))
    return(age)
}
age = get_age("Master")
age = get_age("Miss")
age = get_age("Mr")
age = get_age("Mrs")
age = get_age("Rare")
par(mfrow = c(1, 2))
hist(full$Age, freq = F, main = "Before Replacement", col = "lightblue", ylim = c(0, 0.04), xlab = "age")
hist(age, freq = F, main = "After Replacement", col = "darkblue", ylim = c(0, 0.04))
d <- data.frame(Age = age[1:891], Survived = train$Survived)
ggplot(d, aes(Age, fill = factor(Survived))) + geom_histogram()
ggplot(train, aes(Pclass, fill = factor(Survived))) + geom_histogram(stat = "count")
tapply(train$Survived, train$Pclass, mean)
ggplot(train, aes(Parch, fill = factor(Survived))) + geom_histogram(stat = "count")
ggplot(train, aes(SibSp, fill = factor(Survived))) + geom_histogram(stat = "count")
family <- full$SibSp + full$Parch
d <- data.frame(family = family[1:891], Survived = train$Survived)
ggplot(d, aes(family, fill = factor(Survived))) + geom_histogram(stat = "count")
tapply(d$Survived, d$family, mean)
d <- data.frame(Cabin = cabin[1:891], Survived = train$Survived)
ggplot(d, aes(Cabin, fill = factor(Survived))) + geom_histogram(stat = "count")
tapply(d$Survived, d$Cabin, mean)
ggplot(train, aes(Fare, fill = factor(Survived))) + geom_histogram()
cuts <- cut(train$Fare, hist(train$Fare, 10, plot = F)$breaks)
rate <- tapply(train$Survived, cuts, mean)
d <- data.frame(fare = names(rate), rate)
barplot(d$rate, xlab = "fare", ylab = "survival rate")
d <- data.frame(Embarked = embarked[1:891], Survived = train$Survived)
ggplot(d, aes(Embarked, fill = factor(Survived))) + geom_histogram(stat = "count")
tapply(train$Survived, train$Embarked, mean)
f.survived = train$Survived
f.age = age[1:891]
t.age = age[892:1309]
f.fare = full$Fare[1:891]
t.fare = full$Fare[892:1309]
f.cabin = cabin[1:891]
t.cabin = cabin[892:1309]
f.title = title[1:891]
t.title = title[892:1309]
family <- full$SibSp + full$Parch
f.family = family[1:891]
t.family = family[892:1309]
f.pclass = train$Pclass
t.pclass = test$Pclass
f.sex = train$Sex
t.sex = test$Sex
f.embarked = embarked[1:891]
t.embarked = embarked[892:1309]
new_train = data.frame(survived = f.survived, age = f.age, fare = f.fare, sex = f.sex, embarked = f.embarked, family = f.family, title = f.title, cabin = f.cabin, pclass = f.pclass)
fit_logit <- glm(factor(survived) ~ age + fare + sex + embarked + family + title + cabin + pclass, data = new_train, family = binomial)
ans_logit = rep(NA, 891)
for (i in 1:891) {
    ans_logit[i] = round(fit_logit$fitted.values[[i]], 0)
}
mean(ans_logit == train$Survived)
table(ans_logit)
library("randomForest")
set.seed(123)
fit_rf <- randomForest(factor(survived) ~ age + fare + sex + embarked + family + title + cabin + pclass, data = new_train)
rf.fitted = predict(fit_rf)
ans_rf = rep(NA, 891)
for (i in 1:891) {
    ans_rf[i] = as.integer(rf.fitted[[i]]) - 1
}
mean(ans_rf == train$Survived)
table(ans_rf)
library(rpart)
fit_dt <- rpart(factor(survived) ~ age + fare + sex + embarked + family + title + cabin + pclass, data = new_train)
dt.fitted = predict(fit_dt)
ans_dt = rep(NA, 891)
for (i in 1:891) {
    if (dt.fitted[i, 1] >= dt.fitted[i, 2]) {
        ans_dt[i] = 0
    }
    else {
        ans_dt[i] = 1
    }
}
mean(ans_dt == train$Survived)
table(ans_dt)
library(e1071)
fit_svm <- svm(factor(survived) ~ age + fare + sex + embarked + family + title + cabin + pclass, data = new_train)
svm.fitted = predict(fit_svm)
ans_svm = rep(NA, 891)
for (i in 1:891) {
    ans_svm[i] = as.integer(svm.fitted[[i]]) - 1
}
mean(ans_svm == train$Survived)
table(ans_svm)
a = sum(ans_logit == 1 & f.survived == 1)
b = sum(ans_logit == 1 & f.survived == 0)
c = sum(ans_logit == 0 & f.survived == 1)
d = sum(ans_logit == 0 & f.survived == 0)
data.frame(a, b, c, d)
a = sum(ans_rf == 1 & f.survived == 1)
b = sum(ans_rf == 1 & f.survived == 0)
c = sum(ans_rf == 0 & f.survived == 1)
d = sum(ans_rf == 0 & f.survived == 0)
data.frame(a, b, c, d)
a = sum(ans_dt == 1 & f.survived == 1)
b = sum(ans_dt == 1 & f.survived == 0)
c = sum(ans_dt == 0 & f.survived == 1)
d = sum(ans_dt == 0 & f.survived == 0)
data.frame(a, b, c, d)
a = sum(ans_svm == 1 & f.survived == 1)
b = sum(ans_svm == 1 & f.survived == 0)
c = sum(ans_svm == 0 & f.survived == 1)
d = sum(ans_svm == 0 & f.survived == 0)
data.frame(a, b, c, d)
test_data_set <- data.frame(age = t.age, fare = t.fare, sex = t.sex, embarked = t.embarked, family = t.family, title = t.title, cabin = t.cabin, pclass = t.pclass)
predict_svm = predict(fit_svm, newdata = test_data_set)
predict_svm = as.integer(predict_svm) - 1
table(predict_svm)
predict_logit = predict(fit_logit, newdata = test_data_set)
predict_logit = as.numeric(predict_logit > 0)
table(predict_logit)
predict_rf = predict(fit_rf, newdata = test_data_set)
predict_rf = as.integer(predict_rf) - 1
table(predict_rf)
predict_dt = predict(fit_dt, newdata = test_data_set)
predict_dt = ifelse(predict_dt[, 1] >= predict_dt[, 2], 0, 1)
table(predict_dt)
d <- data.frame(PassengerId = test$PassengerId, Survived = predict_svm)
write.csv(d, file = "TitanicResultSvm.csv", row.names = F)
d <- data.frame(PassengerId = test$PassengerId, Survived = predict_logit)
write.csv(d, file = "TitanicResultLogit.csv", row.names = F)
d <- data.frame(PassengerId = test$PassengerId, Survived = predict_rf)
write.csv(d, file = "TitanicResultRF.csv", row.names = F)
d <- data.frame(PassengerId = test$PassengerId, Survived = predict_dt)
write.csv(d, file = "TitanicResultDT.csv", row.names = F)
