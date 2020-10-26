library(ggplot2)
library(readr)
library(data.table)
system("ls ../input")
tdata <- fread("../input/train.csv")
table <- tdata[, c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked"), with = F]
table[, `:=`(Survived = as.factor(Survived), Sex = as.factor(Sex), Embarked = as.factor(Embarked))]
m1 <- glm(Survived ~ Pclass + Sex + Age + I(Age^2) + Fare + Embarked, data = table, family = "binomial", na.action = na.omit)
m1$xlevels$Embarked <- c("C", "Q", "S", "")
test <- fread("../input/test.csv")
test[, `:=`(Sex = as.factor(Sex), Embarked = as.factor(Embarked))]
p1 <- as.vector(predict.lm(m1, newdata = test, type = "response"))
p1 <- ifelse(is.na(p1) || p1 < 0.5, 0, 1)
test[, `:=`(Survived, p1)]
write.csv(test[, c("PassengerId", "Survived"), with = F], "result.csv", row.names = F, quote = F)
