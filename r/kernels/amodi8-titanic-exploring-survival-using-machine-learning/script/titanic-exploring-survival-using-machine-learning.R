library(ggplot2)
library(readr)
system("ls ../input")
train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")
str(train)
table(train$Survived)
prop.table(table(train$Survived))
test$Survived <- rep(0, 418)
submit <- data.frame(Passengerid = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)
table(train$Sex)
round(prop.table(table(train$Sex, train$Survived), 1) * 100, 2)
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
submit <- data.frame(Passengerid = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "maleperish.csv", row.names = FALSE)
summary(train$Age)
hist(train$Age, col = c("red", "steelblue"), freq = F, main = "Distribution of Age", xlab = "Age")
rug(jitter(train$Age), col = "darkgrey")
lines(density(train$Age, na.rm = T), col = "yellow", lwd = 3)
box()
train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)
aggregate(Survived ~ Child + Sex, data = train, FUN = length)
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {
    sum(x)/length(x)
})
train$Fare2 <- "30+"
train$Fare2[train$Fare >= 10 & train$Fare < 20] = "10-20"
train$Fare2[train$Fare >= 20 & train$Fare < 30] = "20-30"
train$Fare2[train$Fare < 10] = "<10"
aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) {
    sum(x)/length(x)
})
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Sex == "female" & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(Passengerid = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "higherclassfemalesurvive.csv", row.names = FALSE)
